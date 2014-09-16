-- The StandAloneDeriving compiler directive is necessary to create
-- an orphan instance of Ordfor SDL.Keysym
{-# LANGUAGE StandaloneDeriving #-}

-- The Arrows compiler directive is necessary to use arrow proc notation
{-# LANGUAGE Arrows #-}

module Main
  ( main
  ) where

import Prelude hiding ((.), id, null, filter)
import qualified Control.Monad as M
import Control.Arrow (returnA)
import Control.Monad.Fix (MonadFix)
import Control.Wire hiding (empty)
import FRP.Netwire hiding (empty)
import Data.Functor.Identity (runIdentity)
import Data.Monoid (Monoid)
import Data.Set (Set, empty, insert, delete, null, filter)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Keyboard as SDLK

-- a vector for use with 2D coordinates and velocity
data V2 a = V2 { _x :: !a
               , _y :: !a }

type Point = V2 Double
type Velocity = V2 Double

-- Utility Wire for use with most of the generated wires
type SimWire = Wire (Timed NominalDiffTime ()) () Identity

-- a Session with an IO monad transformer
type GameSession = Session IO (Timed NominalDiffTime ())

-- Player Wire that takes a Set of keys and outputs a Player
type PlayerWire = SimWire (Set SDL.Keysym) Player

-- Player
data Player = Player { _position1 :: Point
                     , _position2 :: Point
                     , _width     :: Double
                     , _height    :: Double }

-- Ball Wire that takes nothing and outputs a Ball
type BallWire = SimWire () Ball

-- Ball
data Ball = Ball { _position   :: Point
                 , _radius   :: Double
                 , _velocity :: Velocity }

-- Ball Collisions
data BallCollision = BounceUp
                   | BounceDown
                   | BounceLeft
                   | BounceRight
                   | BounceNone

-- Game Wire that takes a Set of keys and outputs a Game
type GameWire = SimWire (Set SDL.Keysym) Game

-- Game
data Game = Game { _player :: Player
                 , _ball   :: Ball }

-- Game State that contains the current wires
data GameState = GameState { _playerWire :: PlayerWire
                           , _ballWire   :: BallWire }

-- generate a GameWire from a GameState
gameWire :: GameState -> GameWire
gameWire (GameState player ball) = proc keysDown -> do
  player' <- player -< keysDown
  ball' <- ball -< ()
  returnA -< Game player' ball'

initialState :: GameState
initialState = makeGameState initialFrame

initialFrame :: Game
initialFrame = Game player ball

  where [w, h, r] = fmap (* gameSize) [paddleWidth, paddleHeight, ballSize]
        player = Player
                 (V2 10  halfWinHeight)
                 (V2 620 halfWinHeight)
                 w
                 h

        ball = Ball
               (V2 halfWinWidth halfWinHeight)
               r
               (V2 0 80)

makeGameState :: Game -> GameState
makeGameState (Game pl ba) = GameState (playerWire pl) (ballWire ba)

----------------------------------------------------------------------

-- generate a BallWire from a Ball
ballWire :: Ball -> BallWire
ballWire (Ball pos r vel) = proc _ -> do
  rec velocity <- ballVelocity vel -< position
      position <- ballPosition pos -< velocity
  returnA -< Ball position r velocity

-- ballWire :: Ball -> BallWire
-- ballWire (Ball (V2 p0 py) r (V2 v0 vy)) = proc _ -> do
--   rec (_, v) <- integralWith' (\(dv, v) -> (dv, v + dv)) v0 -< (dv, 10)
--       (dv, p) <- integralWith' collisionResponse p0 -< (0, v)
--   returnA -< Ball (V2 p0 py) r (V2 v0 vy)
  
-- -- now collisionResponse can communicate 'dv' to the velocity as
-- -- part of collision response
-- collisionResponse :: (Double, Double) -> (Double, Double)
-- collisionResponse (w, a) = undefined

-- integralWith' :: (Fractional a)
--                  => ((w,a) -> (w,a))
--                  -> a
--                  -> SimWire (w, a) (w, a)
-- integralWith' correct = loop
--     where loop (w, x') =
--             mkPure $ \ds dx ->
--             let dt = realToFrac (dtime ds)
--                 (b, x)  = correct (w, x' + dt*dx)
--             in x' `seq` (Right (w, x'), loop (b, x))

-- update Ball position
ballPosition :: Point -> SimWire Velocity Point
ballPosition (V2 x y) = proc (V2 vx vy) -> do
  -- x' <- integralWith collResX x -< (vx, ballSize * gameSize)
  -- y' <- integralWith collResY y -< (vy, ballSize * gameSize)
  x' <- integral x -< vx
  y' <- integral y -< vy
  returnA -< (V2 x' y')

collResX :: Double -> Double -> Double
collResX = collRes boundX

collResY :: Double -> Double -> Double
collResY = collRes boundY

collRes :: Point -> Double -> Double -> Double
collRes (V2 low high) pad = max (low + 1) . min ((high - 1) - pad)

-- update Ball velocity
ballVelocity :: Velocity -> SimWire Point Velocity
ballVelocity (V2 vx vy) = proc (V2 x y) -> do
  vx' <- ballCollision (V2 0 (fromIntegral winWidth - ballSize * gameSize)) -< (x, vx)
  vy' <- ballCollision (V2 0 (fromIntegral winHeight - ballSize * gameSize)) -< (y, vy)
  returnA -< V2 vx' vy'

ballCollision :: Point -> SimWire (Double, Double) Double
ballCollision (V2 b1 b2) = proc (a, v) -> do
  returnA -< if a <= b1 then 500
             else if a >= b2 then -500
                  else v

----------------------------------------------------------------------

-- generate a PlayerWire from a Player
playerWire :: Player -> PlayerWire
playerWire (Player p1 p2 w h) = proc keysDown -> do
  v1 <- movePlayer1 -< keysDown
  v2 <- movePlayer2 -< keysDown
  p1' <- playerPosition p1 -< v1
  p2' <- playerPosition p2 -< v2
  returnA -< Player p1' p2' w h

-- update Player position
playerPosition :: Point -> SimWire Double Point
playerPosition (V2 x y) = proc velocity -> do
  y' <- integralWith clampY y -<
               (velocity, paddleHeight * gameSize)
  returnA -< V2 x y'

-- update Player 1 velocity
movePlayer1 :: SimWire (Set SDL.Keysym) Double
movePlayer1 = pure (-250) . when (keyDown SDL.W)
              <|> pure (250) . when (keyDown SDL.S)
              <|> pure 0

-- update Player 2 velocity
movePlayer2 :: SimWire (Set SDL.Keysym) Double
movePlayer2 = pure (-250) . when (keyDown SDL.Up)
              <|> pure (250) . when (keyDown SDL.Down)
              <|> pure 0
                  
----------------------------------------------------------------------
clampX :: Double -> Double -> Double
clampX = clamp_ boundX

clampY :: Double -> Double -> Double
clampY = clamp_ boundY

clamp_ :: Point -> Double -> Double -> Double
clamp_ (V2 low high) pad = max low . min (high - pad)

-- constants
gameSize  = 300 :: Double
winWidth  = 640 :: Int
winHeight = 480 :: Int

halfWinHeight = fromIntegral $ winHeight `div` 2
halfWinWidth = fromIntegral $ winWidth `div` 2

padding      = 0.01 :: Double
paddleWidth  = 0.03 :: Double
paddleHeight = 0.2  :: Double
ballSize     = 0.04 :: Double
boundX = V2 0.0 (fromIntegral winWidth)  :: Point
boundY = V2 0.0 (fromIntegral winHeight) :: Point

----------------------------------------------------------------------

main :: IO ()
main =
  let title     = "Eric's Pong"
      position  = SDL.Position 0 0
      size      = SDL.Size winWidth winHeight
      win_flags = [SDL.WindowOpengl]
  in SDL.withWindow title position size win_flags $ \window ->

    -- Setup a rendering context.
    let device    = SDL.FirstSupported
        ren_flags = [SDL.Accelerated]
    in SDL.withRenderer window device ren_flags $ \renderer ->
       render renderer empty clockSession_ (gameWire initialState)

----------------------------------------------------------------------

-- primary rendering action
render :: SDL.Renderer -> Set SDL.Keysym -> GameSession -> GameWire -> IO ()
render screen keysDown s w = do

  -- capture any new events
  keysDown' <- parseEvents keysDown
  
  -- step the session
  -- > (resultOfSession, newSession) <- stepSession session
  (ds, s') <- stepSession s

  -- step the wire
  -- > (resultOfWire, newWire) <- stepWire wire resultOfSession input
  -- input must be of type Either.
  --
  -- Use `runIdentity` to pull result out of the Identity monad
  let (ex, w') = runIdentity $ stepWire w ds (Right keysDown')

      -- unwrap ex from Right constructor
  let x = either (const initialFrame) id ex
      player = _player x
      player1 = _position1 player
      player2 = _position2 player
      ball = _ball x

  -- Set the drawing color to black
  SDL.setRenderDrawColor screen 10 10 10 255

  -- Clear the buffer, using the color set above.
  SDL.renderClear screen
  
  -- Set the drawing color to light blue.
  SDL.setRenderDrawColor screen 101 208 246 255 -- 0 153 204 255
  
  -- Create player 1
  SDL.renderFillRect screen $ renderPlayer player1 player
  
  -- Create player 2
  SDL.renderFillRect screen $ renderPlayer player2 player
  
  -- Create the ball
  SDL.renderFillRect screen $ renderBall ball
  
  -- Swap our buffer for the present screen buffer, displaying it.
  SDL.renderPresent screen

  -- run at 16 fps
  SDL.delay (1000 `div` 60)

  -- Quit if Escape is pressed, otherwise continue
  M.unless (keyDown SDL.Escape keysDown') $ do
    render screen keysDown' s' w'

renderBall :: Ball -> SDL.Rect
renderBall (Ball (V2 x y) r _)= SDL.Rect (round x) (round y) w h
  where w = round r
        h = w

renderPlayer :: Point -> Player -> SDL.Rect
renderPlayer (V2 x y) (Player _ _ w h) =
  SDL.Rect x' y' w' h'
  where w' = round w
        h' = round h
        x' = round x
        y' = round (y - (h/2))

----------------------------------------------------------------------
    
-- Use pollEvent to return the topmost event from the queue. If there
-- are no events in the queue, pollEvenet will return Nothing.
parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
  maybe_event <- SDL.pollEvent
  case maybe_event of

    -- Nothing happened
    Nothing -> return keysDown

    -- Something happened
    (Just event) ->
      case SDL.eventData event of
        
        -- add a key to the set
        SDL.Keyboard SDL.KeyDown _ _ k -> parseEvents (insert k keysDown)

        -- remove a key from the set
        SDL.Keyboard SDL.KeyUp _ _ k -> parseEvents (delete k keysDown)

        -- add Escape to the set on a Quit event to quit the game
        SDL.Quit -> do
          keyQuit <- SDLK.getKeyFromScancode SDLK.ScancodeEscape
          return (insert (SDL.Keysym SDL.Escape keyQuit 0) keysDown)

        -- otherwise, continue
        _ -> parseEvents keysDown

keyDown :: SDL.Scancode -> Set SDL.Keysym -> Bool
keyDown k = not . null . filter ((== k) . SDL.keyScancode)

-- must make SDL.Keysym an instance of Ord to use it in a Set
deriving instance Ord SDL.Keysym

----------------------------------------------------------------------

challenge1 :: (HasTime t s, Monad m) => Wire s e m Double Double
challenge1 = integral 0 . pure 20

challenge2 :: (HasTime t s, Monad m) => Wire s () m (Set SDL.Keysym) Double
challenge2 = integral halfWinHeight . velocity

velocity :: (Monad m, Monoid e) => Wire s e m (Set SDL.Keysym) Double
velocity = pure (-250) . when (keyDown SDL.Up)
           <|> pure (250) . when (keyDown SDL.Down)
           <|> pure 0

-- eventLoop :: SDL.Renderer -> IO ()
-- eventLoop renderer = do
--   runRenderer renderer
--   maybe_event <- SDL.pollEvent
--   case maybe_event of
--     Nothing      -> eventLoop renderer
--     (Just event) ->
--       case SDL.eventData event of

--         -- if the event was Quit then halt the loop
--         SDL.Quit -> return ()

--         -- otherwise print the event and continue
--         _        -> print event >> eventLoop renderer

