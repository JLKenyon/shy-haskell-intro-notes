{-# LANGUAGE FlexibleContexts #-}

module Main where

import Linear.V2 (V2(V2))
import Linear (norm)

import Control.Monad.Free
import Control.Monad
import Data.Monoid

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

import qualified Helm.Cmd as Cmd
import qualified Helm.Time as Time
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Sub as Sub

-- Define my "language"
data AgentAction next = Jump (V2 Double) next
                      | Move (V2 Double) next
                      | Rest Double next
                      | Done

-- Must be a Monad, so it must be a Functor too.
instance Functor AgentAction where
    fmap f (Jump x next) = Jump x (f next)
    fmap f (Move x next) = Move x (f next)
    fmap f (Rest x next) = Rest x (f next)
    fmap f Done          = Done

data Agent = Agent {
    name :: String,
    location :: (V2 Double),
    speed :: Double,
    next :: Free AgentAction ()
}

agent1 = Agent "agent1" (V2 0.0 0.0) 100.0 prog1
agent2 = Agent "agent2" (V2 0.0 0.0) 200.0 prog2
agent3 = Agent "agent3" (V2 0.0 0.0) 8.0 prog3
agent4 = Agent "agent4" (V2 0.0 0.0) 16.0 prog3

init_agents = [agent1, agent2, agent3, agent4]


data Action = Idle | UpdateAgents Double
data Model = Model (V2 Double) [Agent]

initial :: (Model, Cmd SDLEngine Action)
initial = ((Model (V2 0.0 0.0) init_agents), Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle = (model, Cmd.none)
update (Model pos agents) (UpdateAgents dt) = (Model pos new_agents, Cmd.none)
  where
    new_agents = pincr dt agents

subscriptions :: Sub SDLEngine Action
subscriptions = Time.fps 60 (\dt_ms -> UpdateAgents (dt_ms / 1000.0))

view :: Model -> Graphics SDLEngine
red_square_at v = move v $ filled (rgb 1 0 0) $ square 10
view (Model pos agents) = Graphics2D $ collage $ [red_square_at pos] ++ fmap (red_square_at . location) agents

-- Create a clean interface for "programming" in my language
ag_jump :: MonadFree AgentAction m => (Double, Double) -> m ()
ag_jump (x, y) = liftF (Jump (V2 x y) ())
ag_move (x, y) = liftF (Move (V2 x y) ())
ag_rest     x  = liftF (Rest x ())
ag_done        = liftF Done

prog1 = do
    ag_jump (400, 400)
    forever $ do
        ag_move (400, 400)
        ag_move (500, 400)
        ag_move (500, 500)
        ag_move (400, 500)
        ag_rest 2.0

prog2 = do
    ag_jump (200, 200)
    forever $ do
        ag_move (200, 200)
        ag_move (100, 100)
        ag_move (100, 200)
        ag_move (200, 100)
        ag_rest 3.0

prog3 = do
  ag_jump (100, 400)
  ag_move (300, 400)
  ag_done

dist :: (V2 Double) -> (V2 Double) -> Double
dist v1 v2 = Linear.norm (v2 - v1)

move_towards :: (V2 Double) -> (V2 Double) -> Double -> (V2 Double)
move_towards (V2 x1 y1) (V2 x2 y2) rdy_dst = V2 (x1 + dx) (y1 + dy)
    where
        rem_dist = dist (V2 x1 y1) (V2 x2 y2)
        (V2 vx vy) = V2 ((x2 - x1) / rem_dist) ((y2 - y1) / rem_dist)
        (V2 dx dy) = V2 (vx * rdy_dst) (vy * rdy_dst)

-- Simulator + Interpreter

incr dt (Agent name loc spd (Free (Jump dest nxt))) = incr dt (Agent name dest spd nxt)

incr dt (Agent name loc spd (Free (Rest dur nxt))) = if (dt < dur)
    then (Agent name loc spd (Free (Rest (dur - dt) nxt)))
    else (incr  (dt-dur) (Agent name loc spd nxt))

incr dt (Agent name loc spd cur@(Free (Move dest nxt))) = agt 
    where
        rem_dist = dist loc dest
        rdy_dist = spd * dt
        agt = if rdy_dist < rem_dist
            then Agent name (move_towards loc dest rdy_dist) spd cur
            else incr  dt (Agent name loc spd nxt)

incr dt (Agent name loc spd (Free Done)) = Agent name loc spd ag_done

pincr dt lst = map (incr  dt) lst

-- Create an actual world and run the simulator on it.

main :: IO ()
main = do
  engine <- SDL.startup
  run engine GameConfig
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }

