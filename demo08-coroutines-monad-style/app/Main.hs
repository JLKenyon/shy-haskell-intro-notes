{-# LANGUAGE FlexibleContexts #-}
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate

import Control.Monad.Free
import Control.Monad
import Data.Monoid

-- Define my "language"
data Action next = Jump (Float, Float) next
                 | Move (Float, Float) next
                 | Rest Float next
                 | Done

-- Must be a Monad, so it must be a Functor too.
instance Functor Action where
    fmap f (Jump x next) = Jump x (f next)
    fmap f (Move x next) = Move x (f next)
    fmap f (Rest x next) = Rest x (f next)
    fmap f Done          = Done

-- Create a clean interface for "programming" in my language

jump x = liftF (Jump x ())
move x = liftF (Move x ())
rest x = liftF (Rest x ())
done   = liftF Done

-------------------
-- The good stuff!!
-- These are my example programs
prog1 = do
    jump (100,100)
    forever $ do
        move (100,100)
        move (200,100)
        move (200,200)
        move (100,200)
        rest 2.0

prog2 = do
    jump (-100,-100)
    forever $ do
        move (-100,-100)
        move (-200,-200)
        move (-200,-100)
        move (-100,-200)
        rest 3.0

prog3 = do
  jump (-200, 0)
  move (200, 0)
  done

-- Create a simulation language
data Agent = Agent {
    location :: (Float, Float),
    speed :: Float,
    next :: Free Action ()
}

-- How to render our "world"
frame :: Agent -> Picture

frame (Agent (x,y) _ (Free (Move (x2,y2) _))) = mconcat [Color black
    $ Translate x y 
    $ circle 10,
    Color black $ line [(x,y), (x2,y2)]
    ]

frame (Agent (x,y) _ _) = Color black
    $ Translate x y
    $ circle 10

pframe :: [Agent] -> Picture
pframe agents = mconcat (map frame agents)

-- Vector Math. Pythagoras, unit vectors, etc
dist :: (Float, Float) -> (Float, Float) -> Float
dist (x1,y1) (x2,y2) = sqrt (((x2 - x1) ** 2) + ((y2 - y1) ** 2))

move_towards (x1,y1) (x2,y2) rdy_dst = (x1 + dx, y1 + dy)
    where
        rem_dist = dist (x1,y1) (x2,y2)
        (vx, vy) = ((x2 - x1) / rem_dist, (y2 - y1) / rem_dist)
        (dx, dy) = (vx * rdy_dst, vy * rdy_dst)

-- Simulator + Interpreter

incr vp dt (Agent loc spd (Free (Jump dest nxt))) = incr vp dt (Agent dest spd nxt)

incr vp dt (Agent loc spd (Free (Rest dur nxt))) = if (dt < dur)
    then (Agent loc spd (Free (Rest (dur - dt) nxt)))
    else (incr vp (dt-dur) (Agent loc spd nxt))

incr vp dt (Agent loc spd cur@(Free (Move dest nxt))) = agt 
    where
        rem_dist = dist loc dest
        rdy_dist = spd * dt
        agt = if rdy_dist < rem_dist
            then Agent (move_towards loc dest rdy_dist) spd cur
            else incr vp dt (Agent loc spd nxt)

incr vp dt (Agent loc spd (Free Done)) = Agent loc spd done

pincr vp dt lst = map (incr vp dt) lst


-- Create an actual world and run the simulator on it.

agent1 = Agent (0,0) 100.0 prog1
agent2 = Agent (0,0) 200.0 prog2
agent3 = Agent (0,0) 8.0 prog3
agent4 = Agent (0,0) 16.0 prog3

main = do
    simulate (InWindow "Clock" (600, 600) (20, 20))
        white 
            60
            [agent1, agent2, agent3, agent4]
            pframe
            pincr

