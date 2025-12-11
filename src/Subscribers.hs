module Subscribers (Subscriber (..), action, subName, state) where

import Data.IORef

data Subscriber = Subscriber {subName :: String, action :: IORef ([(Double, Double)], Double) -> (Double, Double) -> IO (), state :: IORef ([(Double, Double)], Double)}
