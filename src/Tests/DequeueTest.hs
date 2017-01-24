module Tests.DequeueTest
 (
 prop_pushpop_front
 ) where

import Dequeue
import Test.QuickCheck

prop_pushpop_front :: Dequeue Int -> Int -> Bool
prop_pushpop_front deq x = toListDEQ deq == toListDEQ (snd ( extractDEQ (popfrontDEQ(pushfrontDEQ deq x))))