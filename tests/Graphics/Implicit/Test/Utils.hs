module Graphics.Implicit.Test.Utils where

import Test.QuickCheck (Gen, choose)
import Prelude (drop, length, pure, take, (<$>), (<*>))

randomGroups :: [a] -> Gen [[a]]
randomGroups [] = pure []
randomGroups as = do
  n <- choose (1, length as)
  (:) <$> pure (take n as)
    <*> randomGroups (drop n as)
