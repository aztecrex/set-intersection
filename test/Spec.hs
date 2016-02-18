import Test.QuickCheck
import Data.Set (Set (..), empty, insert, intersection)

main :: IO ()
main = do
  putStrLn ""
  quickCheck propAssoc
  quickCheck propLeftIdentity
  quickCheck propRightIdentity

propAssoc :: String -> String -> String -> Bool
propAssoc xs ys zs = mappend x (mappend y z) == mappend (mappend x y) z
  where x = makeSet xs
        y = makeSet ys
        z = makeSet zs

propLeftIdentity :: String -> Bool
propLeftIdentity xs = mappend mempty xs == xs

propRightIdentity :: String -> Bool
propRightIdentity xs = mappend xs mempty == xs

newtype Intersect a = Intersect (Set a) deriving (Show, Eq)

makeSet :: (Ord a) => [a] -> Intersect a
makeSet xs = Intersect ys
  where ys = foldr insert empty xs

instance (Ord a) => Monoid (Intersect a) where
  mempty = Intersect empty
  mappend (Intersect xs) (Intersect ys) = Intersect $ intersection xs ys
