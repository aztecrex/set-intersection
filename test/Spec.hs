import Test.QuickCheck
import Data.Set (Set (..), empty, insert, intersection)

main :: IO ()
main = do
  putStrLn ""
  quickCheck propAssoc
  quickCheck propLeftIdentity
  quickCheck propRightIdentity

propAssoc :: Intersect -> Intersect -> Intersect -> Bool
propAssoc a b c =        a ⋅ (b ⋅ c) ≡ (a ⋅ b) ⋅ c

propLeftIdentity :: Intersect -> Bool
propLeftIdentity a =     φ ⋅ a ≡ a

propRightIdentity :: Intersect -> Bool
propRightIdentity a =    a ⋅ φ ≡ a

{--
Newtype ensures we can create an instance of Monoid even though
Set is already an instance wrt union.
--}
newtype Intersect = Intersect (Set Char) deriving (Show, Eq)

{--
Construct a set of lowercase letters from a list of Char. Throws if any
illegal element is in the source list.
--}
makeSet :: String -> Intersect
makeSet xs = Intersect $ foldr insert empty valid
  where valid =
          if all (flip elem ['a'..'z']) xs
          then xs
          else error xs ++ " contains illegal characters (a..z only allowed)"

instance Monoid Intersect where
  mempty = makeSet ['a'..'z']  -- the entire set of lower case letters
  mappend (Intersect xs) (Intersect ys) = Intersect $ intersection xs ys

-- because we can...
(⋅) :: (Monoid a) => a -> a -> a
infixl 6 ⋅
(⋅) = mappend

φ :: (Monoid a) => a
φ = mempty

(≡) :: (Eq a) => a -> a -> Bool
infix 4 ≡
(≡) = (==)

{--
This is for generating arbitary subsets of the set of all lowercase
letters.
--}
genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeSet :: Gen Intersect
genSafeSet = fmap makeSet $ listOf genSafeChar

instance Arbitrary Intersect where
    arbitrary = genSafeSet

