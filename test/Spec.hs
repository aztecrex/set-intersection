import Test.QuickCheck
import Data.Set (Set (..), empty, insert, intersection)

main :: IO ()
main = do
  putStrLn ""
  quickCheck propAssoc
  quickCheck propLeftIdentity
  quickCheck propRightIdentity

propAssoc :: Intersect -> Intersect -> Intersect -> Bool
propAssoc xs ys zs = mappend xs (mappend ys zs) == mappend (mappend xs ys) zs

propLeftIdentity :: Intersect -> Bool
propLeftIdentity xs = mappend mempty xs == xs

propRightIdentity :: Intersect -> Bool
propRightIdentity xs = mappend xs mempty == xs

newtype Intersect = Intersect (Set Char) deriving (Show, Eq)

makeSet :: String -> Intersect
makeSet xs = Intersect $ foldr insert empty valid
  where valid =
          if all (flip elem ['a'..'z']) xs
          then xs
          else error xs ++ " contains illegal characters (a..z only allowed)"

instance Monoid Intersect where
  mempty = makeSet ['a'..'z']
  mappend (Intersect xs) (Intersect ys) = Intersect $ intersection xs ys

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeSet :: Gen Intersect
genSafeSet = fmap makeSet $ listOf genSafeChar


--
-- newtype SafeString = SafeString { unwrapSafeString :: String }
--     deriving Show
--
instance Arbitrary Intersect where
    arbitrary = genSafeSet
--
--
-- testWibble (SafeString str) = str == str
-- Or, you can use forAll at each point you need a safe string:
--
-- testWibble = forAll genSafeString $ \str -> str == str
-- shareimprove this answer

