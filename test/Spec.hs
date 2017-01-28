import Test.QuickCheck

prop_a :: [Int] -> Bool
prop_a xs = reverse (reverse xs) == xs

main :: IO ()
main = quickCheck (prop_a)
