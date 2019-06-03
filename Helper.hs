module Helper (
    LaTeX (..), oneOfEach, (\\\)
) where

class LaTeX a where
    toLaTeX :: a -> String

oneOfEach :: [[a]] -> [[a]]
oneOfEach [] = [[]]
oneOfEach (xs:xss) = [ z:zs | z <- xs, zs <- oneOfEach xss ]    

(\\\) :: Eq a => [a] -> [a] -> [a]
xs \\\ ys = filter (`notElem` ys) xs