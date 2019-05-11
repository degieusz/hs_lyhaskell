module Lib
    {-( someFunc, elemsSumRoot, Point(..), Shape(Rectangle) where-}
    where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-elemsSumRoot :: [Double]-}
elemsSumRoot :: Int
elemsSumRoot = length $ takeWhile (<1000)  $ scanl (\acc x -> acc + x) 0 $ map sqrt  [1..]
{-elemsSumRoot = takeWhile (<1000) (+) 0 (map sqrt  [1..]))-}

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rect Point Point deriving (Show)

shapeSur :: Shape -> Float
shapeSur (Circle _ r) = pi * r ^2
shapeSur (Rect (Point x y) (Point x1 y1)) =  (abs $  x-x1) * (abs $ y-y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) x1 y1= Circle (Point  (x+x1) (y+y1)) r
nudge (Rect (Point x y) (Point x1 y1 )) a b  =
    Rect (Point (x+a) (y+b)) (Point (x1+a) (y1+b))

data Person = Person { fname :: String
                     , lname ::String
                     , age ::Int
                     {-, height ::Float-}
                     {-, phoneNo :: String-}
                     {-, flavor :: String-}
                     } deriving (Show, Eq, Read)

data Maybe a  = Nothing | Just a


data Vec a = Vec a a a deriving( Show )

vplus :: (Num t) => Vec t -> Vec t -> Vec t
(Vec i j k ) `vplus` (Vec l m n) = Vec (i+l) (j+m) (k+n)

vmul :: (Num t) => Vec t -> t -> Vec t
(Vec i j k ) `vmul` n = Vec (i*n) (j*n) (k*n)

vscalmul :: (Num t) => Vec t -> t -> t
(Vec i j k ) `vscalmul` n =(i*n)+(j*n)+(k*n)
