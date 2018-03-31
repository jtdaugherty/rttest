module Main where

import Control.Monad (forM_)
import Control.Applicative ((<|>))
import Data.Maybe
import Data.Tree
import Data.Tree.Zipper

data NodeInfo =
    Label String
    | UId Int
    | CId Int
    deriving Show

demoTree :: Tree NodeInfo
demoTree = t
    where
        t = Node (Label "Root") [cs, us]
        cs = Node (Label "Channels") chans
        us = Node (Label "Users") users
        chans = flip Node [] <$> CId <$> [1, 2, 3, 4, 5]
        users = flip Node [] <$> UId <$> [6, 7, 8, 9, 10]

right :: TreePos Full a -> TreePos Full a
right p = maybe (error "") id $
    (next p) <|> do
        par <- parent p
        case null $ after par of
            True -> firstChild =<< (firstChild $ root p)
            False -> firstChild =<< next par

left :: TreePos Full a -> TreePos Full a
left p = maybe (error "") id $
    (prev p) <|> do
        par <- parent p
        case null $ before par of
            True -> lastChild =<< (lastChild $ root p)
            False -> lastChild =<< prev par

main :: IO ()
main = do
    putStrLn $ drawTree $ fmap show demoTree

    let pos = fromJust $ firstChild =<< (firstChild $ fromTree demoTree)
        five = right $ right $ right $ right pos

        go _ 0 v = v
        go f n v = go f (n - 1) (f v)

        goRight = go right
        goLeft = go left

    putStrLn "Starting position:"
    print $ label pos

    putStrLn "Rightward motion:"
    forM_ [1..11] $ \i ->
        print $ label $ goRight i pos

    putStrLn "Leftward motion:"
    forM_ [1..11] $ \i ->
        print $ label $ goLeft i pos
