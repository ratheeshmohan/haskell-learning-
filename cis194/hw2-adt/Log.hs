module Log where

import Control.Applicative

data MessageType = Info
                 | Warning
                 | Error Integer
                 deriving (Show, Eq)

type TimeStamp = Integer

data LogMessage = LogMessage {
                              msgType :: MessageType,
                              time :: TimeStamp,
                              msg ::String
                             }
                | Unknown String
                deriving (Show, Eq)

data Tree a = Node {
                   content :: a,
                   left :: Tree a,
                   right :: Tree a
                 }
              | Empty

instance Foldable Tree where
  foldr f z Empty        = z
  foldr f z (Node c l r)   = foldr f (f c (foldr f z r)) l


insert :: LogMessage -> Tree LogMessage -> Tree LogMessage

insert (Unknown _) t = t
insert l Empty = Node l Empty Empty
insert l t
        | time (content t) < time l = insert l (left t)
        | otherwise                 = insert l (right t)

build :: [LogMessage] -> Tree LogMessage
build  = foldr (\a b -> insert a b) Empty

inorder :: Tree LogMessage -> [LogMessage]
inorder = foldr (:) []

-- ++ runs on linearly on left argument
--inorder Empty = []
--inorder t =  inorder (left t) ++ [content t]  ++ inorder (right t)

