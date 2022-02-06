{-# LANGUAGE GADTs #-}

-- list with at least 2 elements
data List2 a where
  Cons2 :: a -> List2 a -> List2 a
  Node2 :: a -> a -> List2 a
  deriving (Show)

data AST where
  Go :: String -> AST
  deriving (Show)

data Tree where
  Atom :: String -> Tree
  F    :: Tree -> Tree
  And  :: Tree -> Tree -> Tree
  deriving (Show)

gopark = Go "park"
goleft = Go "left"
goright = Go "right"
goperson = Go "person"
goshop = Go "shop"
goman = Go "man"
goperson2 = Go "person2"

dostuff :: List2 AST -> Tree
dostuff (Node2 (Go x) (Go y)) = F (And (Atom x) (F (Atom y)))
dostuff (Cons2 (Go x) xs) = F (And (Atom x) (dostuff xs))

dostuff' :: [AST] -> Tree
dostuff' [] = error "empty tree"
dostuff' [(Go x)] = F (Atom x)
dostuff' [(Go x),(Go y)] = F (And (Atom x) (Atom y))
dostuff' ((Go x):xs) = F (And (Atom x) (dostuff' xs))

directions1 = Cons2 gopark (Cons2 goleft (Node2 goright goperson))
directions2 = Cons2 goshop (Cons2 goman (Node2 goleft goperson2))

directions12 = Node2 directions1 directions2 

list2List :: List2 x -> [x]
list2List (Node2 x y) = [x,y]
list2List (Cons2 x xs) = x : (list2List xs)

-- >>> dostuff directions1
-- F (And (Atom "park") (F (And (Atom "left") (F (And (Atom "park") (F (Atom "person")))))))

-- >>> dostuff' $ list2List directions1
-- F (And (Atom "park") (F (And (Atom "left") (F (And (Atom "park") (Atom "person"))))))

-- >>> dostuff' $ list2List (foldList2 directions12)
-- F (And (Atom "park") (F (And (Atom "left") (F (And (Atom "right") (F (And (Atom "person") (F (And (Atom "shop") (F (And (Atom "man") (F (And (Atom "left") (Atom "person2"))))))))))))))

-- >>> dostuff' $ directions


ex = Cons2 3 (Cons2 2 (Node2 1 0))
ex2 = Cons2 30 (Cons2 20 (Node2 10 100))
ex3 = Cons2 300 (Cons2 200 (Node2 1000 1000))

ex4 = substList2 ex ex2

ex123 = Cons2 ex (Cons2 ex2 (Node2 ex3 ex4))

-- ++
substList2 :: List2 x -> List2 x -> List2 x
substList2 (Node2 v1 v2) t2 = Cons2 v1 (Cons2 v2 t2)
substList2 (Cons2 v1 t1) t2 = Cons2 v1 (substList2 t1 t2)

-- concat
foldList2 :: List2 (List2 x) -> List2 x
foldList2 (Node2 v1 v2) = substList2 v1 v2
foldList2 (Cons2 v1 t1) = substList2 v1 (foldList2 t1)

-- >>> substList2 ex ex2
-- Cons2 3 (Cons2 2 (Cons2 1 (Cons2 0 (Cons2 30 (Cons2 20 (Node2 10 100))))))
-- >>> foldList2 ex123
-- Cons2 3 (Cons2 2 (Cons2 1 (Cons2 0 (Cons2 30 (Cons2 20 (Cons2 10 (Cons2 100 (Cons2 300 (Cons2 200 (Cons2 1000 (Cons2 1000 (Cons2 3 (Cons2 2 (Cons2 1 (Cons2 0 (Cons2 30 (Cons2 20 (Node2 10 100))))))))))))))))))
