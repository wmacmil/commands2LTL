{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
module Basic where

import Control.Monad.Identity
import Data.Monoid
import PGF hiding (Tree)
----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

type GAction = Tree GAction_
data GAction_
type GCommand = Tree GCommand_
data GCommand_
type GCommands = Tree GCommands_
data GCommands_
type GListCommand = Tree GListCommand_
data GListCommand_
type GListCommands = Tree GListCommands_
data GListCommands_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  GGo :: Tree GAction_
  GStop :: Tree GAction_
  GTurn :: Tree GAction_
  GCompoundCommand :: GListCommand -> Tree GCommand_
  GSimpleCom :: GAction -> Tree GCommand_
  GOneCommand :: GCommand -> Tree GCommands_
  GListCommand :: [GCommand] -> Tree GListCommand_
  GListCommands :: [GCommands] -> Tree GListCommands_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (GGo,GGo) -> and [ ]
    (GStop,GStop) -> and [ ]
    (GTurn,GTurn) -> and [ ]
    (GCompoundCommand x1,GCompoundCommand y1) -> and [ x1 == y1 ]
    (GSimpleCom x1,GSimpleCom y1) -> and [ x1 == y1 ]
    (GOneCommand x1,GOneCommand y1) -> and [ x1 == y1 ]
    (GListCommand x1,GListCommand y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GListCommands x1,GListCommands y1) -> and [x == y | (x,y) <- zip x1 y1]
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GAction where
  gf GGo = mkApp (mkCId "Go") []
  gf GStop = mkApp (mkCId "Stop") []
  gf GTurn = mkApp (mkCId "Turn") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "Go" -> GGo 
      Just (i,[]) | i == mkCId "Stop" -> GStop 
      Just (i,[]) | i == mkCId "Turn" -> GTurn 


      _ -> error ("no Action " ++ show t)

instance Gf GCommand where
  gf (GCompoundCommand x1) = mkApp (mkCId "CompoundCommand") [gf x1]
  gf (GSimpleCom x1) = mkApp (mkCId "SimpleCom") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "CompoundCommand" -> GCompoundCommand (fg x1)
      Just (i,[x1]) | i == mkCId "SimpleCom" -> GSimpleCom (fg x1)


      _ -> error ("no Command " ++ show t)

instance Gf GCommands where
  gf (GOneCommand x1) = mkApp (mkCId "OneCommand") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "OneCommand" -> GOneCommand (fg x1)


      _ -> error ("no Commands " ++ show t)

instance Gf GListCommand where
  gf (GListCommand [x1,x2]) = mkApp (mkCId "BaseCommand") [gf x1, gf x2]
  gf (GListCommand (x:xs)) = mkApp (mkCId "ConsCommand") [gf x, gf (GListCommand xs)]
  fg t =
    GListCommand (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1,x2]) | i == mkCId "BaseCommand" -> [fg x1, fg x2]
      Just (i,[x1,x2]) | i == mkCId "ConsCommand" -> fg x1 : fgs x2


      _ -> error ("no ListCommand " ++ show t)

instance Gf GListCommands where
  gf (GListCommands [x1]) = mkApp (mkCId "BaseCommands") [gf x1]
  gf (GListCommands (x:xs)) = mkApp (mkCId "ConsCommands") [gf x, gf (GListCommands xs)]
  fg t =
    GListCommands (fgs t) where
     fgs t = case unApp t of
      Just (i,[x1]) | i == mkCId "BaseCommands" -> [fg x1]
      Just (i,[x1,x2]) | i == mkCId "ConsCommands" -> fg x1 : fgs x2


      _ -> error ("no ListCommands " ++ show t)


instance Compos Tree where
  compos r a f t = case t of
    GCompoundCommand x1 -> r GCompoundCommand `a` f x1
    GSimpleCom x1 -> r GSimpleCom `a` f x1
    GOneCommand x1 -> r GOneCommand `a` f x1
    GListCommand x1 -> r GListCommand `a` foldr (a . a (r (:)) . f) (r []) x1
    GListCommands x1 -> r GListCommands `a` foldr (a . a (r (:)) . f) (r []) x1
    _ -> r t

class Compos t where
  compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
         -> (forall a. t a -> m (t a)) -> t c -> m (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . composOpM (Identity . f)

composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composOpM = compos return ap

composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()
composOpM_ = composOpFold (return ()) (>>)

composOpMonoid :: (Compos t, Monoid m) => (forall a. t a -> m) -> t c -> m
composOpMonoid = composOpFold mempty mappend

composOpMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b
composOpMPlus = composOpFold mzero mplus

composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)

newtype C b a = C { unC :: b }
