{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

--{-# LANGUAGE PackageImports #-}
--{-# OPTIONS_GHC -fglasgow-exts #-}


module InstrLTL where

import Data.Maybe
import Control.Monad
import Basic
import LTL
import qualified PGF (Tree, showExpr)
import PGF hiding (Tree, showExpr)

instance (Gf (Tree c)) => Show (Tree c) where
  show = PGF.showExpr [] . gf

{-
This file is intentended to do a quick semantic interpretation as a model and
example for a larger grammar being implemented. The limitations of this
degenerate grammar are simply to interpret sequences of unmodified actions as
systematically determined formulas in the fragment of linear temporal logic, one
can essentially just say to do things in a very deliberate sequence.

The GF grammar, outside of these abstract syntax functions outputs lists of
commands, ListCommands, whereby we only use the generic derived lists with at
least one Commands.

  OneCommand   : Command -> Commands ;
  CompoundCommand : [Command] -> Command ;
  SimpleCom      : Action   -> Command ;

The main thing that happens is, lists of commands in individual sentence are put
into a normal form, whereby all ambiguous parses are simply assumed to
correspond to a left-to-right sequence of commands. Then, the sentences are
included in the normal form.

normalizeList x
  where x = "go , stop and turn . stop , go and stop and turn . turn and go ."

yields
"go , stop , turn , stop , go , stop , turn , turn and go ."

The goal now is to take the AST of this normalized list and convert it into another haskell GADT which represents the LTL formula of some form like

-- HELP NEEDED --

listCommand2LTL isn't working, and despite astToAtom working on a few different
cases, it is simply returning empty string when invoked in a seperate function.
I want the Atom to be filled in with linearization information about the single
node action AST, and instead I'm getting the empty node.

-- F (And (Atom "") (F (And (Atom "") (F (And (Atom "") (F (And (Atom "") (F (And (Atom "") (F (And (Atom "") (F (And (Atom "") (F (And (Atom "") (F (Atom "")))))))))))))))))

Why can't we infer the type of astToAtom? What is its type, and is this
side-issue effecting our ability to make it behave properly in listCommand2LTL?

We can see that the astToAtom isn't even working with the callASTtoAtomDebug

-}

example1 = GSimpleCom GTurn : [] :: [GCommand]
exmaple2 = GSimpleCom GGo : GSimpleCom GTurn : []
turn = (GSimpleCom GTurn)
-- >>> astToAtom turn
-- Atom "turn"
-- >>> callASTtoAtomDebug turn
-- Atom ""
-- >>> listCommand2LTL example1
-- F (Atom "")
-- >>> listCommand2LTL exmaple2
-- F (And (Atom "") (F (Atom "")))
-- why isn't this working?

callASTtoAtomDebug :: GCommand -> IO Psi
callASTtoAtomDebug (GSimpleCom x) =
  do
     x' <- (astToAtom x)
     return $ x'

-- example sentences for testing
x = "go , stop and turn . stop , go and stop and turn . turn and go ."
y = "go , stop and turn ."
z = "go and turn . go ."
z' = "go . go ."
z'' = "go ."

-- need the stuff so that we can, when performing the semantic transformation,
-- linearize to locally in the Atom base case (which is intented to be deferred
-- to other parts of the verifier)
gr :: IO PGF
gr = readPGF "pgf/Basic.pgf"

cat :: IO Type
cat = liftM startCat gr

langs :: IO [Language]
langs = liftM languages gr

eng :: IO Language
eng = liftM head langs


-- maybe we wanna get away with not using this otherwise
-- BaseCommands (OneCommand (SimpleCom Go))
semantics :: GListCommands -> IO Psi
semantics x =
  let (GListCommands ((GOneCommand y) : _)) = normalizeList x --hackey, assume single sentence
  in case y of
      q@(GSimpleCom a) -> astToAtom q
      (GCompoundCommand (GListCommand xs)) -> listCommand2LTL xs


listCommand2LTL :: [GCommand] -> IO Psi
listCommand2LTL [] = error "empty tree"
listCommand2LTL [(GSimpleCom x)] = liftM F (astToAtom x)
-- listCommand2LTL [(GSimpleCom x),(GSimpleCom y)] = liftM2 (\a1 a2 -> F (And a1 a2)) (astToAtom x) (liftLinearize y)
listCommand2LTL ((GSimpleCom x):xs) =
  do
    x'  <- astToAtom x
    xs' <- listCommand2LTL xs
    -- return $ F (And _ _)
    return $ F (And x' xs')



-- Linearize a subtree to a string for the atomic logical proposition
-- Why can't we infer the type ?
-- astToAtom :: forall a. Tree a -> IO Psi
astToAtom x =
  do
    gr' <- gr
    eng' <- eng
    let x' = gf x
    return $ Atom (linearize gr' eng' x')

-- Normalize the syntax tree--
-- ad hoc fix, how to do a better job on the base case (this wont generalize in the bigger case?)
-- Normalize the list to still fit a basic category
normalizeList ::  GListCommands -> GListCommands
normalizeList (GListCommands (GOneCommand (GSimpleCom x) : [])) = (GListCommands (GOneCommand (GSimpleCom x) : []))
normalizeList xs =
  let normalizedSublist = normalizeNestedLists xs
  in GListCommands ((GOneCommand (GCompoundCommand normalizedSublist)) : [])

normalizeNestedLists :: GListCommands -> GListCommand
normalizeNestedLists (GListCommands xs) = normalizeListCommand $ GListCommand (unSentence xs)

-- take apart the sentences of commands into individual commands
unSentence :: [GCommands] -> [GCommand]
unSentence ((GOneCommand x):xs) =  x : unSentence xs
unSentence _ = [] -- x ++ unSentence xs

-- Just working on in the context of an individual level.
-- Works by simply concatintating the lists with sublist cases
-- first recursively flattening to
normalizeListCommand :: GListCommand -> GListCommand
normalizeListCommand (GListCommand xs) = GListCommand (concatMap flattenSublist xs)
  where
    flattenSublist :: GCommand -> [GCommand]
    flattenSublist (GCompoundCommand x') = (getListCommands (normalizeListCommand x'))
    flattenSublist x  = x : [] -- (GSimpleCom x')

getListCommands :: GListCommand -> [GCommand]
getListCommands (GListCommand x) = x

---- TESTING AREA----

-- Verifying by eye that normalizeList seems to work.

-- Dante magic
-- >>> gr <- readPGF "pgf/Basic.pgf"
-- >>> cat = startCat gr
-- >>> eng = head $ languages gr
-- >>> goToTheWomanT1 = head $ head $ parseAll gr cat x
-- >>> goToTheWomanAST = fg $ goToTheWomanT1
-- >>> goToTheWomanAST :: GListCommands
-- ConsCommands (OneCommand (CompoundCommand (ConsCommand (SimpleCom Go) (BaseCommand (SimpleCom Stop) (SimpleCom Turn))))) (ConsCommands (OneCommand (CompoundCommand (ConsCommand (SimpleCom Stop) (BaseCommand (SimpleCom Go) (CompoundCommand (BaseCommand (SimpleCom Stop) (SimpleCom Turn))))))) (BaseCommands (OneCommand (CompoundCommand (BaseCommand (SimpleCom Turn) (SimpleCom Go))))))
-- >>> -- goToTheWomanASTRev = reverseCommands goToTheWomanAST :: GListCommands
-- >>> goToTheWomanASTRev = normalizeList goToTheWomanAST :: GListCommands
-- >>> goToTheWomanASTRev
-- BaseCommands (OneCommand (CompoundCommand (ConsCommand (SimpleCom Go) (ConsCommand (SimpleCom Stop) (ConsCommand (SimpleCom Turn) (ConsCommand (SimpleCom Stop) (ConsCommand (SimpleCom Go) (ConsCommand (SimpleCom Stop) (ConsCommand (SimpleCom Turn) (BaseCommand (SimpleCom Turn) (SimpleCom Go)))))))))))
-- >>> goToTheWomanRev = gf goToTheWomanASTRev
-- >>> linearize gr eng goToTheWomanRev
-- "go , stop , turn , stop , go , stop , turn , turn and go ."

--Seeing that our to the AST form fails

-- >>> gr <- readPGF "pgf/Basic.pgf"
-- >>> cat = startCat gr
-- >>> eng = head $ languages gr
-- >>> goToTheWomanT1 = head $ head $ parseAll gr cat x
-- >>> goToTheWomanAST = fg $ goToTheWomanT1
-- >>> goToTheWomanAST :: GListCommands
-- ConsCommands (OneCommand (CompoundCommand (ConsCommand (SimpleCom Go) (BaseCommand (SimpleCom Stop) (SimpleCom Turn))))) (ConsCommands (OneCommand (CompoundCommand (ConsCommand (SimpleCom Stop) (BaseCommand (SimpleCom Go) (CompoundCommand (BaseCommand (SimpleCom Stop) (SimpleCom Turn))))))) (BaseCommands (OneCommand (CompoundCommand (BaseCommand (SimpleCom Turn) (SimpleCom Go))))))
-- >>> semantics goToTheWomanAST
-- F (And (Atom "") (F (And (Atom "") (F (And (Atom "") (F (And (Atom "") (F (And (Atom "") (F (And (Atom "") (F (And (Atom "") (F (And (Atom "") (F (Atom "")))))))))))))))))

-- one could have a tree :
-- go to the _cafe if theres no traffic on _, otherwise take the _ to the _(2)cafe, stoping by Greg's on the way
-- if traffic_on_ then _ requires a type theoretical interpretation to evaluate the first argument
-- what if we are to interpret LTL formulas as types (specifications) which programs satisfy
