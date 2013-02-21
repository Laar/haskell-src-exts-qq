-- Needed for the type signatures/aliases of the Antiquoters
{-# LANGUAGE Rank2Types #-}
-- | Inner workings of haskell-src-exts-qq. It defines the basic types including
-- the `GenericQuasiquoter` and `AntiQuoter` aliases and the low-level functions
-- to work with them.
module Language.Haskell.Exts.QQBase (

    -- * Quasiquoters
    -- | The most general quasiquoters which don't have any antiquoters applied
    -- nor `ParseMode`.
     GenericQuasiQuoter, hsGen, tyGen, patGen, decGen,
    -- ** Basic quasiquoters
    -- | Basic Quasiquoters which can be used to quasiquote when creating
    -- `AntiquoterPass`es.
    hsBasic, tyBasic, patBasic,
    -- ** Low level quasiquoter construction
    qq,
    mkGenericQQ, mappedGenericQQ,
    -- * Anti quotation
    -- ** Quotable class
    Quotable(..),
    -- ** AntiQuoters and passes
    -- | Various forms of functions used to form the final antiquotation. In
    -- general the -Pass types denote a single transformation with a specific
    -- source type. These can be combined using `extQ` to form the -Pass less
    -- version. The result type, and thus the location where it can be used,
    -- is used as a prefix for the name. Exp and Pat are used to denote
    -- antiquoters which can only be used when quasiquoting expression c.q.
    -- patterns. Gen is used when the `Antiquoter` can have any `Quotable`
    -- output.
    AntiquoterPass, GenAntiquoterPass, ExpAntiquoterPass, PatAntiquoterPass,
    Antiquoter, GenAntiquoter, ExpAntiquoter, PatAntiquoter,
    -- ** Basis antiquoters
    noAntiquoter, basicAntiquoter, idSplice,
    -- ** antiquoter combinators    
    antiL, antiR,
    -- * Utilities
    allExtensions,
) where

import qualified Language.Haskell.Exts as Hs
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Control.Monad
import Data.Generics

type GenericQuasiQuoter = ExpAntiquoter -> PatAntiquoter -> Hs.ParseMode
    -> QuasiQuoter

hsGen, tyGen, patGen, decGen :: GenericQuasiQuoter
hsGen  = mkGenericQQ (undefined :: Hs.Exp)
tyGen  = mkGenericQQ (undefined :: Hs.Type)
patGen = mkGenericQQ (undefined :: Hs.Pat)
decGen = mkGenericQQ (undefined :: Hs.Decl)

hsBasic, tyBasic, patBasic :: QuasiQuoter
hsBasic  = basicQQ hsGen
tyBasic  = basicQQ tyGen
patBasic = basicQQ patGen

basicQQ :: GenericQuasiQuoter -> QuasiQuoter
basicQQ gqq = gqq basicAntiquoter basicAntiquoter allExtensions

-- | Constructs the standard `GenericQuasiQuoter`. It needs an extra paramater
-- to  determine the output type, this parameter is not used thus it's safe to
-- use `undefined`.
mkGenericQQ :: (Data r, Hs.Parseable r) => r -> GenericQuasiQuoter
mkGenericQQ a = \eaq paq mode
    -> qq eaq paq (Hs.parseWithMode mode `asParserOf` a)
    where
        asParserOf :: (Data r, Hs.Parseable r) => (String -> Hs.ParseResult r)
            -> r -> (String -> Hs.ParseResult r)
        asParserOf = const

-- | Constructs a `GenericQuasiQuoter` where the result of the parsed data is
-- transformed by the given function.
mappedGenericQQ :: (Data s, Hs.Parseable r) => (r -> s) -> GenericQuasiQuoter
mappedGenericQQ f = \eaq paq mode
    -> qq eaq paq (\s -> fmap f $ Hs.parseWithMode mode s)

-- | ParseMode for parsing with all extensions enabled.
allExtensions :: Hs.ParseMode
allExtensions = Hs.defaultParseMode{Hs.extensions = Hs.knownExtensions}

-- | Constructs a `QuasiQuoter` from two antiquoters and a parser.
qq :: Data a => ExpAntiquoter -> PatAntiquoter
    -> (String -> Hs.ParseResult a) -> QuasiQuoter
qq antiquoteExp antiquotePat parser
    = QuasiQuoter { quoteExp = parser `project` dataToExpQ'
                  , quotePat = parser `project` dataToPatQ'
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 613
                  , quoteType = error "Unimplemented."
                  , quoteDec = error "Unimplemented."
#endif
                  }
    where
        dataToExpQ' = dataToQa qualify lit con antiquoteExp
        dataToPatQ' = dataToQa qualify lit con antiquotePat

project :: Monad m => (a -> Hs.ParseResult b) -> (b -> m c) -> a -> m c
project f k s = case f s of
                  Hs.ParseOk x -> k x
                  Hs.ParseFailed _ err -> fail err

-- | The generic functions in 'Language.Haskell.TH.Quote' don't use global
-- names for syntax constructors previous to GHC 7.4.1. This has the unfortunate
-- effect of breaking quotation when the haskell-src-exts syntax module is
-- imported qualified. The solution is to set the flavour of all names to
-- 'NameG' on older versions of GHC.
-- See also <http://www.haskell.org/pipermail/glasgow-haskell-users/2013-February/023793.html>.
qualify :: Name -> Name
#if defined(__GLASGOW_HASKELL__) &&  __GLASGOW_HASKELL__ < 704
-- Need special cases for constructors used in string literals. Assume nearly
-- all else is a datatype defined in Syntax module of haskell-src-exts.
qualify n | ":" <- nameBase n = '(:)
          | "[]" <- nameBase n = '[]
          | "(,)" <- nameBase n = '(,)
          | "Nothing" <- nameBase n = 'Nothing
          | "Just" <- nameBase n = 'Just
          | "True"      <- nameBase n = 'True
          | "False"     <- nameBase n = 'False
          | "Left"      <- nameBase n = 'Left
          | "Right"     <- nameBase n = 'Right
          | "LT"        <- nameBase n = 'LT
          | "EQ"        <- nameBase n = 'EQ
          -- GT is also exported by Data.Generics
          | "GT"        <- nameBase n = 'Prelude.GT
          | "SrcLoc" <- nameBase n = 'Hs.SrcLoc
          | "Boxed" <- nameBase n = 'Hs.Boxed
          | otherwise = Name (mkOccName (nameBase n)) flavour
    where pkg = "haskell-src-exts-" ++ VERSION_haskell_src_exts
          flavour = NameG VarName (mkPkgName pkg)
                    (mkModName "Language.Haskell.Exts.Syntax")
#else
qualify n = n
#endif

-- | An `AntiquoterPass` is a single transformation of a input type @e@ that
-- `Maybe` transformed into some output @q@ in the `Q` monad. Effectively it
-- antiquotes or transforms the haskell-src-exts syntax into template haskell.
type AntiquoterPass  q e =                         e -> Maybe (Q q)
-- | A generalized version of `AntiquoterPass` where the input can be any `Data`
-- instance. Multiple `AntiquoterPass`es can be combined into an `Antiquoter` by
-- using `antiL` and `antiR` using `noAntiquoter` like a neutral element.
type Antiquoter      q   = forall e. Data e     => AntiquoterPass q   e
-- | A generalization of `AntiquoterPass` where the output could be any
-- instance of the `Quotable` class. This represents an antiquotation pass that
-- can be applied regardless of the destination type.
type GenAntiquoterPass e = forall q. Quotable q => AntiquoterPass q   e
-- | A generalized version of `Antiquoter` and `GenAntiquoterPass`, a general
-- antiquotation independent of input and output.
type GenAntiquoter       = forall q. Quotable q => Antiquoter     q
-- | `AntiquoterPass` which should result in an `Exp`.
type ExpAntiquoterPass e =                         AntiquoterPass Exp e
-- | `Antiquoter` which should result in an `Exp`.
type ExpAntiquoter       =                         Antiquoter     Exp
-- | `AntiquoterPass` which should result in an `Pat`.
type PatAntiquoterPass e =                         AntiquoterPass Pat e
-- | `Antiquoter` which should result in an `Pat`.
type PatAntiquoter       =                         Antiquoter     Pat

-- | An antiquoter that does nothing.
noAntiquoter :: GenAntiquoter
noAntiquoter = const Nothing

-- | The basic antiquoter which only antiquotes `IdSplice`s.
basicAntiquoter :: GenAntiquoter
basicAntiquoter = noAntiquoter `antiL` idSplice

-- | Extend an `AntiQuoter` by an `AntiQuoterPass` prefering the Antiquoter on
-- the left side (when using in infix notation).
antiL :: Typeable e => Antiquoter q -> AntiquoterPass q e -> Antiquoter q
antiL f g a = f a `mplus` mkQ mzero g a

-- | Extend an `AntiQuoter` by an `AntiQuoterPass` prefering the AntiquoterPass
-- on the right side (when using in infix notation).
antiR :: Typeable e => Antiquoter q -> AntiquoterPass q e -> Antiquoter q
antiR f g a = mkQ mzero g a `mplus` f a

-- | An AntiquoterPass that antiquotes `IdSplice`s (those of the form @ $a @).
idSplice :: GenAntiquoterPass Hs.Exp
idSplice (Hs.SpliceExp (Hs.IdSplice v)) = Just . var $ mkName v
-- ParenSplices can't be antiquoted here as they contain expressions.
idSplice _ = Nothing

-- | `Quotable` generalises over the possible output types from a quasiquote
-- therefore enabling to write antiquoters that can be used for expression
-- contexts as well as pattern contexts.
class Quotable q where
    var :: Name -> Q q
    con :: Name -> [Q q] -> Q q
    lit :: Lit  -> Q q

instance Quotable Exp where
    var = varE
    con = foldl appE . conE
    lit = litE

instance Quotable Pat where
    var = varP
    con = conP
    lit = litP