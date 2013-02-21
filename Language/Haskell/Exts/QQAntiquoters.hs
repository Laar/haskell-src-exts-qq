{-# LANGUAGE QuasiQuotes #-}
-- | Basic antiquoters.
module Language.Haskell.Exts.QQAntiquoters (
    -- * `AntiquoterPass`es
    parenSplice, underscored, doublePParen, doubleTParen,
    idSplice,
    -- * `Antiquoter`s
    defaultPatAntiquoter,
    defaultExpAntiquoter,
    noAntiquoter, basicAntiquoter,
) where

import qualified Language.Haskell.Exts as Hs
import qualified Language.Haskell.Meta.Syntax.Translate as Hs
import Language.Haskell.Exts.QQBase
import Language.Haskell.TH.Syntax
import Data.List (isPrefixOf, isSuffixOf)

-- | Antiquoter that executes `ParenSplice` splices (those of the form
-- @$(complicated expression)@), the inner expression should be convertable to
-- template haskell.
parenSplice :: ExpAntiquoterPass Hs.Exp
parenSplice [hsBasic| $($e)|] = Just . return $ Hs.toExp e
parenSplice _                 = Nothing

-- | Splices idents of the form @__f__@ with the variable @f@, which should be
-- of type `String`. Note, as the variable should start with a lowerCase letter
-- it will always represent a variable when used in pattern contexts (and thus
-- cannot be used to capture constructor names). (Therefore it's not in the
-- default set for pattern contexts.)
underscored :: GenAntiquoterPass Hs.Name
underscored (Hs.Ident n) | "__" `isPrefixOf` n, "__" `isSuffixOf` n  =
    let nn = take (length n - 4) (drop 2 n)
    in Just $ con 'Hs.Ident [var (mkName nn)]
underscored _ = Nothing

-- | Antiquotes @((x))@ in patterns into the variable @x@.
doublePParen :: GenAntiquoterPass Hs.Pat
doublePParen (Hs.PParen (Hs.PParen (Hs.PVar (Hs.Ident n)))) =
    Just $ var (mkName n)
doublePParen _ = Nothing

-- | Antiquotes @((t))@ in types into the variable @t@.
doubleTParen :: GenAntiquoterPass Hs.Type
doubleTParen (Hs.TyParen (Hs.TyParen (Hs.TyVar (Hs.Ident n)))) =
    Just . var $ mkName n
doubleTParen _ = Nothing

-- | The default pattern context antiquoter using `doublePParen` and
-- `doubleTParen` in addition to `idSplice`.
defaultPatAntiquoter :: GenAntiquoter
defaultPatAntiquoter = basicAntiquoter
    `antiL` doublePParen
    `antiL` doubleTParen
-- | The default expression context antiquoter, this extends the
-- `defaultPatAntiquoter` with `underscored` and `parenSplice`.
defaultExpAntiquoter :: ExpAntiquoter
defaultExpAntiquoter = defaultPatAntiquoter
    `antiL` underscored
    `antiL` parenSplice
