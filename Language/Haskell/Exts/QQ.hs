-- | This module defines quasiquoters for haskell-src-exts expressions and
-- declarations.
--
-- Antiquotations steal the splice syntax of Template Haskell, so for
-- example example 'x' appears antiquoted in @[$hs| $x ++ $(Hs.strE \"bar\") |]@.
-- Expressions appearing inside parenthesized splices are limited to concrete
-- syntax expressible by Template Haskell's 'Exp' data type.
--
-- Names in patterns can also be antiquoted, using double parentheses. For
-- instance:
--
-- > let x = Hs.name "n" in [hs| \ ((x)) -> $(Hs.Var (Hs.UnQual x)) + 1 |]
--
-- Alternatively, one can use the double underscore syntax, useful when
-- antiquoting a function name as in the following:
--
-- > let f = "incr"
-- >     fE = Hs.Var $ Hs.UnQual $ Hs.name f
-- > in [hs| let __f__ x = x + 1 in $fE 10 |]
--
-- The double parentheses syntax is also used for antiquoting types. For
-- instance:
--
-- > let typ = Hs.TyCon (Hs.UnQual $ Hs.name "Int")
-- > in [hs| 1 :: ((typ)) |]
--
-- In a pattern context, antiquotations use the same syntax.

module Language.Haskell.Exts.QQ (hs, pat,dec, decs, ty,
    hsWithMode, patWithMode, decWithMode, decsWithMode, tyWithMode) where

import qualified Language.Haskell.Exts as Hs
import Language.Haskell.TH.Quote
import Language.Haskell.Exts.QQBase
import Language.Haskell.Exts.QQAntiquoters

-- | A quasiquoter for expressions. All Haskell extensions known by
-- haskell-src-exts are activated by default.
hs :: QuasiQuoter
hs = hsWithMode allExtensions

-- | A quasiquoter for types. All Haskell extensions known by
-- haskell-src-exts are activated by default.
ty :: QuasiQuoter
ty = tyWithMode allExtensions

-- | A quasiquoter for a pattern
pat :: QuasiQuoter
pat = patWithMode allExtensions

-- | A quasiquoter for a single top-level declaration.
dec :: QuasiQuoter
dec = decWithMode allExtensions

-- | A quasiquoter for multiple top-level declarations.
decs :: QuasiQuoter
decs = decsWithMode allExtensions

-- | Rather than importing the above quasiquoters, one can create custom
-- quasiquoters with a customized 'ParseMode' using this function.
--
-- > hs = hsWithMode mode
-- > dec = decWithMode mode
-- > decs = decsWithMode mode
hsWithMode :: Hs.ParseMode -> QuasiQuoter
hsWithMode = withMode hsGen

patWithMode :: Hs.ParseMode -> QuasiQuoter
patWithMode = withMode patGen

decWithMode :: Hs.ParseMode -> QuasiQuoter
decWithMode = withMode decGen

decsWithMode :: Hs.ParseMode -> QuasiQuoter
decsWithMode = withMode $ mappedGenericQQ strip
    where
        -- Implementation note, to parse multiple decls it's (ab)used that a
        -- listing of decls (possibly with import istatements and other extras)
        -- is a valid module.
        strip :: Hs.Module -> [Hs.Decl]
        strip (Hs.Module _ _ _ _ _ _ decls) = decls

tyWithMode :: Hs.ParseMode -> QuasiQuoter
tyWithMode = withMode tyGen

withMode :: GenericQuasiQuoter -> Hs.ParseMode -> QuasiQuoter
withMode gqq = gqq defaultExpAntiquoter defaultPatAntiquoter

