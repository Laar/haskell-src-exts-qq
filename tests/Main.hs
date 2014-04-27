{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Language.Haskell.Exts
import Language.Haskell.Exts.QQ

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ testGroup "Simple tests" simpleTests
    ]


-- Simple tests which test only the basic parsing and conversion to
-- haskell of the several antiquoters.
simpleTests :: [TestTree]
simpleTests = 
    [ testCase "Expression quoting" $
        [hs| const 1 |]
            @?= App (Var (UnQual (Ident "const"))) (Lit (Int 1))
    , testCase "Type quoting" $
        [ty| [a] -> Int |]
            @?= TyFun (TyList (TyVar (Ident "a"))) (TyCon (UnQual (Ident "Int")))
    , testCase "Decl quoting" . assert $ case [dec| one = 1 |] of
        -- It includes a src location which should not matter
        PatBind _ (PVar (Ident "one")) Nothing 
            (UnGuardedRhs (Lit (Int 1))) (BDecls []) -> True
        _  -> False
    , testCase "Decls quoting" . assert $ case [decs|
one :: Int
one = 1|] of
            [ TypeSig _ [Ident "one"] (TyCon (UnQual (Ident "Int")))
             , PatBind _ (PVar (Ident "one")) 
                Nothing (UnGuardedRhs (Lit (Int 1))) (BDecls [])
                ] -> True
            _ -> False
    ]
