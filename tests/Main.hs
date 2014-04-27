{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import Language.Haskell.Exts as Hs
import Language.Haskell.Exts.QQ

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ testGroup "Simple tests" simpleTests
    , testGroup "Anti quoting tests" antiquotingTests
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

antiquotingTests :: [TestTree]
antiquotingTests =
    [ testCase "Double bracket pattern" . assert $
        let x = Ident "pat" 
            quote =  [hs| \((x)) -> 1|]
        in case quote of
            Lambda _ [PVar (Ident "pat")] (Lit (Int 1)) 
                -> True
            _   -> False
    , testCase "Paren splice" . assert $
        let x = Ident "exp"
            quote = [hs| $(Hs.Var (Hs.UnQual x)) |]
        in case quote of
            Var (UnQual (Ident "exp"))
                -> True
            _   -> False
    , testCase "IdSplice" . assert $
        let x = Var . UnQual . Ident $ "exp"
            quote = [hs| $x |]
        in case quote of
            Var (UnQual (Ident "exp"))
                -> True
            _   -> False
    , testCase "Double underscore" . assert $
        let f = "name"
            quote = [hs| let __f__ = 1 in __f__ |]
        in case quote of
            Let (BDecls 
                    -- The pattern binding
                    [PatBind _ (PVar (Ident "name")) 
                        Nothing (UnGuardedRhs (Lit (Int 1))) (BDecls [])])
                    -- the in part
                    (Var (UnQual (Ident "name")))
                -> True
            _   -> False
    , testCase "Combined antiquoting" . assert $
        let x = "x"
            y = Ident "y"
            z = Var . UnQual . Ident $ "z"
            quote = [hs| __x__ $(Var . UnQual $ y) $z|]
        in case quote of
            App (App (Var (UnQual (Ident "x")))  (Var (UnQual (Ident "y")))) 
                    (Var (UnQual (Ident "z")))
                -> True
            _   -> False
    ]
