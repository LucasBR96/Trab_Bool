module Semantics( 
    evaluate_tree,
    to_fun,
    eval
)where

import Symbols( Tree(..) , SemTree(..) ,Tup )

--- interpreta tokens que geram formulas----------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
eval_formula :: [Char] -> ( Bool -> Bool -> Bool )

eval_formula ('~':xs ) = my_neg ( eval_formula xs)
    where
    my_neg :: ( Bool -> Bool -> Bool ) -> ( Bool -> Bool -> Bool )
    my_neg f = g
        where g x y = not( f x y )

eval_formula ">" = imply
    where
    imply :: Bool -> Bool -> Bool
    imply True False = False
    imply _ _ = True

eval_formula "*" = my_and --naõ confundir com and da standard library
    where
    my_and :: Bool -> Bool -> Bool
    my_and True True = True
    my_and _ _ = False

eval_formula "+" = my_or
    where
    my_or :: Bool -> Bool -> Bool
    my_or False False = False
    my_or _ _ = True


eval_formula _ = error "token incorreto para formula"

--interpretam tokens para variáveis

eval_var :: [ Char ] -> ( [ Tup ] -> Bool )
eval_var ( '~':xs ) = my_neg2 ( eval_var xs )
    where
    my_neg2 :: ( [ Tup ] -> Bool ) -> ( [ Tup ] -> Bool )
    my_neg2 f = g
        where g x = not ( f x )

eval_var [ x ] = search x
    where
    search :: Char -> [ Tup ] -> Bool
    search _ [] = error "char inexistennte"
    search c ( ( c1 , b1 ):ts )
        | c == c1 = b1
        | otherwise = search c ts

eval_var _ = error "token invalido para variável"

--Transforma uma arvore sintatica em uma arvore semântica

evaluate_tree :: Tree -> SemTree
evaluate_tree ( No c t1 t2 ) = SemNo f left right
    where
    f = eval_formula c
    left = evaluate_tree t1
    right = evaluate_tree t2

evaluate_tree ( Folha c ) = SemFolha ( eval_var c )


--- transforma a arvore semantica em uma função

to_fun :: SemTree -> ( [Tup] -> Bool )
to_fun tree = foo tree
    where
    foo :: SemTree -> [ Tup ] -> Bool
    foo ( SemNo f t1 t2 ) tup = f ( foo t1 tup ) ( foo t2 tup )
    foo ( SemFolha f ) tup = f tup
 
eval :: SemTree -> [Tup] -> [Bool] 
eval ( SemFolha _ ) _ = []
eval ( SemNo f ( SemFolha f1 ) ( SemFolha f2 ) ) tup = [ f x y ]
    where
    x = f1 tup
    y = f2 tup
eval ( SemNo f ( SemFolha f1 ) t2 ) tup = lft ++ [ f x1 ( last lft ) ]
    where
    x1 = f1 tup
    lft = eval t2 tup
eval ( SemNo f t1 ( SemFolha f2 ) ) tup = rgh ++ [ f ( last rgh ) x2 ]
    where
    x2 = f2 tup
    rgh = eval t1 tup
eval ( SemNo f t1 t2 ) tup = lft ++ rgh ++ [ f ( last lft ) ( last rgh ) ]
    where
    lft = eval t1 tup
    rgh = eval t2 tup


