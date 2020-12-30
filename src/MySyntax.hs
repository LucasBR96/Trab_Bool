module MySyntax(
    is_formula,
    is_op,
    to_string,
    tree_build
)where

import Symbols( operadores , Tree(..)  )
import Data.Char( isLower )
import Data.List( intercalate , concat )

--Funções auxiliares de tree_build-----------------------------------------------------------------
--------------------------------------------------------------------------------------------------
is_formula :: [ Char ] -> Bool
is_formula [ x ] = isLower x
is_formula ( '~':xs ) = is_formula xs
is_formula _ = False

is_op :: [ Char ] -> Bool
is_op [ x ] = elem x operadores
is_op ( '~':xs ) = is_op xs
is_op _ = False

add_formula :: [Char] -> [ Tree ] -> [ Tree ]
add_formula x stack = ( to_folha x ):stack
    where
    to_folha :: [ Char ] -> Tree
    to_folha arr = Folha arr

add_no :: [ Char ] -> [ Tree ] -> [ Tree ]
add_no _ [] = error " Menos de duas fórmulas fechadas "
add_no x [ _ ] = add_no x []   
add_no x ( t1:t2:xs ) = ( to_no x t1 t2 ):xs
    where
    to_no :: [ Char ] -> Tree -> Tree -> Tree
    to_no arr tr1 tr2 = No arr tr1 tr2
------------------------------------------------------------------------------------------

-- Constrói a arvore sintática a partir dos tokens
tree_build :: [ [ Char ] ] -> Tree
--tree_build [] = error" Lista vazia! "
tree_build tokens = foo [] rev
    where
    
    rev = reverse tokens

    foo :: [ Tree ] -> [ [ Char ] ] -> Tree
    foo [ x ] [] = x
    foo _ [] = error "cant make a whole tree"
    foo trees ( x:xs )
        | ( is_formula x ) = foo ( add_formula x trees ) xs
        | ( is_op x ) = foo ( add_no x trees ) xs
        | otherwise = error "unwanted type"

--representação da arvore
to_string :: Tree -> [ [ Char ] ]
to_string arv = foo arv
    where
    foo :: Tree -> [ [ Char ] ]
    foo ( No c ( Folha c1 ) ( Folha c2 ) ) = [ c ++ c1 ++ c2 ]
    foo ( No c ( Folha c1 ) t2 ) = lst_2 ++ [ c ++ c1 ++ ( last lst_2 )  ]
        where lst_2 = foo t2
    foo ( No c t1 ( Folha c2 ) ) = lst_1++ [ c ++ ( last lst_1 ) ++ c2  ]
        where lst_1 = foo t1
    foo ( No c t1 t2 ) = lst_1 ++ lst_2 ++ [ c ++ ( last lst_1 ) ++ ( last lst_2 ) ]
        where 
        lst_1 = foo t1
        lst_2 = foo t2