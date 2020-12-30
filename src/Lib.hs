-- module Lib
--     ( generate_tab,
--       generate_header,
--       generate_inputs,
--       evaluate_results,
--       rjust,
--       tab_to_str
--     ) where
    
module Lib ( generate_tab ) where

import Lexicon( spot_var , tokenize )
import Semantics( evaluate_tree , to_fun, eval )
import MySyntax( tree_build , to_string )
import Symbols( Tup, Tree, SemTree)
import Control.Monad( replicateM )
import Data.List( intercalate )


generate_inputs :: [Char] -> [ [Tup] ]
generate_inputs vars = [ zip vars val | val <- truth ]
    where truth = replicateM ( length vars ) [ False , True ]


rjust :: Int -> [ Char ] -> [ Char ]
rjust n str | length str < n = str ++ [ ' ' | _ <- [ 1 .. ( n - length str ) ] ]
            | otherwise = str

expr_to_tree :: [ Char ] -> SemTree
expr_to_tree expr = tree
    where
    tok = tokenize expr
    tr = tree_build tok
    tree = evaluate_tree tr

all_tab :: [ Char ] -> [ [ Bool ] ]
all_tab arr = [ ( bool_inpt tup ) ++ ( foo tup ) | tup <- generate_inputs vars ]
    where
    bool_inpt :: [ Tup ] -> [ Bool ]
    bool_inpt tups = [ y | ( _ , y ) <- tups]

    foo = eval( expr_to_tree arr )

    vars = spot_var arr

tab_to_str :: Int -> [ [ Bool ] ] -> [ Char ]
tab_to_str n tab = intercalate "\n" [ foo1 [ foo2 x | x <- line] | line <- tab ]
    where
    foo2 :: Bool -> [ Char ]
    foo2 False = ( rjust n "F" ) 
    foo2 _ = ( rjust n "V" )

    foo1 arr = intercalate "|" arr
    
generate_header :: [Char] -> [Char]
generate_header arr = intercalate "|" [ rjust n word | word <- vars ++ subformulas ]
    where
    n = length arr
    vars = [ [x] | x <- spot_var arr]
    subformulas = to_string( tree_build ( tokenize arr ) )

evaluate_results :: [ [ Bool ] ] -> [ Char ]
evaluate_results tab = "Essa formula é uma " ++ foo [ last x | x <- tab ]
    where
    
    foo :: [ Bool ] -> [ Char ]
    foo arr | ( and arr ) = "Tautologia"
            | ( or arr ) = "Satisfativel"
            | otherwise = "Contraditória"

generate_tab :: [Char] -> [Char]
generate_tab cand = intercalate "\n" [ header , body , evaluation ] ++ "\n"
    where
    
    header = generate_header cand
    n = length cand

    tab = all_tab cand
    body = tab_to_str n tab
    evaluation = evaluate_results tab


