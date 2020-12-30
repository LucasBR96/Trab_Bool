module Symbols(
    e_valido,
    operadores,
    Tree(..),
    SemTree(..),
    Tup
)where
import Data.Char ( isLower )

operadores :: [ Char ]
operadores = "+>*"

e_valido :: Char -> Bool
e_valido '~' = True
e_valido x = ( isLower x ) || ( elem x operadores )

--arvore sintática
data Tree = No [ Char ] Tree Tree | Folha [ Char ]
    deriving ( Show , Read , Eq )

type Tup = ( Char , Bool )

data SemTree = SemNo ( Bool -> Bool -> Bool ) SemTree SemTree|
                SemFolha ( [Tup] -> Bool )

