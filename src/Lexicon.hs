module Lexicon(
    tokenize,
    spot_var
)where

import Symbols( e_valido , operadores )
import Data.List( nub , sort )
import Data.Char( isLower )


clean :: [Char] -> [ Char ]
clean arr = filter e_valido arr

tokenize :: [ Char ] -> [ [ Char ] ]
tokenize [] = []
tokenize str = foo cl_arr
    where
    cl_arr = clean str

    foo :: [ Char ] -> [ [ Char ] ]
    foo [] = []
    foo [x] = [ [x] ] 
    foo ( '~':'~':xs ) = foo xs
    foo ( '~':x:xs ) = [ '~', x ]:foo xs
    foo ( x:xs ) = [ x ]:( foo xs )

spot_var :: [ Char ] -> [ Char ]
spot_var str = sort ( nub [ x | x <- str , isLower x ])

