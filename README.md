# Trab_Bool
College Homework about Truth tables and functional programming

The goal of this project is to create a program that reads a boolean formula
and returns its thruth table for it and for its subformulas. All using functional
programming.

Example:

>>> ~a -> (b & c)

a | b | c | ~a | b & c | ~a -> (b & c)
V | V | V | F  | V     | V
V | V | F | F  | F     | V
V | F | V | F  | F     | V
V | F | F | F  | F     | V
F | V | V | V  | V     | V
F | V | F | V  | F     | F
F | F | V | V  | F     | F
F | F | F | V  | F     | F

Also, it classifies the formula as:

    Redundant -> all inputs return true
    Contradictory -> all inputs return false
    Possible -> At least one input returns true
