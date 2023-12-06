:- module(parsing, [ eol//0
                   , digit//1
                   , digit1//1
                   , digits//1
                   , pos_integer//1
                   , integer//1
                   , many1//1
                   , integer_binary/2
                   ]).

:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(lists)).

eol -->
  "\n".

digit(C) -->
  [C], { char_type(C, decimal_digit) }.

digit1(C) -->
  digit(C), { dif(C, '0') }.

digits([]) -->
  [].
digits([C | Cs]) -->
  digit(C), digits(Cs).

pos_integer(0) -->
  "0".
pos_integer(I) -->
  digit1(C), digits(Cs), { number_chars(I, [C | Cs]) }.

integer(I) -->
  "+", !, pos_integer(I).
integer(I) -->
  "-", !, pos_integer(I0), { I #= -I0 }.
integer(I) -->
  pos_integer(I).

many1(C) -->
  [C], many1_(C).
many1_(_) -->
  [].
many1_(C) -->
  [C], many1_(C).

integer_binary(0, []).
integer_binary(Integer, [Bit | Bits]) :-
  length(Bits, Length),
  Base #= Bit * (2 ^ Length),
  integer_binary(Integer0, Bits),
  Integer #= Base + Integer0.
