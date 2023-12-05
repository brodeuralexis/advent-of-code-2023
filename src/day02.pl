:- module(day02, [day02/2]).

:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(reif)).
:- use_module(parsing).

game_id(game(Id, _), Id).
game_rounds(game(_, Rounds), Rounds).

color(red) --> "red".
color(blue) --> "blue".
color(green) --> "green".

cubes([Count-Color | Cubes]) -->
  pos_integer(Count), " ", color(Color), ", ", cubes(Cubes).
cubes([Count-Color]) -->
  pos_integer(Count), " ", color(Color).

rounds([Round]) -->
  cubes(Round).
rounds([Round | Rounds]) -->
  cubes(Round), "; ", rounds(Rounds).

game(game(Id, Rounds)) -->
  "Game ", pos_integer(Id),": ", rounds(Rounds), "\n".

games([]) -->
  [].
games([Game | Games]) -->
  game(Game), games(Games).

possibleRound(Round) :-
  (member(RedCount-red, Round) -> RedCount #=< 12; true),
  (member(GreenCount-green, Round) -> GreenCount #=< 13; true),
  (member(BlueCount-blue, Round) -> BlueCount #=< 14; true).

possibleGame(game(_, Rounds)) :-
  maplist(possibleRound, Rounds).

round_red_green_blue(Round, Red, Green, Blue) :-
  (member(R-red, Round) -> Red #= R; Red #= 0),
  (member(G-green, Round) -> Green #= G; Green #= 0),
  (member(B-blue, Round) -> Blue #= B; Blue #= 0).

game_minCubes(game(_, Rounds), [R, G, B]) :-
  maplist(round_red_green_blue, Rounds, Reds, Greens, Blues),
  list_max(Reds, R), list_max(Greens, G), list_max(Blues, B).

day02(Part1, Part2) :-
  phrase_from_file(games(Games), "../data/day02.txt"),
  part1(Games, Part1),
  part2(Games, Part2).

part1(Games, Part1) :-
  tfilter(\G^B^(possibleGame(G) -> B = true; B = false), Games, PossibleGames),
  maplist(game_id, PossibleGames, Ids),
  sum_list(Ids, Part1).

prod_(L, S0, S) :- S #= S0 * L.
prod_list(Ls, S) :- foldl(prod_, Ls, 1, S).

part2(Games, Part2) :-
  maplist(game_minCubes, Games, MinCubes),
  maplist(prod_list, MinCubes, Powers),
  sum_list(Powers, Part2).
