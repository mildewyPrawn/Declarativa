%% M-x prolog-mode for emacs :)
%% matriz de adyacencias.
matrix([
              a-b,a-d,a-e,
              b-c,b-d,b-e,b-f,
              c-e,c-f,
              d-e,d-g,d-h,
              e-f,e-g,e-h,e-i,
              f-h,f-i,
              g-h,
              h-i]).

%% Prints the matrix
prettyPrint([],_) :-
    nl,write("---------------"),nl.

prettyPrint([X|Xs], Y) :-
    (Y =:= 3 ->
         nl,write("---------------"),nl, Y1 = 0, prettyPrint([X|Xs], Y1)
    ; write("| "), write(X), write(" |"), Y1 = Y + 1, prettyPrint(Xs, Y1)), nl.
    
%% verifica que dos celdas sean adyacentes.
adyacente(X,Y) :-
    matrix(L),
    (member(X-Y,L);
     member(Y-X,L)).

%% "BGB_GBG_BGB"
%% Reads a string, and print char per char.
genZero(X) :-
    string_to_list_of_characters(X, Y),
    delMember('_',Y,Z),
    prettyPrint(Z,0).
    %% write(Z),nl.

%% Converts a string into a list of chars. 
string_to_list_of_characters(String, Characters) :-
    name(String, Xs),
    maplist(number_to_character,
             Xs, Characters).

%% Map something?
number_to_character(Number, Character) :-
    name(Character, [Number]).

%% Delete all ocurrences of a list. 
delMember(_, [], []).

delMember(X, [X|Xs], Y) :-
    delMember(X, Xs, Y).

delMember(X, [T|Xs], [T|Y]) :-
    dif(X, T),
    delMember(X, Xs, Y).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
