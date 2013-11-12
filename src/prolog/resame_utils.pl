% vim: set ft=prolog:
%
% Este arquivo contém predicados e funções auxiliares que você pode achar útil.
%
% Não é necessário alterar este arquivo.

:- use_module(library(readutil)).

%% read_matrix_file(+File, -M) is semidet
%
%  Lê para M uma matriz de números do arquivo File.

read_matrix_file(File, M) :-
    read_file_to_codes(File, In0, []),
    append(In1, [10], In0), % remove o último \n
    atom_codes(InputAtom, In1),
    atomic_list_concat(Lines, '\n', InputAtom),
    maplist(atom_numbers, Lines, T),
    reverse(T, M), !.

atom_numbers(Atom, Numbers) :-
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, Numbers).

%% write_matrix(M) is det
%
%  Escreve na saída padrão a matriz M. Cada linha é escrita com a função
%  write_list/1.

write_matrix(M) :-
    reverse(M, R),
    maplist(write_list, R).

%% write_list(+A) is det
%
%  Escreve na saída padrão todos os elementos da lista A separados por espaço.

write_list([X]) :-
    write(X), put_char('\n'), !.
write_list([X|Xs]) :-
    write(X), put_char(' '),
    write_list(Xs).

%% transpose(?M, ?T) is semidet
%
%  Verdadeiro se T é a matriz transposta de M.

transpose(M, []) :-
    maplist(empty, M), !.
transpose(M, [Hs|TTs]) :-
    maplist(head, M, Hs),
    maplist(tail, M, Ts),
    transpose(Ts, TTs), !.

head([H|_], H).
tail([_|T], T).
empty([]).
