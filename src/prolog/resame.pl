% vim: set ft=prolog:

% Neste arquivo estão especificados os predicados que devem ser implementados.
% Você pode criar predicados auxiliares quando necessário.
%
% No arquivo resame_testes.pl estão os testes para alguns predicados.
%
% Para implementar cada predicado, primeiro você deve ler e entender a
% especificação e o teste.
%
% A especificação dos parâmetros dos predicados segue o formato descrito em
% http://www.swi-prolog.org/pldoc/doc_for?object=section%282,%274.1%27,swi%28%27/doc/Manual/preddesc.html%27%29%29
%
% Um Jogo same é representado por uma lista de colunas, sem os elementos nulos
% (zeros).
% Por exemplo, o jogo
% 2 | 3 0 0 0
% 1 | 2 2 2 0
% 0 | 2 3 3 1
% --+--------
%   | 0 1 2 3
% é representado como [[2, 2, 3], [3, 2], [3, 2], [1]].
% O tamanho deste jogo é 3x4 (linhas x colunas).
%
% Uma posição no jogo é representado por uma estrutura pos com dois argumentos
% (lin, col), onde lin é o número da linha e col é o número da coluna.  No
% exemplo anterior, a posição pos(0, 1) tem cor 3, e a posição pos(1, 2) tem
% cor 2.

% Você pode utilizar os predicados definidos no arquivo resame_utils.pl
:- consult(resame_utils).

%% main(+File) is det
%
%  Carrega um jogo same do arquivo File e imprime uma resolução na saída padrão
%  ou sem-solucao se o jogo não tem solução.

main(File) :-
    read_matriz_file(File, M),
    transpose(M, Same),
    solve(Same, Moves).
    writeln(Moves).

%% solve(+Same, -Moves) is nondet
%
%  Verdadeiro se Moves é uma sequência de jogadas (lista de posições) que
%  quando realizadas ("clicadas") resolvem o jogo Same.
%  Este predicado não tem teste de unidade. Ele é testado pelo testador.

solve([], []).
solve(Same, [M|Moves]) :-
    group(Same, Group),
    remove_group(Same, Group, NewSame),
    [M|_] = Group,
    solve(NewSame, Moves).

%% group(+Same, ?Group) is nondet
%
%  Verdadeiro se Group é um grupo de Same. Group é uma lista de posições
%  (estrutura pos(lin,col)). Este predicado é não determinístico e deve ser
%  capaz de gerar todos os grupos de Same. Este predicado não deve gerar grupos
%  repetidos. Este predicado e group/3 para vão utilizar os mesmos precicados
%  auxiliares.

group(Same, Group) :-
    length(Same, NumLin),
    get_pos(),
    make_groups().

%% grupo(+Same, +P, -Group) is semidet
%
%  Verdadeiro se Group é um grupo de Same que contém a posição P.

group(Same, P, Group) :-
    writeln([Same, P, Group]), fail.

%% remove_group(+Same, +Group, -NewSame) is semidet
%
%  Verdadeiro se NewSame é obtido de Same removendo os elemento especificados
%  em Group. A remoção é feita de acordo com as regras do jogo same.
%  Dica:
%    - crie um predicado auxiliar remove_column_group, que remove os elementos
%    de uma coluna específica

remove_group(Same, Group, NewSame) :-
    length(Same, NumCol),
    remove_column_group(Same, 0, NumCol, Group, NewSame).

%% remove_column_group(+Same, +Column, +NumberOfColumns, +Group, -NewSame) is semidet
%
%  Verdadeiro se NewSame � obtido de Same removendo os elementos
%  especificados em Group e que est�o na em cada elem da ListColumn

remove_column_group(Same, X, X, _, []).

remove_column_group(Same, Col, NumCol, Group, [NewColumn | OtherColumn]) :-
    Col < NumCol,
    findall(X, member(pos(X, Col), Group), ListRows),
    ListRows = [],
    nth0(Col, Same, NewColumn),
    NextCol is Col + 1,
    [NewColumn | _] = [NewColumn],
    remove_column_group(Same, NextCol, NumCol, Group, OtherColumn).

remove_column_group(Same, Col, NumCol, Group, [NewColumn | OtherColumn]) :-
    Col < NumCol,
    findall(X, member(pos(X, Col), Group), ListRows),
    ListRows \= [],
    nth0(Col, Same, Column),
    remove_row_column(Column, ListRows, NewColumn),
    NewColumn \= [],
    [NewColumn | _] = [NewColumn],
    NextCol is Col + 1,
    remove_column_group(Same, NextCol, NumCol, Group, OtherColumn).
    

%% remove_row_column(+Column, +ListRow, -NewColumn) is semidet
%  
%  Verdadeiro se NewColumn � obtido de Column removendo o elemento
%  especificado em Row da Column

remove_row_column(Column, [], Column).

remove_row_column(Column, [Row | Rest], NewColumn) :-
    selectchk(Row, Column, NewColumn),
    remove_pos(NewColumn, Rest, RecursiveNewColumn).
    

