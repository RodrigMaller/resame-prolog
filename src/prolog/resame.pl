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
    solve(Same, Moves),
    print(Same, Moves).
    
print(Same, [pos(X, Y) | Moves]) :-
    write(X),
    write(' '), 
    write(Y),
    write('\n'),
    write('\n'),
    group(Same, pos(X, Y), Group),
    ColumnsRealSize = length(last(Same, X)),
    SameRealSize = length(Same),
    remove_group(Same, Group, NewSame),
    zero_same(NewSame, SameRealSize, ColumnsRealSize, ZeroSame),
    write_matrix(ZeroSame),
    write('\n'),
    write('\n'),
    print(NewSame, Moves).
    
zero_same(Same, SameRealSize, ColumnsRealSize, ZeroSame) :-
    zero_rows_same(Same, 0, ColumnsRealSize, NewSame),
    length(Same, SameSize),
    zero_columns_same(NewSame, SameSize, SameRealSize, ColumnsRealSize, ZeroSame).

zero_rows_same(Same, X, Y, Same) :-
    X == Y - 1.  
zero_rows_same([Column | OtherColumns], Col, ColumnsRealSize, ZeroSame) :-
    length(Column, ColSize),
    zero_rows_column(Column, ColSize, ColumnsRealSize, NewColumn),
    nth0(Col, [Column | OtherColumns], _, NewSame),
    nth0(Col, ZeroSame, NewColumn, NewSame),
    zero_rows_same(OtherColumns, ColumnsRealSize, ZeroSame).
    
zero_rows_column(Column, X, X, Column).
zero_rows_column(Column, ColSize, ColumnsRealSize, NewColumn) :-
    append(Column, [0], OtherColumn),
    NewColSize is ColSize + 1,
    zero_rows_column(OtherColumn, NewColSize, ColumnsRealSize, NewColumn).

zero_columns_same(Same, X, X, _, Same).
zero_columns_same(Same, SameSize, SameRealSize, ColumnsRealSize, ZeroSame) :-
    zero_rows_column([], 0, ColumnsRealSize, ZeroColumn),
    append(Same, ZeroColumn, OtherSame),
    NewSameSize is SameSize + 1,
    zero_columns_same(OtherSame, NewSameSize, SameRealSize, ColumnsRealSize, ZeroSame).
    

%% solve(+Same, -Moves) is nondet
%
%  Verdadeiro se Moves é uma sequência de jogadas (lista de posições) que
%  quando realizadas ("clicadas") resolvem o jogo Same.
%  Este predicado não tem teste de unidade. Ele é testado pelo testador.

solve([], []).
solve(Same, [M | Moves]) :-
    group(Same, Group),
    remove_group(Same, Group, NewSame),
    [M | _] = Group,
    solve(NewSame, Moves), !.

%% group(+Same, ?Group) is nondet
%
%  Verdadeiro se Group é um grupo de Same. Group é uma lista de posições
%  (estrutura pos(lin,col)). Este predicado é não determinístico e deve ser
%  capaz de gerar todos os grupos de Same. Este predicado não deve gerar grupos
%  repetidos. Este predicado e group/3 para vão utilizar os mesmos precicados
%  auxiliares.

group(Same, Group) :-
    findall(pos(X, Y), valid_pos(Same, pos(X, Y)), AllValidPos),
    all_groups(Same, AllValidPos, AllGroups),
    member(Group, AllGroups).
    
all_groups(_, [], []).

all_groups(Same, [pos(X, Y) | T], [NewGroup | OtherGroups]) :-
    %writeln(T),
    group(Same, pos(X, Y), NewGroup),
    %writeln(NewGroup),
    subtract_list(T, NewGroup, NewValids),
    %writeln(NewValids),
    all_groups(Same, NewValids, OtherGroups).
    
subtract_list(List1, List2, ListResp) :-
    findall(N, (member(N, List1), not(member(N, List2))), ListResp).
    
valid_pos(Same, pos(X, Y)) :-
    color(Same, pos(X, Y), Color),
    Color \= 0,
    same_color_neighbors_list(Same, pos(X, Y), [], Ns),
    Ns \= [].

%% grupo(+Same, +P, -Group) is semidet
%
%  Verdadeiro se Group é um grupo de Same que contém a posição P.

group(Same, P, Group) :-
    Vs = [P],
    nhood(Same,Vs,Vs,SubGroup),
    SubGroup \== [],
    append(Vs,SubGroup,Group).
    
nhood(_, [], _, []).

nhood(Same,[P|T],Vs,Group) :-
    same_color_neighbors_list(Same,P,Vs,Ns),
    append(Vs,Ns,NewVs),    
    append(T,Ns,NewT),
    nhood(Same,NewT,NewVs,SubGroup),
    append(Ns,SubGroup,Group).

same_color_neighbors(Same,P,N) :-
    neighbors(P,N),
    color(Same,P,C),
    color(Same,N,C).

same_color_neighbors_list(Same,P,Vs,Ns) :-
    findall(N,(same_color_neighbors(Same,P,N),not(member(N,Vs))), Ns).  

color(Same,pos(X,Y),Color) :-
    nth0(Y,Same,Column),
    nth0(X,Column,Color).

%left right
neighbors(pos(X0,Y0), pos(X1,Y1)):-
    neighborsXY(X0,X1),
    Y0 = Y1.

%up down
neighbors(pos(X0,Y0),pos(X1,Y1)):-
    X0 = X1,    
    neighborsXY(Y0,Y1).    

neighborsXY(C0,C1):-
    C1 is C0 -1; %down and left
    C1 is C0 +1. %up and right

%% remove_group(+Same, +Group, -NewSame) is semidet
%
%  Verdadeiro se NewSame é obtido de Same removendo os elemento especificados
%  em Group. A remoção é feita de acordo com as regras do jogo same.
%  Dica:
%    - crie um predicado auxiliar remove_column_group, que remove os elementos
%    de uma coluna específica

remove_group(Same, Group, NewSame) :-
    length(Same, SameSize),
    NumCol is SameSize - 1,
    remove_column_group(Same, NumCol, Group, NSame),
    reverse(NSame, NewSame).

%% remove_column_group(+Same, +Column, +Group, -NewSame) is semidet
%
%  Verdadeiro se NewSame � obtido de Same removendo os elementos
%  especificados em Group e que est�o na Column.

remove_column_group(_, -1, _, []).

remove_column_group(Same, Col, Group, NewSame) :-
    findall(X, member(pos(X, Col),  Group), ListRow),
    nth0(Col, Same, Column),
    dec_sort(ListRow, DecListRow),
    remove_rows_column(Column, DecListRow, NewColumn),
    NewColumn = [],
    NextCol is Col -1,
    remove_column_group(Same, NextCol, Group, NewSame), !.

remove_column_group(Same, Col, Group, [NewColumn | RestColumn]) :-
    findall(X, member(pos(X, Col), Group), ListRow),
    nth0(Col, Same, Column),
    dec_sort(ListRow, DecListRow),
    remove_rows_column(Column, DecListRow, NewColumn),
    NewColumn \= [],
    NextCol is Col - 1,
    remove_column_group(Same, NextCol, Group, RestColumn), !.

dec_sort(List, DecList) :-
    sort(List, SortedList),
    reverse(SortedList, DecList).

%% remove_rows_column(+Column, +Rows, -NewColumn) is semidet
%
%  Verdadeiro se NewColumn � obtido de Column removendo os elementos
%  de Rows.

remove_rows_column(Column, [], Column).

remove_rows_column(Column, [Row | Rest], NewColumn) :-
    nth0(Row, Column, _, OtherColumn),
    remove_rows_column(OtherColumn, Rest, NewColumn), !.


