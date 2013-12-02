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
    read_matrix_file(File, M),
    transpose(M, Same),
    solve(Same, Moves),
    last(Same, Column),
    length(Column, ColumnsRealSize),
    length(Same, SameRealSize),
    sim(Same, SameRealSize, ColumnsRealSize, Moves).

%% sim(?Same, +SameRealSize, +ColumnsRealSize, ?Moves) is det
%
%  Simula a resolu��o do jogo passo a passo e imprime.
   
sim([], _, _, []).
sim(Same, SameRealSize, ColumnsRealSize, [pos(X, Y) | Moves]) :-
    group(Same, pos(X, Y), Group),
    remove_group(Same, Group, NewSame),
    zero_same(NewSame, SameRealSize, ColumnsRealSize, ZeroSame),
    transpose(ZeroSame, MatrixSame),
    print(MatrixSame, pos(X, Y)),
    sim(NewSame, SameRealSize, ColumnsRealSize, Moves), !.
    

%% print(+Same, +Moves) is det
%
%  Escreve na tela cada Move realizado e como o Same fica depois desse Move.

print(_, []).
print(Same, pos(X, Y)) :-
    write(X), put_char(' '), write(Y),
    writeln('\n'),
    write_matrix(Same),
    write('\n').

%% zero_same(+Same, +SameRealSize, +ColumnsRealSize, -ZeroSame) is det
%
%  Verdadeiro se ZeroSame � o Same com as linhas e colunas completas com zeros.
    
zero_same(Same, SameRealSize, ColumnsRealSize, ZeroSame) :-
    zero_rows_same(Same, ColumnsRealSize, NewSame),
    length(Same, SameSize),
    ExSize is SameRealSize - SameSize,
    zero_column(ColumnsRealSize, ZeroColumn),
    zero_columns_same(ExSize, ZeroColumn, ZeroColumns),
    append(NewSame, ZeroColumns, ZeroSame).

%% zero_columns_same(+ExSize, +ZeroColumn, ?ZeroColumns) is semidet
%
%  Verdadeiro se ZeroColumns � uma lista de colunas zeradas de tamanho ExSize.

zero_columns_same(0, _, []).
zero_columns_same(ExSize, ZeroColumn, [H | T]) :-
    H = ZeroColumn,
    NewExSize is ExSize - 1,
    zero_columns_same(NewExSize, ZeroColumn, T), !.

%% zero_column(+NumZeros, ?ZeroColumn) is semidet
%
%  Verdadeiro se ZeroColumn � uma coluna de zeros de tamanho NumZeros.
    
zero_column(0, []).
zero_column(NumZeros, [H | T]) :-
    H is 0,
    NewNumZeros is NumZeros - 1,
    zero_column(NewNumZeros, T), !.  

%% zero_rows_same(?Same, +ColumnsRealSize, ?NewSame) is semidet
%
%  Verdadeiro se NewSame � um novo same com as linhas completadas com zeros.

zero_rows_same([], _, _).
zero_rows_same([Column | OtherColumns], ColumnsRealSize, [NewColumn | OtherNewColumns]) :-
    length(Column, ColSize),
    ExColSize is ColumnsRealSize - ColSize,
    zero_column(ExColSize, ZeroColumn),
    append(Column, ZeroColumn, NewColumn),
    zero_rows_same(OtherColumns, ColumnsRealSize, OtherNewColumns), !.

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

%% all_groups(+Same, ?AllValidPos, -AllGroups) is semidet
%
%  Verdadeiro se AllGroups � uma lista com todos grupos poss�veis de Same
    
all_groups(_, [], []).

all_groups(Same, [pos(X, Y) | T], [NewGroup | OtherGroups]) :-
    %writeln(T),
    group(Same, pos(X, Y), NewGroup),
    %writeln(NewGroup),
    subtract_list(T, NewGroup, NewValids),
    %writeln(NewValids),
    all_groups(Same, NewValids, OtherGroups).
   
%% subtract_list(+List1, +List2, -ListResp) is semidet
%
%  Verdadeiro se ListResp � a List1 menos a List2.

subtract_list([], List, List).

subtract_list(List, [], List).
 
subtract_list(List1, List2, ListResp) :-
    findall(N, (member(N, List1), not(member(N, List2))), ListResp).
  
%% valid_pos(+Same, ?Pos) is nondet
%
%  Verdadeiro se Pos � uma posi��o valida dentro de Same. Gera todas
%  posi��es validas de Same.
    
valid_pos(Same, pos(X, Y)) :-
    color(Same, pos(X, Y), Color),
    Color \= 0,
    same_color_neighbors_list(Same, pos(X, Y), [], Ns),
    Ns \= [].

%% grupo(+Same, +P, -Group) is semidet
%
%  Verdadeiro se Group é um grupo de Same que contém a posição P.

group(Same, P, Group) :-
    nhood(Same, [P], [P], SubGroup),
    SubGroup \== [],
    append([P], SubGroup, Group).
   
%% nhood(+Same, +ListToVisitPos, ?ListVisitedPos, -Group) is semidet
%
%  Verdadeiro se Group � um grupo de Same da vizinhan�a ListToVisitPos.
    
nhood(_, [], _, []).

nhood(Same, [P | T], Vs, Group) :-
    same_color_neighbors_list(Same,P,Vs,Ns),
    append(Vs, Ns, NewVs),    
    append(T, Ns, NewT),
    nhood(Same, NewT, NewVs, SubGroup),
    append(Ns, SubGroup, Group).

%% same_color_neighbors(+Same, +P, ?N) is nondet
%
%  Verdadeiro se N � Vizinho e tem a mesma cor de P. Gera todos vizinhos
%  de mesma cor N de P.

same_color_neighbors(Same, P, N) :-
    neighbors(P, N),
    color(Same, P, C),
    color(Same, N, C).

%% same_color_neighbors_list(+Same, +P, +Vs, -Ns) is semidet
%
%  Verdadeiro se Ns � a lista de vizinhos de mesma cor de P que
%  n�o s�o membros de Vs.

same_color_neighbors_list(Same, P, Vs, Ns) :-
    findall(N, (same_color_neighbors(Same, P, N), not(member(N, Vs))), Ns).

%% color(+Same, +Pos, ?Color) is semidet
%
%  Verdadeiro se Color � a cor de Pos no Same. Devolve a cor Color de Pos.

color(Same, pos(X, Y), Color) :-
    nth0(Y, Same, Column),
    nth0(X, Column, Color).

%% neighbors(+Pos, ?Vizinho) is semidet
%
%  Verdadeiro se Vizinho � um vizinho de Pos. Devolve um Vizinho de Pos.

%left right
neighbors(pos(X0, Y0), pos(X1, Y1)):-
    neighborsXY(X0, X1),
    Y0 = Y1.

%up down
neighbors(pos(X0, Y0), pos(X1, Y1)):-
    X0 = X1,    
    neighborsXY(Y0, Y1).    

%% neighborsXY(+C0, -C1) is det
%
%  Verdadeiro se C1 � uma unidade menor ou maior que C0. Devolve C1 igual a
%  C0 + 1 ou C0 - 1.

neighborsXY(C0, C1):-
    C1 is C0 - 1; %down and left
    C1 is C0 + 1. %up and right

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

%% dec_sort(+List, -DecList) is det
%
%  Verdadeiro se DecList � a List ordenada em ordem decrescente. Devolve DecList
%  igual a List ordenada em ordem decrescente.

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


