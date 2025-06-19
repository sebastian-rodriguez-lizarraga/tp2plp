:- use_module(piezas).



% EJERCICIO 1: 

% L = P1 | R | ...
% P1 | R es prefijo de L de longitud Descartar + Tomar COMENTAR MEJOR 

% prefijo(+L,?P)
prefijo(L,P) :- append(P,_,L).

% sublista(+Descartar, +Tomar, +L, -R)  
sublista(Descartar, Tomar, L, R) :- prefijo(L,P), length(P, Descartar), append(P, R, N), prefijo(L,N), length(R, Tomar). 

%Es reversible en Descartar, ya que como L sí está instanciado, se puede calcular el prefijo P y entonces 
%por mas que no se pase Descartar instanciado, al hacer length(P, Descartar) ya se instancia y funciona de la 
%misma manera que si se lo hubiese pasado instanciado.  
%Tambien es reversible en R, pudiendolo pasar ya instanciado, ya que al hacer append(P, R, N) se puede instanciar bien N,
%ya que tanto P como R estan instanciados en ese punto. Ademas tambien se puede calcular bien length(R, Tomar) ya que R esta instanciado. 
%Por ultimo, Tomar puede estar o no instanciado, ya que como R lo esta, length(R, Tomar) da true o false si Tomar esta instanciado, y sino lo instancia en 
%la longitud de R, que si esta instanciado. 


% EJERCICIO 2: 

% tablero(+K, -T) 
tablero(N, [A,B,C,D,E]):- length(A,N), length(B,N), length(C,N), length(D,N), length(E,N).



% EJERCICIO 3: 

%dimension(+M, -F, -C)
dimension([H|T], F, C) :- length([H|T],F), length(H, C).



% EJERCICIO 4: 
%coordenadas(+T, -IJ)
coordenadas(T, (I,J)) :- dimension(T, F, C), between(1, F, I), between(1, C, J). 



% EJERCICIO 5: 
%kPiezas(+K, -PS) 
kPiezas(K, PS) :- nombrePiezas(N), kPiezasAux(K, PS, N).

%kPiezasAux(+K, -PS, +L)
kPiezasAux(0, [], L).
kPiezasAux(K, [X|XS], [X|YS]) :- length([X|YS],Z), K =< Z, N is K-1, kPiezasAux(N, XS, YS). 
kPiezasAux(K, [X|XS], [Y|YS]) :- length([Y|YS],Z), K =< Z, kPiezasAux(K, [X|XS], YS), X \= Y. 



%  EJERCICIO 6
%seccionTablero(+T,+ALTO,+ANCHO,+IJ,?ST)
seccionTablero(_,0,_,_,[]).
seccionTablero([F|T], Al, An, (I,J), YS) :- I > 1, I1 is I-1, seccionTablero(T, Al, An, (I1,J), YS).
seccionTablero([F|T], Al, An, (I,J), [Y|YS]) :- I=:=1, K is J-1, sublista(K, An, F, Y), V is Al-1, seccionTablero(T,V,An,(I,J), YS).



% EJERCICIO 7 
%ubicarPieza(+Tablero, +Identificador)
ubicarPieza(T, E) :- pieza(E,P), dimension(P,F,C), dimension(T, FT, CT), between(1,FT, I), between(1, CT, J), seccionTablero(T, F, C, (I,J), P).



% EJERCICIO 8

%poda(+Poda,+Tablero)
poda(sinPoda, _).
poda(podaMod5, T) :- todosGruposLibresModulo5(T).

% preguntar si cambia algo el orden de los identificadores
 %ubicarPiezas(+Tablero, +Poda, +Identificadores)
 ubicarPiezas(T,P,[]).
 ubicarPiezas(T,P,[H|T2]) :- poda(P, T), ubicarPieza(T,H), ubicarPiezas(T,P,T2).



% EJERCICIO 9: 
%llenarTablero(+Poda, +Columnas, -Tablero).
llenarTablero(P,C,T) :- kPiezas(C,PS), tablero(C,T), ubicarPiezas(T, P, PS).



% EJERCICIO 10: 
cantSoluciones(Poda, Columnas, N) :-
findall(T, llenarTablero(Poda, Columnas, T), TS),
length(TS, N). 

%Cantidad de soluciones para 3 columnas sin poda
%time(cantSoluciones(sinPoda, 3, N)).
% 28,086,428 inferences, 2.021 CPU in 2.021 seconds (100% CPU, 13898904 Lips)
%N = 28.

%Cantidad de soluciones para 4 columnas sin poda
%time(cantSoluciones(sinPoda, 4, N)).
% 1,182,111,798 inferences, 87.434 CPU in 87.453 seconds (100% CPU, 13520041 Lips)
%N = 200.



%EJERCICIO 11

%todosGruposLibresModulo5(+Tablero)
todosGruposLibresModulo5(T) :- coordenadasLibres(T, LC), agrupar(LC, G), listasModulo5(G).

%listasModulo5(+L)
listasModulo5(L) :- forall(member(X, L), (length(X, N), N mod 5 =:= 0)).

% PREGUNTAR SI SE PUEDE USAR IESIMO, QUE USA APPEND QUE ES RECURSIVO 
%iesimo(+K, +LS, -X)
iesimo(K, LS, X) :- append(P, [X|_], LS), length(P, K).

%estaLibre(+(I,J), +T)
estaLibre((I,J), T) :- I2 is I-1, J2 is J-1, iesimo(I2,T,X), iesimo(J2,X,K), var(K). 

%coordenadasLibres(+T, LC)
coordenadasLibres(T,LC) :- findall(IJ, (coordenadas(T, IJ), estaLibre(IJ,T)), LC). 


%Cantidad de soluciones para 3 columnas con poda mod 5
% ?- time(cantSoluciones(podaMod5, 3, N)).
% 17,038,737 inferences, 1.158 CPU in 1.158 seconds (100% CPU, 14720090 Lips)
% N = 28.

%Cantidad de soluciones para 4 columnas con poda mod 5
% ?- time(cantSoluciones(podaMod5, 4, N)).
% 394,997,584 inferences, 26.745 CPU in 26.754 seconds (100% CPU, 14769094 Lips)
% N = 200.



%EJERCICIO 12 
% sublista(-Descartar, ?Tomar, +L, +R)  
% sublista(Descartar, Tomar, L, R) :- prefijo(L,P), length(P, Descartar), append(P, R, N), prefijo(L,N), length(R, Tomar). 