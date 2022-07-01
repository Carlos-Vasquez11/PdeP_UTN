viveEnLaMasionDreadBurt(tiaAgatha).
viveEnLaMasionDreadBurt(elMayordomo).
viveEnLaMasionDreadBurt(charles).

estaEnLaMansion(milHouse).
estaEnLaMansion(Persona) :-
    viveEnLaMasionDreadBurt(Persona).



 odiaA(tiaAgatha, LeOdiaA) :-
    viveEnLaMasionDreadBurt(LeOdiaA),
    LeOdiaA \= elMayordomo.
%Supongo que se odia a si misma

odiaA(elMayordomo, LeOdiaA) :-
    odiaA(tiaAgatha,LeOdiaA).

odiaA(charles, LeOdiaA) :-
    estaEnLaMansion(LeOdiaA),
    not(odiaA(tiaAgatha,LeOdiaA)).

esMasRicoQueAgatha(UnaPersona) :-
    viveEnLaMasionDreadBurt(UnaPersona),
    not(odiaA(elMayordomo,UnaPersona)).

quienMata(Victima,Asesino) :-
    viveEnLaMasionDreadBurt(Asesino),
    odiaA(Asesino,Victima),
    esMasRicoQueAgatha(Asesino).

% 1.
% a. Lo resuelve
% b. quienMata(tiaAgatha,Asesino).
% 
% 2.
% a.
% - ¿Quien no odia a Milhouse?. Charles lo hace.
% - Charles odia a milHouse y al Mayodormo. 
% - El Mayordomo y ella misma odian a la Tia Agatha 
% - Lista de Odiadiores y Odiados:
% Odiador = tiaAgatha:
% Odiado = charles 
% 
% Odiador = elMayordomo:
% Odiado = tiaAgatha
% Odiado = charles
% 
% Odiador = charles:
% Odiado = milHouse
% Odiado = elMayordomo
%
% - Verdadero
%
% b.
%  odiaA(Odiador, milHouse).
%  odiaA(charles, OdiaA).
%  odiaA(Odiador, tiaAgatha).
%  odiaA(Odiador, Odiado).
%  odiaA(elMayordomo,_).