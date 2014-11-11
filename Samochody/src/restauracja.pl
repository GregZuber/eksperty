:- module(se2,[wybierz]).

:- dynamic([xpozytywne/2, xnegatywne/2]).

danie_to(spaghetti) :-
	pozytywne(masz, duzo_czasu),
	negatywne(jestes_na, diecie),
	pozytywne(jesz, mieso).

danie_to(herbata) :-
	negatywne(masz, duzo_czasu),
	pozytywne(jestes_na, diecie),
	negatywne(jesz, mieso).

danie_to(kurczak_slodko_kwasny_z_orzeszkami) :-
	pozytywne(masz, duzo_czasu),
	negatywne(jestes_na, diecie),
	pozytywne(jesz, mieso).

danie_to(schabowy_z_ziemniakami) :-
	pozytywne(masz, duzo_czasu),
	negatywne(jestes_na, diecie),
	pozytywne(jesz, mieso).

danie_to(wino) :-
	negatywne(masz, duzo_czasu),
	pozytywne(jestes_na, diecie),
	negatywne(jesz, mieso).

danie_to(golonka) :-
	pozytywne(masz, duzo_czasu),
	negatywne(jestes_na, diecie),
	pozytywne(jesz, mieso).

danie_to(jajecznica) :-
	negatywne(masz, duzo_czasu),
	negatywne(jestes_na, diecie),
	negatywne(jesz, mieso).

danie_to(jogurt_z_dietetycznymi_platkami) :-
	negatywne(masz, duzo_czasu),
	pozytywne(jestes_na, diecie),
	negatywne(jesz, mieso).

danie_to(leczo) :-
	pozytywne(masz, duzo_czasu),
	negatywne(jestes_na, diecie),
	pozytywne(jesz, mieso).

danie_to(owoce_morza) :-
	pozytywne(masz, duzo_czasu),
	negatywne(jestes_na, diecie),
	pozytywne(jesz, mieso).
	
danie_to(kawior) :-
	pozytywne(masz, duzo_czasu),
	negatywne(jestes_na, diecie),
	pozytywne(jesz, mieso).

pozytywne(X, Y) :-
	xpozytywne(X, Y), !.

pozytywne(X, Y) :-
	not(xnegatywne(X, Y)),
	pytaj(X, Y, tak).

negatywne(X, Y) :-
	xnegatywne(X, Y), !.

negatywne(X, Y) :-
	not(xpozytywne(X, Y)),
	pytaj(X, Y, nie).

pytaj(X, Y, tak) :-
	!, write(X), write(' '), write(Y), write(' ? (t/n)\n'),
	readln([Replay]),
	pamietaj(X, Y, Replay), 
	odpowiedz(Replay, tak).


pytaj(X, Y, nie) :-
	!, write(X), write(' '), write(Y), write(' ? (t/n)\n'),
	readln([Replay]),
	pamietaj(X, Y, Replay),
	odpowiedz(Replay, nie).    

odpowiedz(Replay, tak):-
	sub_string(Replay, 0, _, _, 't').

odpowiedz(Replay, nie):-
	sub_string(Replay, 0, _, _, 'n').

pamietaj(X, Y, Replay) :-
	odpowiedz(Replay, tak),
	assertz(xpozytywne(X, Y)).

pamietaj(X, Y, Replay) :-
	odpowiedz(Replay, nie),
	assertz(xnegatywne(X, Y)).

wyczysc_fakty :-
	write('\n\nNacisnij enter aby zakonczyc\n'),
	retractall(xpozytywne(_, _)),
	retractall(xnegatywne(_, _)),
	readln(_).

wybierz :-
	danie_to(X), !,
	write('Twoim daniem moze byc '), write(X), nl,
	wyczysc_fakty.

wybierz :-
	write('\nNie jestem w stanie znalezc odpowiedniego dla Ciebie dania, '),
	write('moze masz zbyt wysokie wymagania?\n\n'), wyczysc_fakty.


	