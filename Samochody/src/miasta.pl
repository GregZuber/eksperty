:- module(se2,[wykonaj]).
 
:- dynamic([xpozytywne/2, xnegatywne/2]).
 
miasto_jest(tokio) :-
        ma_klimat(tropikalny),
        jest(nowoczesne),
        negatywne(ma, duzo_terenow_zielonych).
       
miasto_jest(quibdo) :-
        ma_klimat(tropikalny),
        jest(spokojne),
        negatywne(ma, duzo_uniwersytetow).
       
miasto_jest(hawana) :-
        ma_klimat(tropikalny),
        jest(tradycyjne).
       
miasto_jest(alice_springs) :-
        ma_klimat(pustynny),
        jest(liberalne).
       
miasto_jest(tunis) :-
        ma_klimat(pustynny),
        jest(rygorystyczne).
       
miasto_jest(austin) :-
        ma_klimat(pustynny),
        jest(niebezpieczne).
       
miasto_jest(fairbanks) :-
        ma_klimat(mrozny),
        jest(azylem).

miasto_jest(sztokholm) :- 
        ma_klimat(mrozny),
        jest(multikulturowe),
        pozytywne(ma, duza_populacja).

miasto_jest(quebec) :-
        ma_klimat(mrozny),
        jest(niemultikulturowe),
        pozytywne(ma, duza_populacja).

miasto_jest(zurych) :-
        ma_klimat(umiarkowany),
        jest(dobre_dla_sportow_zimowych),
        jest(dobrym_punktem_wypadowym).

miasto_jest(benidorm) :-
        ma_klimat(umiarkowany),
        jest(dobre_dla_sportow_wodnych).

miasto_jest(mikolajki) :-
        ma_klimat(umiarkowany),
        pozytywne(ma, blisko_jeziora),
        negatywne(ma, dobre_polaczenia_z_innymi_miastami).

ma_klimat(tropikalny) :-
        pozytywne(ma, wysokie_opady),
        pozytywne(ma, wysoka_temperature).
       
ma_klimat(pustynny) :-
        negatywne(ma, wysokie_opady),
        pozytywne(ma, wysoka_temperature).
       
ma_klimat(mrozny) :-
        negatywne(ma, wysoka_temperature),
        pozytywne(ma, snieg_podczas_zimy).
       
ma_klimat(umiarkowany_jalowy) :-
        negatywne(ma, wysoka_temperature),
        negatywne(ma, snieg_podczas_zimy).
       
jest(nowoczesne) :-
        pozytywne(ma, wysokie_budynki),
        pozytywne(ma, nowoczesny_styl),
        pozytywne(ma, lotnisko_blisko_centrum).
       
jest(tradycyjne) :-
        negatywne(ma, nowoczesny_styl),
        negatywne(ma, wysokie_budynki),
        pozytywne(ma, duzo_terenow_zielonych),
        negatywne(ma, lotnisko_blisko_centrum).
       
jest(spokojne) :-
        pozytywne(ma, duzo_terenow_zielonych),
        negatywne(ma, lotnisko_blisko_centrum).
       
jest(liberalne) :-
        negatywne(ma, zakaz_picia_w_miejscach_publicznych),
        pozytywne(ma, zalegalizowane_narkotyki_miekkie).
       
jest(rygorystyczne) :-
        pozytywne(ma, zakaz_picia_w_miejscach_publicznych),
        negatywne(ma, zalegalizowane_narkotyki_miekkie).
       
jest(niebezpieczne) :-
        pozytywne(ma, legalny_dostep_do_broni).
 
jest(azylem) :-
        negatywne(ma, duza_populacja),
        negatywne(ma, wielu_obcokrajowcow),
        negatywne(ma, wielu_turystow).

jest(multikulturowe) :-
        pozytywne(ma, wielu_obcokrajowcow),
        pozytywne(ma, wielu_turystow).

jest(niemultikulturowe) :- 
        negatywne(ma, wielu_obcokrajowcow),
        negatywne(ma, wielu_turystow).

jest(dobre_dla_sportow_zimowych) :-
        pozytywne(ma, blisko_gory).

jest(dobre_dla_sportow_wodnych) :-
        pozytywne(ma, plaza).

jest(dobrym_punktem_wypadowym) :-
        pozytywne(ma, dobre_polaczenia_z_innymi_miastami).

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
        !, write(X), write(' to_miasto '), write(Y), write(' ? (t/n)\n'),
        readln([Replay]),
        pamietaj(X, Y, Replay),
        odpowiedz(Replay, tak).
 
 
pytaj(X, Y, nie) :-
        !, write(X), write(' to_miasto '), write(Y), write(' ? (t/n)\n'),
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
 
wykonaj :-
        miasto_jest(X), !,
        write('Twoim miastem moze byc '), write(X), nl,
        wyczysc_fakty.
 
wykonaj :-
        write('\nNie jestem w stanie odgadnac, '),
        write('jakie miasto masz na mysli.\n\n'), wyczysc_fakty.