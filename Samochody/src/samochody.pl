:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- http_handler(/, wykonaj, []).

server :-
    http_server(http_dispatch, [port(8000)]).

:- dynamic
    xpozytywne/2,
    xnegatywne/2.

samochod_jest(suv) :- 	cecha_samochodu(okazyjnie_trudny_teren),
		   	cecha_samochodu(rodzinny),
		   	cecha_samochodu(duzo_miejsca).
                        
samochod_jest(terenowy) :- 	cecha_samochodu(codziennie_trudny_teren),
				cecha_samochodu(duzo_miejsca).
                        
samochod_jest(kabriolet) :- cecha_samochodu(skladany_dach).
                        
samochod_jest(minbus) :- 	cecha_samochodu(wiecej_niz_7_miejsc),
                     		cecha_samochodu(przewozenie_duzej_ilosci_bagazu).
              
samochod_jest(sportowy) :- cecha_samochodu(wyscigi).

samochod_jest(pickup) :- cecha_samochodu(odsloniety_duzy_bagaznik).

samochod_jest(combi) :- cecha_samochodu(powiekszony_bagaznik).

samochod_jest(sedan) :- cecha_samochodu(wysuniety_tyl_nadwozia).

samochod_jest(hatchback) :- cecha_samochodu(nie_wysuniety_tyl_nadwozia).

cecha_samochodu(okazyjnie_trudny_teren) :- 	pozytywne(czy,ma_okazyjnie_dac_rade_przejechac_przez_trudny_teren),
						pozytywne(czy,ma_miec_wyzsze_niz_standardowe_zawieszenie).

cecha_samochodu(rodzinny) :- 	pozytywne(czy,ma_to_byc_samochod_rodzinny),
				pozytywne(czy,ma_byc_taki_ze_5_osob_moze_w_nim_wygodnie_usiasc).

cecha_samochodu(duzo_miejsca) :- 	pozytywne(czy,ma_miec_duzo_miejsca).

cecha_samochodu(codziennie_trudny_teren) :- 	pozytywne(czy,musi_codziennie_przejechac_przez_trudny_teren),
						pozytywne(czy,ma_miec_wyciagarke),
						pozytywne(czy,ma_miec_wysokie_nadwozie).

cecha_samochodu(skladany_dach) :- 	pozytywne(czy,ma_byc_ze_skladanym_dachem),
					pozytywne(czy,umozliwia_opalanie_sie_w_lecie).

cecha_samochodu(przewozenie_duzej_ilosci_bagazu) :- pozytywne(czy,ma_byc_w_stanie_przewieźc_duza_ilosc_bagazu).

cecha_samochodu(wiecej_niz_7_miejsc) :- pozytywne(czy,ma_miec_wiecej_niz_7_miejsc_siedzacych).

cecha_samochodu(wyscigi) :- 	pozytywne(czy,ma_nadawac_sie_do_wyscigow),
				pozytywne(czy,ma_miec_sportowy_silnik),
				pozytywne(czy,ma_miec_silnik_o_duzej_pojemnosci).

cecha_samochodu(odsloniety_duzy_bagaznik) :- 	pozytywne(czy,ma_miec_za_szyba_tylna_odsloniety_duzy_bagaznik),
						pozytywne(czy,bagaznik_za_tylna_szyba_ma_sie_nadawac_do_przewozenia_duzych_materialow).

cecha_samochodu(powiekszony_bagaznik) :- 	pozytywne(czy,ma_miec_powiekszony_bagaznik),
						pozytywne(czy,ma_miec_bagaznik_w_ktorym_zmiesci_sie_rodzina_podczas_wyjazdu_na_wakacje).	

cecha_samochodu(wysuniety_tyl_nadwozia) :- 	pozytywne(czy,ma_miec_wysuniety_tyl_nadwozia_za_szybe_tylna),
					 	negatywne(czy,ma_miec_tyl_nadwozia_na_rowni_z_szyba_tylna).

pozytywne(X,Y) :- xpozytywne(X,Y), !.

pozytywne(X,Y) :- \+xnegatywne(X,Y), pytaj(X,Y,tak).

negatywne(X,Y) :- xnegatywne(X,Y), !.

negatywne(X,Y) :- \+xpozytywne(X,Y), pytaj(X,Y,nie).

pytaj(X,Y,tak) :-
    !, format('~w samochod ~w ? (t/n)~n',[X,Y]).
                    %% read(Reply),
                    %% (Reply = 't'),
                    %% pamietaj(X,Y,tak).
                    
pytaj(X,Y,nie) :-
    !, format('~w samochod ~w ? (t/n)~n',[X,Y]).
                    %% read(Reply),
                    %% (Reply = 'n'),
                    %% pamietaj(X,Y,nie).
                    
pamietaj(X,Y,tak) :- assertz(xpozytywne(X,Y)).

pamietaj(X,Y,nie) :- assertz(xnegatywne(X,Y)).

wyczysc_fakty :- write('Przycisnij cos aby wyjsc'), nl,
                    retractall(xpozytywne(_,_)),
                    retractall(xnegatywne(_,_)),
                    %% get_char(_).
                    halt.
                    
wykonaj(_Request) :- format('Content-type: text/plain~n~n'),
                     samochod_jest(X), !,
                     format('~nWybierz samochod typu ~w', X),
                     nl, wyczysc_fakty.
            
wykonaj(Request) :- format('Content-type: text/plain~n~n'),
                    write('Nie jestem w stanie dopasowac odpowiedniego samochodu dla Ciebie.'), nl,
                    wyczysc_fakty.
