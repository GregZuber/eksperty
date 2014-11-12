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

cecha_samochodu(okazyjnie_trudny_teren) :- pozytywne(czy,ma_okazyjnie_dać_radę_przejechać_przez_trudny_teren).

cecha_samochodu(rodzinny) :- 	pozytywne(czy,ma_to_byc_samochód_rodzinny).

cecha_samochodu(duzo_miejsca) :- 	pozytywne(czy,ma_mieć_dużo_miejsca).

cecha_samochodu(codziennie_trudny_teren) :- pozytywne(czy,musi_codziennie_przejechać_przez_trudny_teren).

cecha_samochodu(skladany_dach) :- pozytywne(czy,ma_być_ze_składanym_dachem).

cecha_samochodu(przewozenie_duzej_ilosci_bagazu) :- pozytywne(czy,ma_być_w_stanie_przewieźć_dużą_ilość_bagażu).

cecha_samochodu(wiecej_niz_7_miejsc) :- pozytywne(czy,ma_mieć_więcej_niż_7_miejsc_siedzących).

cecha_samochodu(wyscigi) :- pozytywne(czy,ma_nadawać_się_do_wyścigów).

cecha_samochodu(odsloniety_duzy_bagaznik) :- pozytywne(czy,ma_mieć_za_szybą_tylną_odsłonięty_duży_bagażnik_nadający_się_do_przewożenia_dużych_towarów).

cecha_samochodu(powiekszony_bagaznik) :- pozytywne(czy,ma_mieć_powiększony_bagażnik).

cecha_samochodu(wysuniety_tyl_nadwozia) :- pozytywne(czy,ma_mieć_wysunięty_tył_nadwozia_za_szybę_tylną).

pozytywne(X,Y) :- xpozytywne(X,Y), !.

pozytywne(X,Y) :- \+xnegatywne(X,Y), pytaj(X,Y,tak).

negatywne(X,Y) :- xnegatywne(X,Y), !.

negatywne(X,Y) :- \+xpozytywne(X,Y), pytaj(X,Y,nie).

pytaj(X,Y,tak) :- !, format('~w samochód ~w ? (t/n)~n',[X,Y]),
                    read(Reply),
                    (Reply = 't'),
                    pamietaj(X,Y,tak).
                    
pytaj(X,Y,nie) :- !, format('~w samochód ~w ? (t/n)~n',[X,Y]),
                    read(Reply),
                    (Reply = 'n'),
                    pamietaj(X,Y,nie).
                    
pamietaj(X,Y,tak) :- assertz(xpozytywne(X,Y)).

pamietaj(X,Y,nie) :- assertz(xnegatywne(X,Y)).

wyczysc_fakty :- write('Przycisnij cos aby wyjsc'), nl,
                    retractall(xpozytywne(_,_)),
                    retractall(xnegatywne(_,_)),
                    get_char(_).
                    
wykonaj :- samochod_jest(X), !,
            format('~nWybierz samochód typu ~w', X),
            nl, wyczysc_fakty.
            
wykonaj :- write('Nie jestem w stanie dopasować odpowiedniego samochodu dla Ciebie.'), nl,
            wyczysc_fakty.
