:- dynamic
    xpozytywne/2,
    xnegatywne/2.


samochod_jest(audi_5) :- 		cecha_samochodu(suv),
		   			cecha_samochodu(niemiecki),
		   			cecha_samochodu(automatyczna_skrzynia),
		   			cecha_samochodu(pojemnosc_wieksza_niz_2_litry),
		   			negatywne(czy,musi_to_być_samochód_wyprodukowany_po_2010_roku).
                        
samochod_jest(jeep_renegade) :- 	cecha_samochodu(suv),
					cecha_samochodu(jeep),
					cecha_samochodu(manulana_skrzynia),
					cecha_samochodu(szyberdach),
					pozytywne(czy,może_być_droższy_niż_30_000_złotych),
					negatywne(czy,musi_mieć_klimatyzację).
                        
samochod_jest(nissan_Navara) :- 	cecha_samochodu(terenowy),
					cecha_samochodu(nissan),
					cecha_samochodu(bez_szyberdachu),
					cecha_samochodu(4_drzwiowy),
					pozytywne(czy,musi_to_być_samochód_garażowany),
					pozytywne(czy,musi_mieć_klimatyzację).
                        
samochod_jest(land_Rover_Defender) :- 	cecha_samochodu(terenowy),
                     			cecha_samochodu(angielski),
                     			cecha_samochodu(pojemnosc_mniejsza_niz_2_litry),
                     			pozytywne(czy,może_mieć_przebieg_większy_niż_100_000_kilometrów).   
              
samochod_jest(peugeot_508) :- 		cecha_samochodu(kombi),
		   			cecha_samochodu(peugeot),
		   			cecha_samochodu(automatyczna_skrzynia),
		   			negatywne(czy,może_być_droższy_niż_30_000_złotych),
		   			pozytywne(czy,musi_to_być_samochód_bezwypadkowy).
                        
samochod_jest(volkswagen_Passat_B6) :- 	cecha_samochodu(kombi),
					cecha_samochodu(niemiecki),
					negatywne(czy,może_być_droższy_niż_30_000_złotych),
					pozytywne(czy,musi_to_być_samochód_bezwypadkowy).
                        
samochod_jest(opel_Astra_4) :- 		cecha_samochodu(hatchback),
					cecha_samochodu(niemiecki),
					cecha_samochodu(manulana_skrzynia),
					cecha_samochodu(nie_4_drzwiowy),
					negatywne(czy,musi_to_być_samochód_bezwypadkowy).
                        
samochod_jest(citroen_C4) :- 		cecha_samochodu(hatchback),
                     			cecha_samochodu(citroen),
                     			cecha_samochodu(szyberdach),
                     			negatywne(czy,musi_to_być_samochód_garażowany).

samochod_jest(skoda_Superb_2) :- 	cecha_samochodu(sedan),
                     			cecha_samochodu(skoda),
                     			cecha_samochodu(bez_szyberdachu),
                     			pozytywne(czy,musi_to_być_samochód_wyprodukowany_po_2010_roku).                          			

samochod_jest(mercedes_Benz_C_class) :- cecha_samochodu(sedan),
                     			cecha_samochodu(niemiecki),
                     			cecha_samochodu(4_drzwiowy),
                     			pozytywne(czy,może_być_droższy_niż_30_000_złotych),
                     			negatywne(czy,może_mieć_przebieg_większy_niż_100_000_kilometrów),
                     			pozytywne(czy,musi_mieć_ABS).                       			                			

cecha_samochodu(suv) :- 	pozytywne(czy,ma_dać_radę_przejechać_przez_trudny_teren),
				pozytywne(czy,ma_to_być_samochód_rodzinny).

cecha_samochodu(terenowy) :- 	pozytywne(czy,ma_dać_radę_przejechać_przez_trudny_teren),
				pozytywne(czy,ma_mieć_wyciągarkę).

cecha_samochodu(kombi) :- 	negatywne(czy,ma_mieć_wysunięty_tył_nadwozia_za_szybę_tylną),
				pozytywne(czy,ma_mieć_powiększony_bagażnik).

cecha_samochodu(hatchback) :- 	negatywne(czy,ma_mieć_wysunięty_tył_nadwozia_za_szybę_tylną),
				negatywne(czy,ma_mieć_powiększony_bagażnik).

cecha_samochodu(sedan) :- 	pozytywne(czy,ma_mieć_wysunięty_tył_nadwozia_za_szybę_tylną),
				negatywne(czy,ma_mieć_powiększony_bagażnik).

cecha_samochodu(niemiecki) :- 	pozytywne(czy,może_być_produkcji_niemieckiej).

cecha_samochodu(jeep) :- 	pozytywne(czy,może_być_produkcji_amerykańskiej),
				pozytywne(czy,może_być_marki_jeep).
				
cecha_samochodu(nissan) :- 	pozytywne(czy,może_być_produkcji_japońskiej),
				pozytywne(czy,może_być_marki_nissan).
				
cecha_samochodu(peugeot) :- 	pozytywne(czy,może_być_produkcji_francuskiej),
				pozytywne(czy,może_być_marki_peugeot).
				
cecha_samochodu(citroen) :- 	pozytywne(czy,może_być_produkcji_francuskiej),
				pozytywne(czy,może_tbyć_marki_citroen).
				
cecha_samochodu(skoda) :- 	pozytywne(czy,może_być_produkcji_czeskiej),
				pozytywne(czy,może_być_marki_skoda).	

cecha_samochodu(angielski) :- 	pozytywne(czy,może_być_produkcji_angielskiej).

cecha_samochodu(automatyczna_skrzynia) 		:-	 	pozytywne(czy,może_mieć_automatyczną_skrzynię_biegów).
							
cecha_samochodu(manulana_skrzynia) 		:-		negatywne(czy,może_mieć_automatyczną_skrzynię_biegów).

cecha_samochodu(pojemnosc_wieksza_niz_2_litry) 	:- 		pozytywne(czy,ma_mieć_pojemność_większą_niż_2_litry).
				
cecha_samochodu(pojemnosc_mniejsz_niz_2_litry) 	:- 		negatywne(czy,ma_mieć_pojemność_większą_niż_2_litry).

cecha_samochodu(szyberdach)  			:- 		pozytywne(czy,ma_mieć_szyberdach).

cecha_samochodu(bez_szyberdachu)		:- 		negatywne(czy,ma_mieć_szyberdach).

cecha_samochodu(4_drzwiowy)  			:- 		pozytywne(czy,ma_mieć_4_drzwi).

cecha_samochodu(nie_4_drzwiowy)			:- 		negatywne(czy,ma_mieć_4_drzwi).

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
	!, write(X), write(' to_samochód '), write(Y), write(' ? (t/n)\n'),
	readln([Replay]),
	pamietaj(X, Y, Replay), 
	odpowiedz(Replay, tak).


pytaj(X, Y, nie) :-
	!, write(X), write(' samochód '), write(Y), write(' ? (t/n)\n'),
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

wyczysc_fakty :- write('Przycisnij cos aby wyjsc'), nl,
                    retractall(xpozytywne(_,_)),
                    retractall(xnegatywne(_,_)),
                    get_char(_).
                    
wykonaj :- samochod_jest(X), !,
            format('~nWybierz samochód ~w', X),
            nl, wyczysc_fakty.
            
wykonaj :- write('Nie jestem w stanie dopasować odpowiedniego samochodu dla Ciebie.'), nl,
            wyczysc_fakty.
