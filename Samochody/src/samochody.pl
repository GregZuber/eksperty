:- dynamic
    xpozytywne/2,
    xnegatywne/2.


samochod_jest(audi_5) :- 		cecha_samochodu(suv),
		   			cecha_samochodu(niemiecki).
                        
samochod_jest(jeep_renegade) :- 	cecha_samochodu(suv),
					cecha_samochodu(jeep).
                        
samochod_jest(nissan_Navara) :- 	cecha_samochodu(terenowy),
					cecha_samochodu(nissan).
                        
samochod_jest(land_Rover_Defender) :- 	cecha_samochodu(terenowy),
                     			cecha_samochodu(angielski).       
              
samochod_jest(peugeot_508) :- 		cecha_samochodu(kombi),
		   			cecha_samochodu(peugeot).
                        
samochod_jest(volkswagen_Passat_B6) :- 	cecha_samochodu(kombi),
					cecha_samochodu(niemiecki).
                        
samochod_jest(opel_Astra_4) :- 		cecha_samochodu(hatchback),
					cecha_samochodu(niemiecki).
                        
samochod_jest(citroen_C4) :- 		cecha_samochodu(hatchback),
                     			cecha_samochodu(citroen).

samochod_jest(skoda_Superb_2) :- 	cecha_samochodu(sedan),
                     			cecha_samochodu(skoda).                          			

samochod_jest(mercedes_Benz_C_class) :- cecha_samochodu(sedan),
                     			cecha_samochodu(niemiecki).                       			                			

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
				pozytywne(czy,może_to_być_jeep).
				
cecha_samochodu(nissan) :- 	pozytywne(czy,może_być_produkcji_japońskiej),
				pozytywne(czy,może_to_być_nissan).
				
cecha_samochodu(peugeot) :- 	pozytywne(czy,może_być_produkcji_francuskiej),
				pozytywne(czy,może_to_być_peugeot).
				
cecha_samochodu(citroen) :- 	pozytywne(czy,może_być_produkcji_francuskiej),
				pozytywne(czy,może_to_być_citroen).
				
cecha_samochodu(skoda) :- 	pozytywne(czy,może_być_produkcji_czeskiej),
				pozytywne(czy,może_to_być_skoda).	

cecha_samochodu(angielski) :- 	pozytywne(czy,może_być_produkcji_angielskiej).							
				


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
            format('~nWybierz samochód ~w', X),
            nl, wyczysc_fakty.
            
wykonaj :- write('Nie jestem w stanie dopasować odpowiedniego samochodu dla Ciebie.'), nl,
            wyczysc_fakty.
