:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).

:- http_handler(/, ask, []).
:- http_handler('/answer', answer, []).

:- dynamic
       backend/2,
   backend/2,
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

pytaj(X,Y,tak) :- !,
                  form_question(X,Y,Question),
                  ask_and_gather_reply(Question, Reply),
                  (Reply = 't'),
                  pamietaj(X,Y,tak).
                    
pytaj(X,Y,nie) :- !,
                  form_question(X,Y,Question),
                  ask_and_gather_reply(Question, Reply),
                  (Reply = 'n'),
                  %% format("=== egzamined reply ~n"),
                  pamietaj(X,Y,nie).

form_question(X,Y,Question) :-
    atom_concat(X, '_samochod_', Tmp),
    atom_concat(Tmp, Y, Question).

ask_and_gather_reply(Question, Reply) :-
    backend(qid, QId),
    format('=== Waiting for question req~n'),

    thread_get_message(QId, {get_question, TId}),
    format("=== Got question req~n"),

    thread_send_message(TId, {question, Question}),
    format("=== Sent question: ~s~n", [Question]),

    thread_get_message(QId, {answer, Reply}),
    format("=== Got reply: ~s~n", [Reply]).
                    
pamietaj(X,Y,tak) :-
    format("== Remember: ~s ~s tak~n", [X,Y]),
    assertz(xpozytywne(X,Y)).

pamietaj(X,Y,nie) :-
    format("== Remember: ~s ~s nie~n", [X,Y]),
    assertz(xnegatywne(X,Y)).

wyczysc_fakty :- write('Przycisnij cos aby wyjsc'), nl,
                    retractall(xpozytywne(_,_)),
                    retractall(xnegatywne(_,_)),
                    get_char(_).

backend_init(QueueId, ThreadId) :-
    asserta(backend(tid, ThreadId)),
    asserta(backend(qid, QueueId)),
    wykonaj.

wykonaj :-
    format('=== entered backend thread~n'),
    samochod_jest(X), !,
    %% append("Wybierz samochód ", X, Result),
    send_final_reply('final reply'),
    nl, wyczysc_fakty.
            
wykonaj :-
    Result = 'Nie jestem w stanie dopasować odpowiedniego samochodu dla Ciebie.',
    send_final_reply(Result),
    nl,
    wyczysc_fakty.

send_final_reply(Reply) :-
    backend(qid, QId),
    thread_get_message(QId, {get_question, TId}),
    format("=== Got question req~n"),
    thread_send_message(TId, {stop, Reply}),
    format('=== Sent final reply ~w~n', Reply).

start :-
    http_server(http_dispatch, [port(8000)]),
    format('=== started server~n'),
    start_backend(QId, TId),
    format('=== started backend~n'),
    asserta(backend(tid, TId)),
    asserta(backend(qid, QId)).

stop :-
    http_stop_server(8000, []).

start_backend(QueueId, ThreadId) :-
    message_queue_create(QueueId),
    thread_create(backend_init(QueueId, ThreadId), ThreadId, []).

ask(_Request) :-
    thread_self(TId),
    send_to_backend({get_question, TId}),
    thread_get_message(QuestionOrStop),
    {question, Q} = QuestionOrStop  ->
        reply_html_page(
                title('POST demo'),
                [
                    h1(Q),
                    form([action='/answer', method='POST'], [
                             p([], [
                                   label([for=answer],'Answer (y/n):'),
                                   input([name=answer, type=textarea])
                              ]),
                             p([], input([name=submit, type=submit, value='Submit'], []))
            ])])
    ; {stop, Reply} = QuestionOrStop,
      reply_html_page(title('POST demo'), [h1(Reply)]).

answer(Request) :-
    member(method(post), Request), !,
    http_read_data(Request, [answer=A | _], []),
    send_to_backend({answer, A}),
    ask(Request).

send_to_backend(Msg) :-
    backend(qid, QId),
    thread_send_message(QId, Msg).
