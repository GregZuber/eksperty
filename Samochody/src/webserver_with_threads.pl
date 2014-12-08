:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).

:- http_handler(/, init, []).
:- http_handler('/answer', answer, []).

:- dynamic
       backend/2.

thread_with_queue(QueueId, ThreadId) :-
    message_queue_create(QueueId),
    thread_create(threadA(QueueId), ThreadId, []).

threadA(QueueId) :-
    thread_self(SelfId),
    wait_for_question_request_and_send(QueueId, 'ala?'),
    wait_for_answer(QueueId),
    wait_for_question_request_and_send(QueueId, 'ela?'),
    wait_for_answer(QueueId),
    finish(QueueId).

wait_for_question_request_and_send(QId, Question) :-
    thread_get_message(QId, {get_question, TId}),
    format("Got question req~n"),
    thread_send_message(TId, {question, Question}),
    format("Sent question ~n").

wait_for_answer(QId) :-
    thread_get_message(QId, {answer, A}),
    format("Got answer ~p~n", [A]).

finish(QId) :-
    thread_get_message(QId, {get_question, TId}),
    format("Got question req but finishing~n"),
    thread_send_message(TId, {stop, 'just stop'}),
    format("Sent stop ~n").

wait_for_thread_termination(ThreadId) :-
    repeat,
    thread_property(ThreadId, status(X)),
    (X == true
     -> format("Terminated~n")
     ; format("Still waiting~n")).

test(Msg) :-
    thread_with_queue(QueueId, ThreadId),
    thread_send_message(QueueId, {init, TId}),
    format("Waiting for thread to terminate...~n"),
    wait_for_thread_termination(ThreadId).

start :-
    http_server(http_dispatch, [port(8000)]),
    thread_with_queue(QId, TId),
    asserta(backend(tid, TId)),
    asserta(backend(qid, QId)).

stop :-
    http_stop_server(8000, []).

init(Request) :-
    ask.

ask :-
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
    ; reply_html_page(title('POST demo'), [h1('end')]).

answer(Request) :-
    member(method(post), Request), !,
    http_read_data(Request, [answer=A | _], []),
    send_to_backend({answer, A}),
    ask.

send_to_backend(Msg) :-
    backend(qid, QId),
    thread_send_message(QId, Msg).
