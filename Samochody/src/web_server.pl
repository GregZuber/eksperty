:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/websocket)).
:- http_handler(/, say_hi, []).

server :-
    http_server(http_dispatch, [port(8000)]).

say_hi(_Request) :-
    reply_html_page(
            [title('Howdy')],
            [h1('A Simple Web Page'),
             p('With some text')]).
