% -*- mode: prolog -*-

:- use_module(library(http/http_server)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_header)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(base64)).
:- use_module(library(lists)).
:- use_module(library(persistency)).
:- use_module(library(arouter)).

:- use_module(db).
:- use_module(home_controller).
:- use_module(song_controller).
:- use_module(songbook_controller).
:- use_module(user_controller).

run :- song_db_attach,
       http_server(handle_request, [port(3333)]).

handle_request(Request):-
    (   arouter:route(Request)
    ->  true
    ;   http_dispatch(Request)).


:- run.
