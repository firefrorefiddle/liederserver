:- module(db,[
	      song_db_attach/0,
	      user/4,
	      add_user/4,
	      remove_user/2,
	      song/3,	      
	      add_song/3,
	      remove_song/1,
	      song_version/4,
	      add_song_version/4,
	      header/4,
	      add_header/5
	     ]).

:- use_module(library(persistency)).

:- persistent next_id(number).
:- persistent user(username: atom, email: atom, last_name: atom, first_name: atom).
:- persistent song(id:number, title: atom, author: atom).
:- persistent song_version(songId: number, id: number, user: atom, text: string).
:- persistent header(id: number, user: atom, key: atom, value: atom).
:- persistent songbook(id:number, title:atom, user:atom).
:- persistent songbook_version(songbookId:number, version: atom, songVersions: list).

song_db_attach :- db_attach('songs.db', []).

get_next_id(Id) :-
    with_mutex(songs_db, (
		   (next_id(Id);Id=1),
		   Id1 is Id+1,
		   retractall_next_id(Id),
		   assert_next_id(Id1))).
	       
add_user(Username, Email, LastName, FirstName) :-
    with_mutex(songs_db, assert_user(Username, Email, LastName, FirstName)).

remove_user(Username, Email) :-
    with_mutex(songs_db, retractall_user(Username, Email, _, _)).

add_song(Id, Title, Author) :-
    ( song(Id, Title, Author), !
    ; with_mutex(songs_db,
		 (
		     get_next_id(Id),
		     assert_song(Id, Title, Author)
		 ))
    ).

remove_song(Id) :-
    with_mutex(songs_db,
	       retractall_song(Id,_,_)).

add_song_version(SongId, Id, User, Text) :-
    (nonvar(SongId), ! ; throw("SongId should be bound!")),
    with_mutex(songs_db,
	       (
		   (var(Id), get_next_id(Id) ; retractall(song_version(SongId, Id, _, _, _))),
		   assert_song_version(SongId, Id, User, Text)
	       )).

add_header(SongId, VersionId, User, Key, Value) :-
    (nonvar(SongId), ! ; throw("SongId should be bound!")),
    ( song_header(Key)
    -> assert_header(SongId, User, Key, Value)
    ;  (nonvar(VersionId), ! ; throw("VersionId should be bound!")),
       assert_header(VersionId, User, Key, Value)
    ).

% belongs to the song rather than to the version
song_header(title).
song_header(copyright).
song_header(author).
song_header(reference).
song_header('extra-index').
song_header(original).
