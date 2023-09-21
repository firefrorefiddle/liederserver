:- use_module(library(http/http_server)).
:- use_module(library(http/http_files)).
:- use_module(library(arouter)).
:- use_module(library(base64)).
:- use_module(library(lists)).
:- use_module(db).
:- use_module(library(persistency)).

:- route_get(/, get_songs).
:- route_get(song_from_db, get_song).
:- route_get(song_preview, get_song_preview).

:- http_handler(root(assets/'style.css'), http_reply_file('assets/style.css', []), [id(style_GET)]).

run :- song_db_attach,
       http_server(handle_request, [port(3333)]).

get_songs :-
    findall(Song-Author-Id-Headers, song_from_db(Id, Song, Author, Headers), Songs),
    sort(Songs, Sorted),
    reply_standard_layout("Songs", ul([], \song_list(Sorted))).

song_list([]) --> [].
song_list([Title-Author-Id-Headers|Rest]) -->
    { title_attrs(Headers, TitleText)
    },
    html(li(a([title(TitleText), href('/song_from_db?id='+Id)],[Title, " by ",Author]))),
    song_list(Rest).

title_attrs(Headers, TitleText) :-
    maplist([Key-Value,Title]>>atomic_list_concat([Key, ": ", Value, "\n"], Title),
	    Headers,
	    TitleText).   

get_song :-
    http_current_request(R),
    http_parameters(R, [id(IdA, [])]),
    atom_number(IdA, Id),
    song_from_db(Id, Title, _, _),
    once(song_version(Id, VersionId, _User, Text)),
    reply_standard_layout(Title,
			  [ div([class(flex_container)],
				[ div([class(flex_child)],
				      pre([], [Text])),
				  div([class(flex_child), id(preview),
				       'hx-get'('/song_preview?id='+Id+'&versionId='+VersionId),
				       'hx-trigger'('load from:body')]				     
				      ,[])
				])
			  ]).

get_song_preview :-
    http_current_request(R),    
    http_parameters(R, [versionId(VersionIdA, [])]),
    atom_number(VersionIdA, VersionId),    
    get_song_preview(VersionId).

get_song_preview(VersionId) :-
    song_version(_, VersionId, _, Text),
    tmp_file(preview, Path),
    open(Path, write, Stream),
    write(Stream, Text),
    close(Stream),
    latex_song(Path, Pngs),
    reply_htmx(div(\disp_pngs(Pngs))).
    
disp_pngs([]) --> [].
disp_pngs([Png|Pngs]) --> disp_png(Png), disp_pngs(Pngs).

disp_png(Png) -->
    {
	read_file_to_codes(Png,Codes,[encoding(octet)]),
	phrase(base64(Codes),Base64Codes),
	string_codes(Base64, Base64Codes),
	string_concat("data:image/png;base64, ", Base64, Src)
    },
    html(img([style('width:450'),src(Src),alt("Preview")])).
    
song_title(Song, Headers, Title) :-
    once(member(title-Title, Headers) ; Title = Song).

reply_htmx(Elem) :-
    html_write:html_current_option(content_type(Type)),
    phrase(html(Elem), HTML),
    format('Content-type: ~w~n~n', [Type]),
    html_write:print_html(HTML).

reply_standard_layout(Title, Body) :-
    reply_html_page([title(Title),
		     script([src("https://unpkg.com/htmx.org@1.9.2"),
			     integrity("sha384-L6OqL9pRWyyFU3+/bjdSri+iIphTN/bvYyM37tICVyOJkWZLpP2vGn6VUEXgzg6h"),
			     crossorigin("anonymous")],[]),
		     link([rel(stylesheet),href('/assets/style.css')])],
		    div([id(body),'hx-boost'(true)],
			[\navbar,
			 h1([class(page_title)], Title),
			 div([],Body)])).

navbar -->  html(div([id(navbar),'hx-boost'(true)],
		     [
			 a([href('/')],"Songs")
		     ]
		    )).


song_root('/home/mike/src/Liedermappe/lieder').
songmaker_bin('/home/mike/local/bin/songmaker').
tex_root('/home/mike/src/Liedermappe/tex').
tex_harness(chorded, 'chorded_standalone.tex', "Chorded").

song_from_db(Id, Title, Author, Headers) :-
    song(Id, Title, Author),
    findall(Key-Value, header(Id, _, Key, Value), Headers).

song_from_fs(S, Path, Headers) :-
    song_root(R),
    directory_files(R, Files),
    member(S, Files),    
    sub_atom(S, _, _ ,_, '.sng'),
    atomic_list_concat([R, '/', S], Path),
    open(Path, read, Stream, []),	
    song_headers(Stream, Headers),
    close(Stream).

import_songs(User) :-
    song_from_fs(S, Path, Headers),
    writeln(S),
    song_title(S, Headers, Title),
    file_text(Path, Text),
    (member(author-Author, Headers) -> true ;
     (member(lyricsBy-Author, Headers) -> true;
      throw(missing_author(S, Headers, Title)))),
    add_song(SongId, Title, Author),
    add_song_version(SongId, VersionId, User, Text),
    import_headers(SongId, VersionId, User, Headers),
    false.
import_songs(_) :- true.

import_headers(SongId, VersionId, User, Headers) :-
    member(Key-Value, Headers),
    add_header(SongId, VersionId, User, Key, Value),
    fail.
import_headers(_, _, _, _) :- true.

song_header(Stream, Key-Value) :-
    repeat,
    read_line_to_string(Stream, String),
    (
	once(sub_string(String,B,_,A,": ")
	    -> true ; 	once(sub_string(String,B,_,A,":"))),
	sub_string(String,0,B,_,KeyS),
	sub_string(String,_,A,0,ValueS),
	atom_string(Key, KeyS),
	atom_string(Value, ValueS)    
    -> true
    ;
    !).

song_headers(Stream, Headers) :-
    findall(Key-Value, (song_header(Stream, Key-Value), nonvar(Key)), Headers).
       
handle_request(Request):-
(   route(Request)
->  true
;   http_dispatch(Request)).


file_lines(Path, Lines) :-
    findall(Line,
	    (open(Path, read, Stream, []),
	     repeat,
	     read_line_to_string(Stream, Line),
	     ( Line = end_of_file
	     -> !,
		fail		
	     ; true)),
	    Lines).

file_text(Path, Text) :-
    file_lines(Path, Lines),
    join_nl(Lines, Text).

join_nl([], "").
join_nl([L|Ls], Text) :-
    join_nl(Ls, Text0),
    string_concat(L, "\n", Text1),
    string_concat(Text1, Text0, Text).

run_songmaker(Path, Tex) :-
    songmaker_bin(Songmaker),
    tmp_file('song', Tmp),
    atom_concat(Tmp, '.sng', Sng),
    atom_concat(Tmp, '.tex', Tex),
    copy_file(Path, Sng),    
    process_create(Songmaker, [Sng], []).

copy_from_tex_root(Path, Target) :-
    tex_root(TexRoot),    
    atomic_list_concat([TexRoot,'/',Path], SourcePath),    
    copy_file(SourcePath, Target).

simple_tex(BasicTex, Harness, FullTex) :-
    tex_harness(Harness, HarnessFile, _),
    tmp_file('tex', FullTex0),
    atom_concat(FullTex0, '.tex', FullTex),    
    file_directory_name(FullTex, Directory),
    file_base_name(BasicTex, BasicFilename),
    copy_from_tex_root(HarnessFile, Directory),
    copy_from_tex_root('layout_standalone.tex', Directory),
    copy_from_tex_root('font.tex', Directory),        
    open(FullTex, write, Stream),
    format(Stream, '\\input{~a}~n\\begin{document}\\begin{songs}{}~n\\input{~a}~n\\end{songs}\\end{document}~n', [HarnessFile, BasicFilename]),
    close(Stream).

latex_song(Path, Files) :-
    run_songmaker(Path, Tex),
    simple_tex(Tex, chorded, CompleteTex),
    file_directory_name(CompleteTex, Directory),        
    atom_concat(Basename, '.tex', CompleteTex),
    atom_concat(Basename, '.dvi', Dvi),
    process_create(path(latex), [CompleteTex],[cwd(Directory)]),
    process_create(path(dvipng), ["-D300", Dvi],[cwd(Directory)]),
    find_png_files(Basename, Files).    

find_png_files(Basename, Files) :-
    atom_concat(Basename, '.log', Log),
    phrase_from_file(number_of_pages(N), Log),
    (
	N=1
    -> atom_concat(Basename, '1.png', File),
       Files=[File]
    ; bagof(I,between(1,N,I),Is),
      maplist({Basename}/[I,File]>>atomic_list_concat([Basename, I, '.png'], File),
	      Is, Files)
    ).
      

number_of_pages(N) -->
    {
	atom_codes('(', Paren),
	atom_codes(' page', Pages)
    },
    ..., Paren, [NCodes], Pages, ...,
    {
	atom_codes(NAtom, [NCodes]),
	atom_number(NAtom, N)
    }.	

seq([])     --> [].
seq([E|Es]) --> [E], seq(Es).

... --> [] | [_], ... .
