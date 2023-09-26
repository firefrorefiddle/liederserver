:- use_module(library(http/http_server)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(arouter)).
:- use_module(library(base64)).
:- use_module(library(lists)).
:- use_module(db).
:- use_module(library(persistency)).

handle_hx(Handler,_) :- % second param used internally in arouter, is unbound
    http_current_request(R),
    (
	member(hx_target(_), R)
    ->  Method=reply_htmx
    ;
    (   member(hx_boosted(true), R)
    ->  Method=reply_html
    ;   Method=reply_standard_layout
    )
    ),
    handle_hx_request(Method, Handler).

hx_route_get(Address, Handler, Options) :-
    route_get(Address, handle_hx(Handler), Options).

hx_route_get(Address, Handler) :-
    hx_route_get(Address, Handler, []).

handle_hx_request(Method, Handler) :-
    call(Handler, Method).

:- hx_route_get(/, get_songs).
:- hx_route_get(song_from_db, get_song).
:- hx_route_get(song, get_song).
:- hx_route_get(song_preview, get_song_preview).

:- http_handler(root(assets/'style.css'), http_reply_file('assets/style.css', []), [id(style_GET)]).

run :- song_db_attach,
       http_server(handle_request, [port(3333)]).

get_songs(Respond) :-
    findall(Song-Author-Id-Headers, song_from_db(Id, Song, Author, Headers), Songs),
    sort(Songs, Sorted),
    call(Respond,
	 "Songs",
	 div([
		    input([type(text),placeholder("Filter"),
			   '_'("
on keyup		   
   if the event's key is 'Escape'
      set my value to ''
      trigger keyup
   else
      show <li/> in #songlist when its textContent.toLowerCase() contains my value.toLowerCase()")]),
		    
		    ul([id(songlist)], \song_list(Sorted))
		])).

song_list([]) --> [].
song_list([Title-Author-Id-Headers|Rest]) -->
    { title_attrs(Headers, TitleText)
    },
    html(li(a([title(TitleText), href('/song_from_db?id='+Id)],[Title, " (",Author,")"]))),
    song_list(Rest).

title_attrs(Headers, TitleText) :-
    maplist([Key-Value,Title]>>atomic_list_concat([Key, ": ", Value, "\n"], Title),
	    Headers,
	    TitleText).   

get_song(Respond) :-
    http_current_request(R),
    http_parameters(R, [id(IdA, []),edit(Edit, [default(false)])]),
    atom_number(IdA, Id),
    get_song(Respond, Id, Edit).

get_song(Respond, Id, Edit) :-
    song_from_db(Id, Title, _, _),    
    once(song_version(Id, VersionId, _User, Text)),
    call(Respond,
	 Title,
	 [ div([class(flex_container)],
	       [ \lyrics_editor(Id, VersionId, Text, Edit),
		 div([class(flex_child), id(preview),
		      'hx-get'('/song_preview?id='+Id+'&versionId='+VersionId),
		      'hx-trigger'('load from:body')]				     
		     ,[])
	       ])
	 ]).

lyrics_editor(Id, VersionId, Text, false) -->
    html(div([class(flex_child)],
	     \disp_lyrics(Id, VersionId, Text))).

lyrics_editor(Id, VersionId, Text, true) -->
    html(div([class(flex_child)],
	     [div([id="disp_edit"],
		  [a([href='/song?id='+Id+'&versionId='+VersionId],["Cancel"]),		  
		   textarea([
			  style="height:80vh;width:50vw;white-space:pre",
			  rows="20",
			  cols="80",
			  id="editor"
			  ],[Text])])
	      ])).


disp_lyrics(Id, VersionId, Text) -->    
    html(div([id("disp_edit")],
	     [a([href='/song?edit=true&id='+Id+'&versionId='+VersionId],["Edit"]),		    
	      pre([style="height:80vh;width:50vw;white-space:pre"], [Text])])).

get_song_preview(Respond) :-
    http_current_request(R),    
    http_parameters(R, [versionId(VersionIdA, [])]),
    atom_number(VersionIdA, VersionId),    
    get_song_preview(Respond, VersionId).

get_song_preview(Respond, VersionId) :-
    song_version(_, VersionId, _, Text),
    tmp_file(preview, Path),
    open(Path, write, Stream),
    write(Stream, Text),
    close(Stream),
    latex_song(Path, Pngs),
    call(Respond, div(\disp_pngs(Pngs))).

% TODO remove
get_song_editor(Respond) :-
    http_current_request(R),    
    http_parameters(R, [versionId(VersionIdA, [])]),
    atom_number(VersionIdA, VersionId),    
    get_song_editor(Respond, VersionId).

% TODO remove
get_song_editor(Respond, VersionId) :-
    once(song_version(_, VersionId, _, Text)),    
    call(Respond,
	 div([id="disp_edit"],
	     [button([value="Edit",
		      'hx-get'='/get_song_lyrics?versionId='+VersionId,
		      'hx-target'='#disp_edit',
		      'hx-swap'='outerHtml'],["Cancel"]),
	      input([type=textarea,
		     style="height:50vh;width:50vw",
		     rows=100,
		     cols=80,
		     id="editor",
		     value=Text],[])])).


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
    retractall(last_request(_)),
    http_current_request(R),
    assertz(last_request(R)),
    
    html_write:html_current_option(content_type(Type)),
    phrase(html(Elem), HTML),
    format('Content-type: ~w~n~n', [Type]),
    html_write:print_html(HTML).

reply_html(Content) :- reply_html('No Title', Content).
reply_html(Title, Content) :-
    Header = [title(Title)],
    reply_standard_layout(Header, Title, Content).

standard_head(Title, Head) :-
    Head = [title(Title),
	    script([src="https://code.jquery.com/jquery-3.7.1.slim.min.js", integrity="sha256-kmHvs0B+OpCW5GVHUNjv9rOmY0IvSIRcf7zGUDTDQM8=", crossorigin="anonymous"],[]),
	    script([src("https://unpkg.com/htmx.org@1.9.2"),
		    integrity("sha384-L6OqL9pRWyyFU3+/bjdSri+iIphTN/bvYyM37tICVyOJkWZLpP2vGn6VUEXgzg6h"),
		    crossorigin("anonymous")],[]),
	    script([src("https://unpkg.com/htmx.org/dist/ext/sse.js")],[]),
	    script([src("https://unpkg.com/hyperscript.org@0.9.11")],[]),
%	    link([rel(stylesheet),
%		  href("https://cdn.jsdelivr.net/npm/bootstrap@3.4.1/dist/css/bootstrap.min.css"),
%		  integrity("sha384-HSMxcRTRxnN+Bdg0JdbxYKrThecOKuH5zCYotlSAcp1+c8xmyTe9GYg1l9a69psu"),
%		  crossorigin("anonymous")],[]),
%	    link([rel("stylesheet"), href("https://cdn.jsdelivr.net/npm/bootstrap@3.4.1/dist/css/bootstrap-theme.min.css"), integrity("sha384-6pzBo3FDv/PJ8r2KRkGHifhEocL+1X2rVCTTkUfGk7/0pbek5mMa1upzvWbrUbOZ"), crossorigin("anonymous")],[]),
%	    script([src("https://cdn.jsdelivr.net/npm/bootstrap@3.4.1/dist/js/bootstrap.min.js"), integrity("sha384-aJ21OjlMXNL5UyIl/XNwTMqvzeRMZH2w8c5cRVpzpU8Y5bApTppSuUkhZXN0VxHd"), crossorigin("anonymous")],[]),
	    link([rel(stylesheet),href('/assets/style.css')])].

:- dynamic last_request/1.

reply_standard_layout(Body) :-
    reply_standard_layout('No title', Body).

reply_standard_layout(Title, Body) :-
    standard_head(Title, Head),
    reply_standard_layout(Head, Title, Body).

reply_standard_layout(Head, Title, Body) :-
    retractall(last_request(_)),
    http_current_request(R),
    assertz(last_request(R)),
    reply_html_page(Head,
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


dbg_response(Title, Response) :-
    write("Title: "),
    writeln(Title),
    dbg_response(Response).
dbg_response(Response) :-
    (  phrase(html(Response), H)
    -> print_html(H)
    ;  writeln(Response)
    ).
