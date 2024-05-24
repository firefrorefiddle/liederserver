:- use_module(library(http/http_server)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_header)).
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

hx_route_post(Address, Handler, Options) :-
    route_post(Address, handle_hx(Handler), Options).

hx_route_post(Address, Handler) :-
    hx_route_post(Address, Handler, []).

handle_hx_request(Method, Handler) :-
    call(Handler, Method).

:- hx_route_get(/, get_home).
:- hx_route_get(songs, get_songs).
:- hx_route_get(songbooks, get_songbooks).
:- hx_route_get(song_from_db, get_song).
:- hx_route_get(song, get_song).
:- hx_route_get(song_preview, get_song_preview).
:- hx_route_post(save_song, post_save_song).

:- http_handler(root(test_redirect1), test_redirect, []).
test_redirect(_) :- http_status_reply(see_other(/), current_output, ['HX-Redirect'(/)],303).

:- http_handler(root(assets/'style.css'), http_reply_file('assets/style.css', []), [id(style_GET)]).

run :- song_db_attach,
       http_server(handle_request, [port(3333)]).

get_home(Respond) :-
    call(Respond,
         "Home",
         div([class="container mx-auto p-4"],
             [ "Songbook"
             ])).


get_songs(Respond) :-
    findall(Song-Author-Id-Headers, song_from_db(Id, Song, Author, Headers), Songs),
    sort(Songs, Sorted),
    call(Respond,
         "Songs",
         div([class="container mx-auto p-4"],
             [ input([type="text", placeholder="Filter", class="border p-2 mb-4 w-full", '_'("
on keyup                  
   if the event's key is 'Escape'
      set my value to ''
      trigger keyup
   else
      show <li/> in #songlist when its textContent.toLowerCase() contains my value.toLowerCase()")]),
               ul([id="songlist", class="list-disc pl-5"],
                  \song_list(Sorted))
             ])).

song_list([]) --> [].
song_list([Title-Author-Id-Headers|Rest]) -->
    { title_attrs(Headers, TitleText) },
    html(li([class="mb-2"],
            a([title=TitleText, href='/song_from_db?id='+Id, class="text-blue-500 hover:underline"],
              [Title, " (", Author, ")"]))),
    song_list(Rest).


title_attrs(Headers, TitleText) :-
    maplist([Key-Value,Title]>>atomic_list_concat([Key, ": ", Value, "\n"], Title),
	    Headers,
	    TitleText).   

get_songbooks(Respond) :-
    findall(Songbook-Author-Id, songbook(Id, Songbook, Author), Songbooks),
    sort(Songbooks, Sorted),
    call(Respond,
         "Songbooks",
         div([class="container mx-auto p-4"],
             [ input([type="text", placeholder="Filter", class="border p-2 mb-4 w-full", '_'("
on keyup                  
   if the event's key is 'Escape'
      set my value to ''
      trigger keyup
   else
      show <li/> in #songbooklist when its textContent.toLowerCase() contains my value.toLowerCase()")]),
               ul([id="songbooklist", class="list-disc pl-5"],
                  \songbook_list(Sorted))
             ])).



songbook_list(Songs) -->
    map_list([Title-Author-Id]>>html(li([class="mb-2"],
					a([title=Title, href='/songbook_from_db?id='+Id, class="text-blue-500 hover:underline"],
					  [Title, " (", Author, ")"]))),
	     Songs).


get_song(Respond) :-
    http_current_request(R),
    http_parameters(R, [id(IdA, []),
			versionId(VersionId, [integer, optional(true)]),
			edit(Edit, [default(false)])
		       ]),
    atom_number(IdA, Id),
    get_song(Respond, Id, VersionId, Edit).

get_song(Respond, Id, VersionId, Edit) :-
    song_from_db(Id, Title, _, _),    
    once(song_version(Id, VersionId, _User, Text)),
    call(Respond,
	 Title,
	 [ div([class(flex_container)],
	       [ \lyrics_editor(Id, VersionId, Text, Edit),
		 div([class(flex_child), id(preview),
		      'hx-get'('/song_preview?id='+Id+'&versionId='+VersionId),
		      'hx-trigger'('load from:body'),
		      'hx-target'('#preview-div')
		     ]			     
		     ,["X",
		       div([id('preview-div')],[])])
	       ])
	 ]).

lyrics_editor(Id, VersionId, Text, false) -->
    html(div([class(flex_child)],
	     \disp_lyrics(Id, VersionId, Text))).

lyrics_editor(Id, VersionId, Text, true) -->
    html(div([class(flex_child)],
	     [div([id="disp_edit"],
		  form([a([href='/song?id='+Id+'&versionId='+VersionId],["Cancel"]),
			" ",
			a([href='#',
			   'hx-post'='/save_song',
			   'hx-include'='#editor',
			   'hx-target'='#editor',
			   'hx-push-url'=true
			  ],["Save"]),
			div([id=status],[]),
			input([type=hidden,
			       value=VersionId,
			       name='versionId']),
			textarea([style="height:80vh;width:50vw;white-space:pre",
				  rows="20",
				  cols="80",
				  id="editor",
				  name="editor",
				  'hx-get'='/song_preview',
				  'hx-trigger'='keyup delay:500ms',
				  'hx-target'='#preview'
				 ],[Text])]))
	     ])).


disp_lyrics(Id, VersionId, Text) -->    
    html(div([id("disp_edit")],
	     [a([href='/song?edit=true&id='+Id+'&versionId='+VersionId],["Edit"]),		    
	      pre([style="height:80vh;width:50vw;white-space:pre"], [Text])])).

get_song_preview(Respond) :-
    http_current_request(R),    
    http_parameters(R, [versionId(VersionIdA, [optional(true)]),
			editor(Text, [optional(true)])
		       ]),
    (  nonvar(VersionIdA)
    -> atom_number(VersionIdA, VersionId),
       song_version(_, VersionId, _, Text),       
       get_song_preview(Respond, Text)
    ; (  nonvar(Text)
      -> get_song_preview(Respond, Text)
      ;  http_status_reply(bad_request(missing_parameter(oneof([versionId,text]))))
      )
    ).

get_song_preview(Respond, Text) :-
    tmp_file(preview, Path),
    open(Path, write, Stream),
    write(Stream, Text),
    close(Stream),
    latex_song(Path, Pngs),
    call(Respond, div(\disp_pngs(Pngs))).

post_save_song(_) :-
    http_current_request(R),    
    http_parameters(R, [editor(TextA, []),
			versionId(VersionIdA, [])
		       ]),
    atom_number(VersionIdA, VersionId), % TODO declare parameters
    atom_string(TextA, Text),
    song_version(Id, VersionId, _, _),
    add_song_version(Id, VersionId1, unknown, Text),
    atomic_list_concat(['/song?id=',Id,'&versionId=', VersionId1], RedirectTo),
    % TODO: rework this part to htmx request with status output
    http_redirect(see_other,RedirectTo,R).


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
            meta([charset='UTF-8'], []),
            meta([name='viewport', content='width=device-width, initial-scale=1.0'], []),
	    script([src("https://unpkg.com/htmx.org@1.9.2"),
		    integrity("sha384-L6OqL9pRWyyFU3+/bjdSri+iIphTN/bvYyM37tICVyOJkWZLpP2vGn6VUEXgzg6h"),
		    crossorigin("anonymous")],[]),
	    script([src("https://unpkg.com/htmx.org/dist/ext/sse.js")],[]),
	    script([src("https://unpkg.com/hyperscript.org@0.9.11")],[]),	    
            script([src="https://cdn.tailwindcss.com"], [])
           ].


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
                    div([id=body, 'hx-boost'=true],
                        [ \navbar,
                          h1([class="text-2xl font-bold mb-4"], Title),
                          div([class="content"], Body)
                        ])).

navbar -->  
    html(div([class="mb-4 border-b border-gray-200"],
             ul([class="flex flex-wrap -mb-px"],
                [ li([class="mr-2"], 
                     a([href="/songs", 
                        class="inline-block p-4 border-b-2 border-transparent rounded-t-lg hover:text-gray-600 hover:border-gray-300"], 
                       ["Songs"])),
                  li([class="mr-2"], 
                     a([href="/songbooks", 
                        class="inline-block p-4 border-b-2 border-transparent rounded-t-lg hover:text-gray-600 hover:border-gray-300"], 
                       ["Songbooks"]))
                ]))).


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

% Higher-order predicate to map transformation over list
map_list(_, []) --> [].
map_list(Transform, [H|T]) -->
    call(Transform, H),
    map_list(Transform, T).
