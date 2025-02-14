% -*- mode: prolog -*-

:- use_module(db).
:- use_module(library(clpfd)).

:- set_prolog_flag(double_quotes, chars).
       
parse_song(String, Header, Body) :-
    string_chars(String, Chars),
    phrase(songp(HeaderC, Body), Chars),
    maplist(strings, HeaderC, Header).

strings(KC-VC, KS-VS) :-
    string_chars(KS,KC),
    string_chars(VS,VC).

songp(Header, Body) --> header(Header),
			body(Body),
			skip_after("***").

skip_after(Key) --> empty_lines_, Key, ... .
skip_after(_) --> [].

 ...  --> [] | [_], ... .

key_values([Key-Value|Rest]) --> key_value(Key, Value),
				 key_values(Rest).				 
key_values([]) --> [].
header(Header) --> key_values(Header),
		   "***",
		   whitespace,
		   newline.

key_value(Key, Value) --> keyword(Key),
			  whitespace,
			  ":",
			  whitespace,
			  (value(Value) | empty(Value)),
			  newline.

whitespace --> counted_whitespace(_).

counted_whitespace(N) --> [W],
			  {char_type(W, white)},
			  counted_whitespace(N0),
			  {N #= N0+1}.
counted_whitespace(0) --> [].

keyword(Keyword) --> nonempty_list_of(alphabetic, keyword_char, Keyword).

value(Value) --> nonempty_list_of(printable, not_newline, Value).

empty([]) --> [].

nonempty_list_of(Pred, List) -->
    nonempty_list_of(Pred, Pred, List).

nonempty_list_of(InitPred, Pred, [Something|Rest]) -->
    [Something],
    { call(InitPred, Something) },
    list_of(Pred, Rest).
    
list_of(Pred, [Something|Rest]) -->
    [Something],
    { call(Pred, Something), ! },
    list_of(Pred, Rest).
list_of(_, []) --> [].

newline --> "\n".

empty_lines --> empty_line,
		empty_lines_.
empty_lines_ --> empty_line,
		 empty_lines_.
empty_lines_ --> [].
		     
empty_line --> whitespace, newline.

body([I|Body]) --> instruction(I),
		  body(Body).
body([V|Rest]) --> verse(V),
		   body_(Rest).

body_([I|Body]) --> instruction(I),
		   body_(Body).
body_([V|Rest]) --> empty_lines,
		    verse(V),		    
		    body_(Rest).
body_([]) --> [].

verse(v([Chords-Line|Rest])) --> chords_line(Chords),
				 lyrics_line(Line),
				 verse(v(Rest)).
verse(v([Line|Rest])) --> integrated_chords_line(Line),
			  verse(v(Rest)).
verse(v([''-Line|Rest])) --> lyrics_line(Line),
			     verse(v(Rest)).
verse(v([])) --> [].

lookahead_chord_len(Len, List, List) :-
    phrase(nonempty_list_of(printable, printable, Chord),List,_),
    length(Chord, Len).

chords_line(cl(Line)) --> chords_line_(0, Line),
			  { dif(Line, []) }.

chords_line_(StartAt, [chord(Chord, RelPos)|Rest]) -->
    counted_whitespace(Count),
    (
	chord_len(Chord, Len) | chord_comment(Chord, Len)
    ),
    {
	RelPos #= StartAt + Count,
	NextPos #= RelPos + Len
    },
    chords_line_(NextPos, Rest).
chords_line_(_, []) --> whitespace, newline.

chord_len(Chord, Len) -->
    lookahead_chord_len(Len),
    chord(Chord),
    chord_end.

chord_end,[W] --> [W],
		  { char_type(W, space) }.

chord_comment(comment(String), Len) --> "(", list_of(paren_cont, String), ")", {length(String, Len)}.

chord(dc(Regular/Bass)) --> {Regular=c(_,_,_), Bass=c(_,_,_)}, chord(Regular),"/",chord(Bass).
chord(oc(Chord)) --> {Chord=c(_,_,_)}, "(", chord(Chord), ")".
chord(oc(Chord)) --> {Chord=c(_,_,_)}, chord(Chord), "?".

chord(c(Tone,IsMinor,Kind)) --> chord_tone(Tone, MustBeMinor),
				chord_kind(MustBeMinor, IsMinor, Kind).

chord_tone(Tone, -) --> [Tone], { member(Tone, "ABCDEFGH") }.
chord_tone(Tone, -) --> [Base], (['#']|"is"), { member(Base, "ACDEFGH"), atom_concat(Base, '#', Tone) }.
chord_tone(Tone, -) --> [Base], (['b']|"es"), { member(Base, "ABCDEFGH"), atom_concat(Base, 'b', Tone) }.
chord_tone("Eb", -) --> "Es".
chord_tone("Ab", -) --> "As".

chord_tone(Tone, min) --> [Tone0], { member(Tone0, "abcdefgh"), upcase_atom(Tone0, Tone) }.
chord_tone(Tone, min) --> [Base], (['#']|"is"), { member(Base, "abcdefgh"), atom_concat(Base, '#', Tone0), upcase_atom(Tone0, Tone) }.
chord_tone(Tone, min) --> [Base], (['b']|"es"), { member(Base, "abcdefgh"), atom_concat(Base, 'b', Tone0), upcase_atom(Tone0, Tone) }.
chord_tone("Eb", min) --> "es".
chord_tone("Ab", min) --> "as".


chord_kind(MBM, maj, 4) --> {dif(MBM,min)}, "4".
chord_kind(min, min, 4) --> "4".
chord_kind(MBM, maj, 5) --> {dif(MBM,min)}, [] | "M" | "maj".
chord_kind(MBM, dom, 7) --> {dif(MBM,min)}, "7".
chord_kind(MBM, maj, 7) --> {dif(MBM,min)}, "M7" | "Ma7" | "maj7".
chord_kind(min, min, 5) --> [].
chord_kind(MBM, min, 5) --> {dif(MBM,min)}, "m" | "-" | "min".
chord_kind(min, min, 7) --> "7".
chord_kind(_, min, 7) --> "m7" | "min7" | "-7".
chord_kind(MBM, aug, 5) --> {dif(MBM,min)}, "+" | "aug" | "M+5".
chord_kind(MBM, aug, 7) --> {dif(MBM,min)}, "+M7" | "aug7" | "M7" | "CM7+5".



lyrics_line(ll(Chars)) --> nonempty_list_of(not_newline, Chars),
			   newline,
			   { \+ phrase(chords_line(_), Chars) }.
integrated_chords_line(il(Chars)) --> nonempty_list_of(not_newline, Chars), newline,
				      { member('^', Chars) }.
instruction(latex(Instruction)) --> "\\",
				    nonempty_list_of(not_newline, Instruction),
				    newline.

not_newline(C) :- C \= '\n'.
alphabetic(C) :- char_type(C, alpha).
alphanumeric(C) :- char_type(C, alnum).
keyword_char(C) :- char_type(C, alnum) ; member(C, "-_ /,").
printable(C) :- char_type(C, graph).
paren_cont(C) :- C \= '\n', C \= ')'.

writestr(C) :- string_chars(S, C), writeln(S).
