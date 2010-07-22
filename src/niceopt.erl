%% The MIT License

%% Copyright (c) 2010 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.


-module(niceopt).
-author("alisdairsullivan@yahoo.ca").

-export([parse/2]).


-ifdef(test).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(options, {
    opts_with_args = [],
    labels = string,
    win_mode = false
}).


parse(Cmd, Options) ->
    parse(Cmd, [], [], parse_options(Options, #options{})).
    
parse_options([{opts_with_args, Val}|Rest], OptsRec) when is_list(Val) ->
    parse_options(Rest, OptsRec#options{opts_with_args = Val});
parse_options([{labels, atom}|Rest], OptsRec) ->
    parse_options(Rest, OptsRec#options{labels = atom});
parse_options([{labels, string}|Rest], OptsRec) ->
    parse_options(Rest, OptsRec#options{labels = string});
parse_options([{win_mode, Val}|Rest], OptsRec) ->
    parse_options(Rest, OptsRec#options{win_mode = Val});
parse_options([], OptsRec) ->
    OptsRec.
    

parse([], Opts, Args, _OptsRec) ->
    {ok, {lists:reverse(Opts), lists:reverse(Args)}};
parse(Cmd, Opts, Args, OptsRec) when element(#options.win_mode, OptsRec) =:= true ->
    case Cmd of
        [[$/|Win]|Rest] -> 
            parse_win(Win, Rest, Opts, Args, OptsRec)
        ; [Arg|Rest] -> parse(Rest, Opts, [Arg] ++ Args, OptsRec)
    end;
parse(Cmd, Opts, Args, OptsRec) ->
    case Cmd of
        [[$-, $-|Long]|Rest] -> parse_long(Long, Rest, Opts, Args, OptsRec)
        ; [[$-|Short]|Rest] -> parse_short(Short, Rest, Opts, Args, OptsRec)
        ; [Arg|Rest] -> parse(Rest, Opts, [Arg] ++ Args, OptsRec)
    end.  


parse_long(Long, Rest, Opts, Args, OptsRec) ->
    case split(Long, "=") of
        {Key, true} -> maybe_arg(Key, Rest, Opts, Args, OptsRec)
        ; {Key, Val} -> parse(Rest, [{label(Key, OptsRec), Val}] ++ Opts, Args, OptsRec)
    end.


parse_short([], Rest, Opts, Args, OptsRec) ->
    parse(Rest, Opts, Args, OptsRec);
parse_short(S, Rest, Opts, Args, OptsRec) ->
    {Short, Shorts} = get_short(S),
    case Shorts of
        [] -> maybe_arg(Short, Rest, Opts, Args, OptsRec)
        ; _ -> 
            case has_arg(Short, OptsRec) of
                true -> parse(Rest, [{label(Short, OptsRec), Shorts}] ++ Opts, Args, OptsRec)
                ; false -> parse_short(Shorts, Rest, [label(Short, OptsRec)] ++ Opts, Args, OptsRec)
            end
    end.
    

parse_win([], Rest, Opts, Args, OptsRec) ->
    parse(Rest, Opts, Args, OptsRec);
parse_win(Win, Rest, Opts, Args, OptsRec) ->
    {Opt, Others} = get_win_opt(Win),
    case split(Opt, ":") of
        {Key, true} -> parse_win(Others, Rest, [label(Key, OptsRec)] ++ Opts, Args, OptsRec)
        ; {Key, Value} -> parse_win(Others, Rest, [{label(Key, OptsRec), Value}] ++ Opts, Args, OptsRec)
    end.

    
maybe_arg(Key, [], Opts, Args, OptsRec) ->
    parse([], [label(Key, OptsRec)] ++ Opts, Args, OptsRec);
maybe_arg(Key, [[$-|_]|_] = Rest, Opts, Args, OptsRec) ->
    parse(Rest, [label(Key, OptsRec)] ++ Opts, Args, OptsRec);
maybe_arg(Key, [Arg|Rest], Opts, Args, OptsRec) ->
    case has_arg(Key, OptsRec) of
        true -> parse(Rest, [{label(Key, OptsRec), Arg}] ++ Opts, Args, OptsRec)
        ; false -> parse(Rest, [label(Key, OptsRec)] ++ Opts, [Arg] ++ Args, OptsRec)
    end.


has_arg(Key, OptsRec) ->
    lists:member(Key, OptsRec#options.opts_with_args).
    

label(X, OptsRec) when is_integer(X) ->
    label([X], OptsRec);
label(Key, OptsRec) ->
    case OptsRec#options.labels of
        string -> Key
        ; atom -> list_to_atom(Key)
    end.

    
split(Opt, SChar) ->
    split(Opt, SChar, []).

split([], _SChar, Key) ->
    {lists:reverse(Key), true};     
split([H|T], [SChar], Key) ->
    case H =:= SChar of
        true -> {lists:reverse(Key), T}
        ; false -> split(T, [SChar], [H] ++ Key)
    end.
    

get_short([S|Rest]) when S =< 127 -> {[S], Rest};
get_short([S, T|Rest]) when S >= 192, S =< 223 -> {[S, T], Rest};
get_short([S, T, U|Rest]) when S >= 224, S =< 239 -> {[S, T, U], Rest};
get_short([S, T, U, V|Rest]) when S >= 240, S =< 247 -> {[S, T, U, V], Rest}.


get_win_opt(Opt) ->
    get_win_opt(Opt, []).
    
get_win_opt([], Opt) ->
    {lists:reverse(Opt), []};    
get_win_opt([$/|T], Opt) ->
    {lists:reverse(Opt), T};    
get_win_opt([H|T], Opt) ->
    get_win_opt(T, [H] ++ Opt).
    
    
%% eunit tests
-ifdef(EUNIT).

short_test() ->
    ?assert(?MODULE:parse(["-a", "-ab", "-abc"], []) =:= {ok, {["a", "a", "b", "a", "b", "c"], []}}).

short_with_args_test() ->
    ?assert(?MODULE:parse(["-aarg", "-a", "-baarg", "-a", "arg", "arg"], [{opts_with_args, ["a"]}]) =:= {ok, {[{"a", "arg"}, "a", "b", {"a", "arg"}, {"a", "arg"}], ["arg"]}}).

long_test() ->
    ?assert(?MODULE:parse(["--hi", "--there"], []) =:= {ok, {["hi", "there"], []}}).
    
long_with_args_test() ->
    ?assert(?MODULE:parse(["--key=value", "--key", "--key", "value", "value"], [{opts_with_args, ["key"]}]) =:= {ok, {[{"key", "value"}, "key", {"key", "value"}], ["value"]}}).

all_args_test() ->
    ?assert(?MODULE:parse(["some", "random", "words"], []) =:= {ok, {[], ["some", "random", "words"]}}).
    
mixed_test() ->
    ?assert(
        ?MODULE:parse(["-aarg", "--key=value", "arg", "-a", "--key", "value", "--novalue", "-a", "-bc", "another arg", "--path", "/usr/bin"], [{opts_with_args, ["a", "key", "path"]}]) 
            =:= {ok, {[{"a", "arg"}, {"key", "value"}, "a", {"key", "value"}, "novalue", "a", "b", "c", {"path", "/usr/bin"}], ["arg", "another arg"]}}).
            
win_test() ->
    ?assert(?MODULE:parse(["/a/b/c", "/d", "/key:value", "arg"], [{win_mode, true}]) =:= {ok, {["a", "b", "c", "d", {"key", "value"}], ["arg"]}}).
            
labels_as_atoms_test() ->
    ?assert(?MODULE:parse(["-a", "--long"], [{labels, atom}]) =:= {ok, {[a, long], []}}).
    
unicode_test() ->
    ?assert(?MODULE:parse([[45, 194, 162, 226, 130, 172, 240, 164, 173, 162]], []) =:= {ok, {[[194, 162], [226, 130, 172], [240, 164, 173, 162]], []}}).
            
-endif.