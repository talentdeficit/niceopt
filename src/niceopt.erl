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
    labels = string
}).


parse(Cmd, Options) ->
    parse(Cmd, [], [], parse_options(Options, #options{})).
    
parse_options([{opts_with_args, Val}|Rest], OptsRec) when is_list(Val) ->
    parse_options(Rest, OptsRec#options{opts_with_args = Val});
parse_options([{labels, atom}|Rest], OptsRec) ->
    parse_options(Rest, OptsRec#options{labels = atom});
parse_options([{labels, string}|Rest], OptsRec) ->
    parse_options(Rest, OptsRec#options{labels = string});
parse_options([], OptsRec) ->
    OptsRec.
    

parse([], Opts, Args, _OptsRec) ->
    {ok, {lists:reverse(Opts), lists:reverse(Args)}};
parse(Cmd, Opts, Args, OptsRec) ->
    case Cmd of
        [[$-, $-|Long]|Rest] -> parse_long(Long, Rest, Opts, Args, OptsRec)
        ; [[$-|Short]|Rest] -> parse_short(Short, Rest, Opts, Args, OptsRec)
        ; [Arg|Rest] -> parse(Rest, Opts, [Arg] ++ Args, OptsRec)
    end.


parse_long(Long, Rest, Opts, Args, OptsRec) ->
    case split(Long) of
        {Key, true} -> maybe_arg(Key, Rest, Opts, Args, OptsRec)
        ; {Key, Val} -> parse(Rest, [{label(Key, OptsRec), Val}] ++ Opts, Args, OptsRec)
    end.


parse_short([], Rest, Opts, Args, OptsRec) ->
    parse(Rest, Opts, Args, OptsRec);
parse_short([S], [[$-|_]|_] = Rest, Opts, Args, OptsRec) ->
    parse(Rest, [{label(S, OptsRec), true}] ++ Opts, Args, OptsRec);
parse_short([S], [Arg|Rest], Opts, Args, OptsRec) ->
    case has_arg([S], OptsRec) of
        true -> parse(Rest, [{label(S, OptsRec), Arg}] ++ Opts, Args, OptsRec)
        ; false -> parse(Rest, [{label(S, OptsRec), true}] ++ Opts, [Arg] ++ Args, OptsRec)
    end;
parse_short([S|Shorts], Rest, Opts, Args, OptsRec) ->
    case has_arg([S], OptsRec) of
        true -> parse(Rest, [{label(S, OptsRec), Shorts}] ++ Opts, Args, OptsRec)
        ; false -> parse_short(Shorts, Rest, [{label(S, OptsRec), true}] ++ Opts, Args, OptsRec)
    end.

    
maybe_arg(Key, [], Opts, Args, OptsRec) ->
    parse([], [{label(Key, OptsRec), true}] ++ Opts, Args, OptsRec);
maybe_arg(Key, [[$-|_]|_] = Rest, Opts, Args, OptsRec) ->
    parse(Rest, [{label(Key, OptsRec), true}] ++ Opts, Args, OptsRec);
maybe_arg(Key, [Arg|Rest], Opts, Args, OptsRec) ->
    case has_arg(Key, OptsRec) of
        true -> parse(Rest, [{label(Key, OptsRec), Arg}] ++ Opts, Args, OptsRec)
        ; false -> parse([Arg] ++ Rest, [{label(Key, OptsRec), true}] ++ Opts, Args, OptsRec)
    end.
    

split(Opt) ->
    split(Opt, []).
    
split([], Key) ->
    {lists:reverse(Key), true};
%% escaped = in key
split([$\\, $=|T], Key) ->
    split(T, [$=] ++ Key);    
split([$=|Val], Key) ->
    {lists:reverse(Key), Val};    
split([H|T], Key) ->
    split(T, [H] ++ Key).


has_arg(Key, OptsRec) ->
    lists:member(Key, OptsRec#options.opts_with_args).
    
    
label(X, OptsRec) when is_integer(X) ->
    label([X], OptsRec);
label(Key, OptsRec) ->
    case OptsRec#options.labels of
        string -> Key
        ; atom -> list_to_atom(Key)
    end.
    
    
%% eunit tests
-ifdef(EUNIT).

short_test() ->
    ?assert(?MODULE:parse(["-a", "-ab", "-abc"], []) =:= {ok, {[{"a", true}, {"a", true}, {"b", true}, {"a", true}, {"b", true}, {"c", true}], []}}).

short_with_args_test() ->
    ?assert(?MODULE:parse(["-aarg", "-a", "-baarg", "-a", "arg", "arg"], [{opts_with_args, ["a"]}]) =:= {ok, {[{"a", "arg"}, {"a", true}, {"b", true}, {"a", "arg"}, {"a", "arg"}], ["arg"]}}).

long_test() ->
    ?assert(?MODULE:parse(["--hi", "--there"], []) =:= {ok, {[{"hi", true}, {"there", true}], []}}).
    
long_with_args_test() ->
    ?assert(?MODULE:parse(["--key=value", "--key", "--key", "value", "value"], [{opts_with_args, ["key"]}]) =:= {ok, {[{"key", "value"}, {"key", true}, {"key", "value"}], ["value"]}}).

all_args_test() ->
    ?assert(?MODULE:parse(["some", "random", "words"], []) =:= {ok, {[], ["some", "random", "words"]}}).
    
mixed_test() ->
    ?assert(
        ?MODULE:parse(["-aarg", "--key=value", "arg", "-a", "--key", "value", "--novalue", "-a", "-bc", "another arg"], [{opts_with_args, ["a", "key"]}]) 
            =:= {ok, {[{"a", "arg"}, {"key", "value"}, {"a", true}, {"key", "value"}, {"novalue", true}, {"a", true}, {"b", true}, {"c", true}], ["arg", "another arg"]}}).
            
labels_as_atoms_test() ->
    ?assert(?MODULE:parse(["-a", "--long"], [{labels, atom}]) =:= {ok, {[{a, true}, {long, true}], []}}).
            
-endif.