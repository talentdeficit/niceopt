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


-module(niceopt_test).
-author("alisdairsullivan@yahoo.ca").

-include_lib("eunit/include/eunit.hrl").


short_test() ->
    ?assert(niceopt:parse(["-a", "-ab", "-abc"], []) =:= {[{a, true}, {a, true}, {b, true}, {a, true}, {b, true}, {c, true}], []}).

short_with_args_test() ->
    ?assert(niceopt:parse(["-aarg", "-a", "-baarg", "-a", "arg", "arg"], [{opts_with_vals, ["a"]}]) =:= {[{a, "arg"}, {a, true}, {b, true}, {a, "arg"}, {a, "arg"}], ["arg"]}).

long_test() ->
    ?assert(niceopt:parse(["--hi", "--there"], []) =:={[{hi, true}, {there, true}], []}).
    
long_with_args_test() ->
    ?assert(niceopt:parse(["--key=value", "--key", "--key", "value", "value"], [{opts_with_vals, ["key"]}]) =:= {[{key, "value"}, {key, true}, {key, "value"}], ["value"]}).

all_args_test() ->
    ?assert(niceopt:parse(["some", "random", "words"], []) =:= {[], ["some", "random", "words"]}).
    
mixed_test() ->
    ?assert(
        niceopt:parse(["-aarg", "--key=value", "arg", "-a", "--key", "value", "--novalue", "-a", "-bc", "another arg", "--path", "/usr/bin"], [{opts_with_vals, ["a", "key", "path"]}]) 
            =:= {[{a, "arg"}, {key, "value"}, {a, true}, {key, "value"}, {novalue, true}, {a, true}, {b, true}, {c, true}, {path, "/usr/bin"}], ["arg", "another arg"]}).
            
win_test() ->
    ?assert(niceopt:parse(["/a/b/c", "/d", "/key:value", "arg"], [{mode, win}]) =:= {[{a, true}, {b, true}, {c, true}, {d, true}, {key, "value"}], ["arg"]}).
    
early_termination_test() ->
    ?assert(niceopt:parse(["-a", "--", "-a", "--a"], []) =:= {[{a, true}], ["-a", "--a"]}).
    
early_termination_win_test() ->
    ?assert(niceopt:parse(["/a", "--", "/a", "/a"], [{mode, win}]) =:= {[{a, true}], ["/a", "/a"]}).
            
labels_as_strings_test() ->
    ?assert(niceopt:parse(["-a", "--long", "--key=value"], [{labels, string}]) =:= {[{"a", true}, {"long", true}, {"key", "value"}], []}).
    
unicode_test() ->
    ?assert(niceopt:parse([[45, 194, 162, 226, 130, 172, 240, 164, 173, 162]], [{labels, string}]) =:= {[{[194, 162], true}, {[226, 130, 172], true}, {[240, 164, 173, 162], true}], []}).