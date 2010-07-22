# 'nice' command line parsing for escripts

***niceopt*** parses escript arguments like a slightly less strict getopt. getopt requires you to specify legal options and whether they have optional or mandatory or no values and et cetera. niceopt doesn't care. niceopt just needs to know what option flags might have optional values and it'll return a proplist of option flags (and their values, if any) in the command line, and a list of arguments.


### like so

    Cmd = ["-abc", "-d" "--hi", "alisdair"],
    {ok, {["a", "b", "c", "d", "hi"], ["alisdair"]}} = niceopt:parse(Cmd, []).

    Cmd = ["-abc", "-d", "--hi", "alisdair"],
    {ok, {[{"a", "bc"}, "d", {"hi", alisdair}], []}} = niceopt:parse(Cmd, [{opts_with_args, ["a", "hi"]}]).

    Cmd = ["argument", "-s", "--long=value"],
    {ok, {["s", {"long", "value"}], ["argument"]}} = niceopt:parse(Cmd, []).

you can also get options as atoms in your proplist, if you're into that:

    Cmd = ["-abc", "-d", "--hi", "alisdair"],
    {ok, {[{a, "bc"}, d, {hi, "alisdair"}], []}} = niceopt:parse(Cmd, [{opts_with_args, ["a", "hi"]}, {labels, atom}]).


### notes

niceopt understands utf8, and is smart enough to treat multi-byte characters as single characters when used as shorts, but exploiting this is probably questionable

the tests in the module require eunit and that you compile with the macro test defined (pass the option {d, test} to the compiler). they are not even close to comprehensive

bugfixes, improvements and feature requests welcomed