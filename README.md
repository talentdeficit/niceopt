# 'nice' command line parsing for escripts

***niceopt*** provides a nice way to parse escript command line arguments. escript arguments are lists of strings with no differentiation between arguments, options and option values apart from that encoded within the strings themselves. the unix standard (getopt and getopt long) and windows standard (inherited from DOS) are convenient for script authors and users but are slightly at odds with the erlangy way to handle options to functions, proplists. niceopt attempts to bridge this gap by parsing getopt/DOS command lines into proplists of options and lists of arguments with as minimal an interface as possible.

like so:

    Cmd = ["-abc", "-d" "--hi", "--key=value", "alisdair"],
    {[a, b, c, d, hi, {key, "value"}], ["alisdair"]} = niceopt:parse(Cmd, []).

or:
	
    Cmd = ["\a\b\c", "\d", "\hi", "\key:value", "alisdair"],
	{[a, b, c, d, hi, {key, "value"}], ["alisdair"]} = niceopt:parse(Cmd, [{mode, win}]).


### interface

only one function is exported, `parse/2`. it takes the command line to be parsed and a proplist of options and returns a 2-tuple of lists, the first a proplist of the options and values, the second a list of strings representing arguments. options available are:

* `opts\_with\_vals`: a list of options which may have values and may not use the explicit `option=value`/`option:value` syntax

* `labels`: the atom `string` or `atom`, controls whether options are returned as strings or atoms in the proplist. default is atom.

* `mode`: the atom `win` or `unix`, selects the syntax of the command line argument to be parsed. see below for details.


### command line syntax

niceopt's unix mode mimics getopt as closely as possible without requiring a lot of interface overhead

short options are specified with a single dash and a single character. utf8 characters that represent a single codepoint but which are multiple bytes are handled correctly, though they may result in unexpected or odd output. a single dash followed by multiple characters is parsed as if expanded like: `-abcd` -> `-a -b -c -d`. if a short option has a possible value (as indicated by the value of `opts\_with\_vals` in the options proplist) it will capture the following character sequence, even if seperated by whitespace, unless it starts with a `-`

long options are specified with two consecutive dashes and a sequence of characters terminated by whitespace or `=`. anything following the `=` (or whitespace, if the long option has a possible value as with short options above) is it's value. you can include `=` in the long option label by escaping it with a preceding backslash: `\=`

the sequence `--`, terminated by whitespace, is a flag to niceopt to cease parsing options and consider everything following as an argument

anything else is considered an argument

niceopt does not parse partial long opts like getopt, because that requires declaring every possible option ahead of time, the exact reason i wrote niceopt instead of using a straight getopt port


niceopt's windows mode mimics DOS command line arguments, which are super simple

anything starting with a forward slash `/` is an option, terminated by another `/`, whitespace or the value seperator, `:`. anything following the value seperator (up to whitespace) is it's value

anything else is an argument

note that `escript\opt:val\opt` may not be valid on your platform, the first option may have to be preceded by whitespace (`escript \opt:val\opt`)


### notes

the tests in the module require eunit and that you compile with the macro test defined (pass the option {d, test} to the compiler). they are not even close to comprehensive

this readme is really terrible, it's shameful i haven't fixed it yet

thanks to ttmrichter for feedback and suggestions

bugfixes, improvements and feature requests welcomed