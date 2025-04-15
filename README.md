# erlarg

An Erlang lib that parsed a list of strings into a structured tree.  
useful for handling options and parameters in escript 

> [!WARNING]
> This lib is still under developement and is likely to change in the future  
> Keep this in mind. If you need a working alternative, you can use [getopt](https://github.com/jcomellas/getopt).

## install

add these lines to your `rebar.config`:
```erlang
{deps, [...
 {erlarg, {git, "https://github.com/eptwalabha/erlarg.git", {branch, "master"}}}
 ]}.

% if you plan on building an escript
{escript_incl_apps, [...
 erlarg
 ]}.
```
fetch and compile the dependencies of your project with:
```bash
rebar3 compile --deps_only
```
That's it, you're done.

## How to use ?

I'll describe in more details how to use this lib.  
Meanwhile, you can have a look at the tests to see how things work.
