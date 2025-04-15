# erlarg

A lib that parsed a list of strings into a structured tree.  
useful for handling options and parameters in escript 

## install

add these lines to your `rebar.config`:
```erlang
{deps, [...
 {erlarg, {git, "https://github.com/eptwalabha/erlarg.git", {branch, "master"}}}
 ]}.

# if you plan on building an escript
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
