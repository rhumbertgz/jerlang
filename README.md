JErlang: Erlang with Joins
=====
This repository contains an extract of the original JErlang's implementation available here: https://web.archive.org/web/20160405003024/http://www.doc.ic.ac.uk:80/~susan/jerlang/. 

The code published is this repo focus only on the _library_ implementation. We did small updates to the original code implemented in R12B-3 to run in a modern Erlang/OPT (version 23).
We also encapsulated the code as a rebar3 library project. As a consequence, small changes in the structure of the original were made.

All the credit of the code in this repository belongs to JErlang's author *Hubert Plociniczak*.

Build JErlang
-------------

    $ rebar3 compile

Add JErlang as a source dependency to your project
--------------------------------------------------
```
{deps,
    [
        {rebar, {git, "git://github.com/rhumbertgz/jerlang.git"}},
    ]
}.
```

