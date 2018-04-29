# foil

High-Performance Erlang Cache Compiler

[![Build Status](https://travis-ci.org/lpgauth/foil.svg?branch=master)](https://travis-ci.org/lpgauth/foil)
[![Coverage Status](https://coveralls.io/repos/github/lpgauth/foil/badge.svg?branch=master)](https://coveralls.io/github/lpgauth/foil?branch=master)

## About

Foil is a cache that compiles key-values into Erlang modules. Key-values can be namespaced and are backed by an ETS table for easy re-compilation.

## API

<a href="https://github.com/lpgauth/foil/blob/master/doc/foil.md#index" class="module">Function Index</a>

## Benchmarks

```
make bench
Running bin/bench.sh...
===> Verifying dependencies...
===> Compiling foil
                   name   mean    p99   p999
               ets_atom  0.311  0.374  0.685
     foil_indirect_atom  0.190  0.320  1.940
       foil_direct_atom  0.085  0.106  0.280
             ets_binary  0.489  0.617  0.866
   foil_indirect_binary  0.253  0.311  0.749
     foil_direct_binary  0.135  0.209  0.637
            ets_complex  0.646  0.839  1.190
  foil_indirect_complex  0.276  0.295  0.542
    foil_direct_complex  0.166  0.196  0.451
               ets_list  0.427  0.544  0.823
     foil_indirect_list  0.253  1.730  2.000
       foil_direct_list  0.109  0.129  0.305
              ets_tuple  0.427  0.660  0.965
    foil_indirect_tuple  0.206  0.289  2.030
      foil_direct_tuple  0.115  0.139  0.314
```

## Examples

```erlang
1> foil_app:start().
{ok, [metal, foil]}

2> foil:new(test).
ok

3> foil:insert(test, key, value).
ok

4> foil:insert(test, key2, <<"foo">>).
ok

5> foil:load(test).
ok

6> test_foil:lookup(key).
{ok, value}

7> foil:lookup(test, key2).
{ok, <<"foo">>}

8> foil:lookup(test, key3).
{error, key_not_found}
```

## Tests

```makefile
make dialyzer
make elvis
make eunit
make xref
```
## License
```license
The MIT License (MIT)

Copyright (c) 2017 Louis-Philippe Gauthier

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
