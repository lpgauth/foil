# foil

High-Performance Erlang Cache Compiler

[![Build Status](https://travis-ci.org/lpgauth/foil.svg?branch=master)](https://travis-ci.org/lpgauth/foil)
[![Coverage Status](https://coveralls.io/repos/github/lpgauth/foil/badge.svg?branch=master)](https://coveralls.io/github/lpgauth/foil?branch=master)

## About

Foil is a cache that compiles key-values into Erlang modules. Key-values can be namespaced and are backed by an ETS table for easy re-compilation.

## API

<a href="https://github.com/lpgauth/foil/blob/master/doc/foil.md#index" class="module">Function Index</a>

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
