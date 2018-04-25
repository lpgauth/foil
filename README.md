# foil

High-Performance Erlang Cache Compiler

[![Build Status](https://travis-ci.org/lpgauth/foil.svg?branch=master)](https://travis-ci.org/lpgauth/foil)
[![Coverage Status](https://coveralls.io/repos/github/lpgauth/foil/badge.svg?branch=master)](https://coveralls.io/github/lpgauth/foil?branch=master)

## About

Foil is a cache that compiles key-values into Erlang modules. Key-values can be namespaced and are backed by an ETS table for easy re-compilation.

## API

<a href="https://github.com/lpgauth/foil/blob/master/doc/foil.md#index" class="module">Function Index</a>

## Examples


```
erl -pa  _build/compile/lib/*/ebin
Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:10] [kernel-poll:false]

Eshell V9.3  (abort with ^G)
```

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

9> foil:new(complex_test).
ok

10> Pid = spawn(timer, sleep, [100000]).      
<0.70.0>

11> foil:insert(complex_test, pid, Pid).
ok

12> Ref = make_ref().
#Ref<0.2665151396.1923612679.12103>

13> foil:insert(complex_test, ref, Ref).
ok

14> ComplexTerm = [Ref, {my_sleepy_pid, Pid}, [1,2,9.5], [{one, "one"}, {two, "two"}, {pid_again, Pid}]].

15> foil:insert(complex_test, term, ComplexTerm).
ok

16> foil:load(complex_test).
ok

17>{ok, Pid} = complex_test_foil:lookup(pid).
18>{current_function,{timer,sleep,1}} = process_info(Pid, current_function).

19>{ok, Ref} = complex_test_foil:lookup(ref).

20>{ok, ComplexTerm} = complex_test_foil:lookup(term).

21>foil:delete(complex_test).
ok

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
