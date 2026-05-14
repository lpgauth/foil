# foil

High-Performance Erlang Cache Compiler

![Build Status](https://github.com/lpgauth/foil/workflows/Erlang%20CI/badge.svg)

### Requirements

* Erlang/OTP 25+

## About

Foil is a cache that compiles key-values into Erlang modules. Key-values can be namespaced and are backed by an ETS table for easy re-compilation.

## Performance characteristics

`foil:load/1` rebuilds the entire compiled module for a namespace — O(n) in the number of keys. The compile is fast but not free: a 10k-entry namespace compiles in ~150ms on a recent x86_64 box, ~1.5ms per key amortized.

`foil:lookup/2` is a direct function-clause match plus one ETS hop (the namespace → module table). With `foil_direct_*` (caller inlines `Module:lookup/1`) lookups are 0.085–0.166 μs at p50; ETS is 0.311–0.646 μs. The break-even depends on read/write ratio:

- **Reads dominate, namespace stable:** foil wins consistently. Compile cost amortizes.
- **Read/write parity, namespace small (<100 keys):** ETS is competitive once you factor compile cost back in.
- **Write-heavy:** ETS is the better tool. foil's `insert/3` is cheap, but you still need a `load/1` to make changes visible to `lookup/2`.

The cache hits its sweet spot for read-heavy lookup tables (route maps, feature flags, ID-to-name translations) that change rarely but are read on every request.

## API

[Function reference on hexdocs](https://hexdocs.pm/foil/)

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
make test
```
## License
```license
The MIT License (MIT)

Copyright (c) 2017-2026 Louis-Philippe Gauthier

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
