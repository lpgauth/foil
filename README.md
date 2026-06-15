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

## Elixir

Foil ships an `Foil` shim module, so the same API is available from
Elixir with no extra dependency:

```elixir
iex> :foil_app.start()
{:ok, [:metal, :foil]}

iex> Foil.new(:test)
:ok

iex> Foil.insert(:test, :key, "value")
:ok

iex> Foil.load(:test)
:ok

iex> Foil.lookup(:test, :key)
{:ok, "value"}

iex> Foil.namespaces()
{:ok, [:test]}
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
