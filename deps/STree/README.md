# s-tree (Haskell)

A **static, pointer-free B-tree** for read-heavy workloads. A sorted array is permuted once
into a generalised Eytzinger layout with fanout `B = L+1`, after which lower and upper bounds
are answered by a branch-free descent doing one node rank per level. Bulk lookups go through a
vectorised descent in C.

The tree is immutable: built once from sorted input, then only queried. The input array can be
discarded — `index` recovers any element from the layout.

Two structures share that layout. `Data.STree.BTree` maps a key to its **position**;
`Data.STree.BPlus` is a **static map**, keeping all keys in the leaves so it can carry values,
answer `keyAt` in `O(1)`, and return ranges as slices. See
[the map variant](#map-variant-a-static-btree) for which to reach for.

## Usage

```haskell
{-# LANGUAGE DataKinds #-}

import Data.Int (Int32)
import qualified Data.Vector.Unboxed as U
import Data.STree

main :: IO ()
main = case build (U.fromList [-3, 2, 4, 11, 35, 60]) :: Either BuildError (BTree 16 Int32) of
  Left err   -> print err
  Right tree -> do
    print (lowerBoundIdx tree 11)   -- 3
    print (lowerBoundIdx tree 12)   -- 4
    print (tree ! 3)                -- 11
```

`cabal run s-tree-example` runs the above and dumps three Graphviz graphs, described under
[Visualising the layout](#visualising-the-layout).

The type parameter of `BTree l a` is the node width `l`, so the fanout is `l + 1`. Supported
widths are `2, 4, 8, 16, 32, 64`. The upper bound of 64 is real — the scalar node rank is
computed in one `Word64` — and 2 is useful mainly for making small trees deep enough to look
at. Key types are the class `Key`, instantiated for `Int`, `Int32`, `Int64`, `Word`, `Word32`,
`Word64`, `Float` and `Double`.

Input must be sorted non-decreasingly and every key must be below `sentinel`
(`maxBound`, or `+Infinity` for floating point, which the padding reserves). `build` checks
both and returns `Left`; `unsafeBuild` skips the checks.

| | |
| --- | --- |
| `build` / `buildFromList` / `unsafeBuild` | construct from sorted input |
| `lowerBoundIdx` / `upperBoundIdx` | index of the first key `>= x` / `> x` |
| `lowerBoundIdxMany` / `upperBoundIdxMany` | the same for many keys at once, vectorised |
| `BP.lowerBoundIdxMany` / `BP.upperBoundIdxMany` | bulk map lookups: vectorised index, scalar leaf |
| `index`, `(!)` | the key at a position of the original input |
| `toVector` | recover the whole sorted input |
| `height`, `size`, `lineSize`, `sizeInBytes` | shape and footprint |
| `logSize @l n` | the height `n` keys would give, without building anything |
| `toDot` / `toDotStyled` | Graphviz rendering of the tree, and of the array it lives in |
| `calibrateBTreeBatch` / `calibrateBTreeScalar` / `calibrateBPlusScalar` | pick the node width(s) by measuring on this machine |
| `writeSTree` / `readSTree` | persist and reload |

## Map variant: a static B+tree

`Data.STree.BPlus` associates a value with every key. It keeps all keys in the leaves, in sorted
order and contiguous, with the internal levels holding only separators copied out of them.

```haskell
import qualified Data.STree.BPlus as BP

Right t = BP.build keys values :: Either BP.BuildError (BP.BPlusTree 16 16 Int32 Int32)
BP.lookup t 42            -- every value stored under 42, in order
BP.rangeValues t 10 20    -- every value whose key is in [10, 20)
BP.keyAt t 7              -- O(1)
```

The type carries two widths: `BPlusTree li ll k v`, where `ll` is the keys per leaf and `li` the
node width of the internal index. They are independent because they are searched under different
conditions — the index is small and stays cached, a leaf is one cold random access — and because
`ll` alone fixes how many separators there are, and so how big the index is.

**It is built out of the B-tree, not alongside it.** The index over the separators is exactly a
`BTree li k`, so that module is used unmodified and none of its layout code is duplicated. What
is new is only the leaf array, the separator extraction, and a two-step descent that has no
counterpart to the B-tree's virtual size, exceeding-leaf bookkeeping or two-branch final index —
with contiguous leaves the position is just `leaf * ll + rank`.

Duplicates are allowed, and `lookup` returns every match: equal keys are adjacent, so the result
is an `O(1)` slice sharing the underlying array.

### Bulk lookups, half-vectorised

`BP.lowerBoundIdxMany` gets most of the vectorised path for free. The index *is* a `BTree`, so
picking a leaf for every query is one call to the existing SIMD kernel; only the final rank inside
the chosen leaf stays in Haskell. **No new C.** At 10^6 keys that is 122 ns per query down to
**18 ns — 6.8x** — against 13 ns for the fully-vectorised B-tree, so it lands within 1.4x of what
a dedicated kernel could do.

**It inverts the index width**: `li` wants to be *wider* on this path, because its descent is
vectorised and fewer levels then wins. Using the scalar optimum costs 2.4x (`li=4 ll=8` measures
41 ns against 17). `calibrateBPlusScalar` times the scalar path, so its answer is the wrong one here.

The leaf width, on the other hand, is almost free — see below.

### How the leaf is searched, and why it decides the leaf width

A leaf rank is the only scalar work left in a bulk lookup, so how it is done sets how much a wide
leaf costs — and a wide leaf is what keeps the index small. Measured over 200k distinct queries at
`li=16`, ns per query:

| ll | 2 | 4 | 8 | 16 | 32 | 64 |
| --- | --- | --- | --- | --- | --- | --- |
| forward scan | 27.1 | 36.1 | 40.8 | **29.6** | 40.8 | 67.4 |
| bisection | 28.3 | 38.7 | 46.7 | 37.3 | 44.1 | **59.0** |

**The scan wins below `ll=64`**, which is the opposite of the comparison count: bisection does
`log2 ll` comparisons against the scan's `ll/2`. That is the right trade when the leaf is already
in cache, and it is what a benchmark of a thousand repeated queries measures — which is how
bisection came to be chosen here first. On a real query stream the leaf is cold, and then the
*shape* of the accesses beats their number: a scan walks forward through one or two cache lines
with nothing to wait on, while each of bisection's addresses depends on the previous comparison,
so its few accesses serialise. Past `ll=32` the leaf spans enough lines that the count wins again,
so the implementation scans below 64 and bisects at 64. `ll` is a compile-time constant, so the
choice folds away.

This is what makes a wide leaf affordable. With scanning, **`ll=16` costs 9% over `ll=2` and
stores an eighth as many separators** (0.24 MB against 1.91); with bisection the same choice cost
32%. Note the curve is not monotone — the dip at `ll=16` is the index height stepping from five
levels to four as the separator count falls.

The leaf width is what decides how much the index has to store:

| ll | 2 | 4 | 8 | 16 | 32 | 64 |
| --- | --- | --- | --- | --- | --- | --- |
| bulk ns/query, **1000 repeated queries** | 19.2 | 16.5 | 17.4 | 17.0 | 18.3 | 19.4 |
| bulk ns/query, **100k distinct queries** | **20.8** | — | — | 34.7 | — | — |
| separators stored | 1.91 MB | 0.95 | 0.48 | 0.24 | 0.12 | **0.06** |

The first row measures the hot-cache regime and is the wrong thing to tune on. **The best leaf
width depends on query locality**, and it flips:

| distinct query pool | 1,000 | 10,000 | 100,000 | 200,000 |
| --- | --- | --- | --- | --- |
| `ll=2` (`li=8`) | 16.5 | **19.9** | **20.8** | **22.2** |
| `ll=16` (`li=16`) | **15.9** | 32.6 | 34.7 | 38.1 |

A thousand queries repeated thousands of times keeps their paths hot and favours the shallower
tree; past about ten thousand distinct queries that stops being true. Those figures were taken
with the bisecting leaf; with the scan the cold-regime gap between `ll=2` and `ll=16` narrows to
about 9%, which is what makes a wide leaf worth taking.

Two things follow. `Data.STree.BTree`'s figures are unaffected: its batch path at `L=16` measures
11.8 ns at every pool size from 1,000 to 200,000, because the whole descent is in C with no scalar
leaf work (`L=8` is not so robust: 22.9 hot against 36.0 cold). And this is a concrete argument for
calibrating rather than copying a table — `calibrateBPlusBulk` gets this right because it times
*your* queries, and it disagreed with the benchmark above until the benchmark was found to be at
fault.

Bisection also needs no sentinel padding, since the partial last leaf is bounded by `size`
directly, and it is `NaN`-safe by construction — a bounded interval cannot be walked past.

### Which to use

| | `BTree` | `BPlusTree` |
| --- | --- | --- |
| carries values | no | yes |
| key storage | exactly `n` keys | `n` plus `n/ll` separators |
| position query, each at its best scalar width | 85 ns (`L=2`) | 88 ns (`li=2 ll=8`) |
| key at a position | 8.8 ns | **1.0 ns** |
| all keys in order | 8.8 ms per 10^6 | **0.66 ms** |
| build | 2.6 ms per 10^6 | **0.28 ms** |
| range scan | walk per element | `O(1)` slice |

Measured at `n = 10^6`, `Int32`, on the machine described under
[Performance](#performance). The two are level on position queries once each is given its best
scalar width; everything else in the table is the B+tree's structural advantage rather than a
tuning difference.

The trade is the separators: `1/ll` more key storage, measured at +6.26% for `ll=16` and +1.6%
for `ll=64` (100k `Int32` keys: 400,000 B for the B-tree against 425,024 B of keys for the
B+tree, plus the values).

Everything else is a win, some of it by more than expected. A **full** `lookup` — descend, check
the key, fetch the value — costs 130 µs per 1000 queries, which is what the B-tree charges for
the bare position alone: the value comes for free relative to it. Building is 9.4x faster,
because the leaves need no permutation at all and only `n/ll` separators do. Recovering the keys
in order is 13x faster, and that gap widens with `n`, since it is `O(1)` against `O(n log_B n)`.

The reason to keep the plain `BTree` is the `1/ll` of key storage, and that it is the structure
the vectorised batch path currently supports.

## Persistence

`Data.STree.Serialize` writes a tree to a file and reads it back without redoing the
permutation. A tree is one flat array plus a few numbers that all follow from its size, so the
file is a 64-byte header followed by the array verbatim — no structure to encode, no
per-element work beyond the copy.

```haskell
import Data.STree.Serialize

writeSTree "index.st" tree
loaded <- readSTree "index.st" :: IO (Either SerializeError (BTree 16 Int32))
```

```
offset  size  field
0       8     magic "STREE\0\0\0"
8       2     format version (u16 LE)
10      1     key type tag (`keyTag`)
11      1     node width l
12      4     flags (u32 LE; bit 0: payload is little-endian)
16      8     number of keys, n (u64 LE)
24      8     payload length in bytes (u64 LE)
32      8     payload checksum, 0 if absent (u64 LE)
40      24    reserved, zero
64      ...   the layout array verbatim, sentinel padding included
```

Only `n` is real metadata; the height, virtual size and both `exceeding` counts are
recomputed on load through the same `geometry` function the builder uses, so the two cannot
drift. The padding is part of the payload because the descent reads it.

**Loading is eager.** `readSTree` returns only once every key is in memory: it allocates the
array up front and fills it with a chunked read through a 1 MiB staging buffer. Peak
residency is the payload plus about a megabyte. Nothing is deferred, so a truncated or
unreadable file fails at load time instead of surfacing an exception from inside a pure query
later. If you ever need an index larger than RAM, `mmap` is the alternative — it would want
the payload moved from `Data.Vector.Unboxed` to `Data.Vector.Storable`.

`writeSTree` replaces the target atomically (temporary file in the same directory, then
`rename`), so an interrupted write cannot leave a file that passes validation with garbage in
its tail. That is not the same as durability: `fsync` the directory yourself if you need to
survive power loss.

What loading checks, and why that is the right amount:

* magic, format version, key tag, node width, and payload endianness — each mismatch is its
  own `SerializeError` rather than a reinterpretation of the bytes;
* the payload length against the file size, in `Integer` and *before* anything is allocated,
  so a corrupt size field cannot become an absurd allocation;
* that the payload length matches the one implied by `n`.

Together those bound every index the descent can compute, which is what makes the query
path's unchecked indexing safe on a foreign file. The array *contents* are not validated —
a garbled payload gives wrong answers, not a crash — because verifying the permutation costs
as much as rebuilding it. The checksum field is reserved for closing that gap; this version
writes 0.

Two portability caveats, both detected rather than silent: the payload is copied raw and so is
host-endian (the flag records which, and a host of the other byte order refuses the file), and
`Int`/`Word` files do not travel across word sizes (the payload length check catches it).

## Bulk lookups

`Data.STree.Batch` answers many queries at once with a vectorised descent in C:

```haskell
import Data.STree.Batch

idxs = lowerBoundIdxMany tree keys   -- == U.map (lowerBoundIdx tree) keys, ~10x faster
```

It returns exactly what `U.map (lowerBoundIdx t)` returns — the test suite checks that against
every node width, key type and size, not merely that it is correct.

**The batch is the unit because the foreign call has to be paid for.** A vectorised node rank
is about a dozen instructions, so calling into C per node, or even per query, would spend the
win on call overhead. One call per few hundred queries makes it free.

### Latency: this does not stall your program

The calls are `unsafe`, which does not release the capability, and GHC's collector is
stop-the-world — so while one runs, no thread anywhere in the program can be collected, and
they all wait. This is not theoretical: a 14 ms `unsafe` call measured here stalled an
unrelated allocating thread for the entire 142 ms run of such calls, where the same work
behind a `safe` call cost that thread 177 us.

Two things keep that from reaching you. The batch is chunked at 256 queries, and — the part
that actually matters — there is an explicit `yield` between chunks. **Chunking on its own does
nothing**, which is a trap worth knowing about: GHC omits yield checks from non-allocating
loops, so the chunk loop ran all its chunks back to back with no safe point, and the pause was
identical to no chunking at all (3.0 ms either way, in a batch of 200k queries). Nor can the
library fix that with `-fno-omit-yields`, because `searchMany` is `INLINABLE` and the loop is
compiled in *your* module, not this one.

With the yield in place, the worst stall an unrelated thread sees during a 200k-query batch is
233-274 us against a 215 us baseline for the same program doing no batching at all — i.e. lost
in that program's own GC pauses. The residual is one chunk, a few microseconds. It costs about
3% of batch throughput; larger chunks do not buy that back, since the cost is the yield itself
rather than its frequency.

If you need even that gone, `safe` calls remove it entirely — but they require the payload in
pinned memory, i.e. moving the layout from `Data.Vector.Unboxed` to `Data.Vector.Storable`.

`cbits/stree_simd.c` uses **GCC/clang vector extensions rather than intrinsics**, so there is
one source for every target and no `#ifdef`: the compiler lowers it to NEON, SSE/AVX, or
scalar code as appropriate. On aarch64 it emits the same instruction sequence as hand-written
NEON. On x86-64 it compiles for the SSE2 baseline — add `cc-options: -mavx2` if you are
building for a machine you control.

The kernel avoids the usual compare-to-mask plus `tzcnt`, which would need x86's `movemask`.
That is unnecessary: since a node's keys are sorted, the rank is just the number of lanes
comparing true, so summing the comparison lanes works everywhere. Three details
decide the generated code, all checked against the disassembly and noted in the C file:
accumulate into one 16-byte vector rather than one L-lane vector, keep the comparison operator
a compile-time constant, and load via `memcpy` so no alignment is required.

## Visualising the layout

`Data.STree.Dot` renders a tree as a Graphviz graph:

```haskell
import Data.STree.Dot

writeFile "tree.dot" (toDot tree)   -- dot -Tsvg tree.dot -o tree.svg
```

Nodes are drawn as records of their `L` keys with the edges leaving from between them, the way
a B-tree is normally drawn, and **each node is named after its offset into the layout array** —
`n0` is the root, `n34` starts at `layout ! 34`. That is what makes this useful for checking
the permutation rather than merely looking at it.

`toDotStyled` takes a `DotStyle`: `dotKey` for types whose `Show` output is too long to read in
a diagram, and `dotArray` to draw **the flat array as well**, beneath the tree.

```haskell
writeFile "tree.dot" (toDotStyled dotStyle { dotArray = True } tree)
```

Both views are shaded by level. Since level `d` occupies a contiguous block starting at
`B^d - 1`, the array comes out as one band of colour per level matching the nodes above it, so
the correspondence reads off directly and needs no connecting edges — which is just as well,
since one edge per node would bury the picture. Every cell also carries its index, so nothing
depends on colour alone, and a legend gives each level's starting offset and node count.

Two Graphviz details, in case you extend this. Per-cell colour is impossible with record
labels, which take one fill per node, so the array is an HTML-like table instead — a second
label dialect, with different escaping. And `rank=sink` does not place the array below the tree:
with no edge between them they are separate components, which dot packs side by side. One
invisible edge to the leftmost node of the deepest level puts it where it belongs, and keeps a
wide array roughly under the tree rather than hanging off to the right.

For `BTree 2 Int32` over `[1..10]`, whose layout is `[5,8,3,4,6,7,9,10,1,2]`:

```
n0 [label="<p0>|5|<p1>|8|<p2>"];     n0:p0 -> n2;  n0:p1 -> n4;  n0:p2 -> n6;
n2 [label="<p0>|3|<p1>|4|<p2>"];     n2:p0 -> n8;
n4 [label="<p0>|6|<p1>|7|<p2>"];
n6 [label="<p0>|9|<p1>|10|<p2>"];
n8 [label="<p0>|1|<p1>|2|<p2>"];
```

The keys below 3 hang off the first port of `[3,4]`, which is the partial deepest level. Its
sentinel padding is drawn as `~`, and dummy children — the ones the descent skips because their
offset is past the end of the data — are simply absent, which is why some ports have no edge.

It is a debugging aid: anything past a few hundred keys produces a graph Graphviz cannot lay
out usefully.

`cabal run s-tree-example` writes three, which between them cover the cases worth
understanding:

| file | |
| --- | --- |
| `example-tree.dot` | six keys at `L=16` — they fit in one node, so the graph is a single box with ten `~` padding cells |
| `example-tree-deep.dot` | fourteen keys at fanout 3 — three levels, a partial deepest level, and ports without edges |
| `example-tree-4-levels.dot` | eighty keys at fanout 3 — `3^4-1`, so exactly complete: four levels, 40 nodes, every internal node with three children, no padding |
| `example-bplus.dot` | the same keys again as a **map**, three levels, with values, one duplicate and one padding slot |

The two fanout-3 tree graphs set `dotArray`, so they show the tree and the array together.

`toDotBPlus` draws a map instead, and is worth putting beside `example-tree-deep.dot` — the same
keys, both three levels deep. Every key sits in the bottom row in order, its value directly
beneath it and its position below that; the tree above holds nothing but *copies* of seven of
those keys, and those seven are outlined in the bottom row so the derivation is visible.

The key `19` is stored twice with different values, positioned to show two things at once: the
pair straddles the boundary between leaf 3 and leaf 4, so `lookup 19` spans two leaves and
returns both values; and because it starts a leaf it is also a separator, which is the case
where the two bounds have to descend the index differently. The odd key count leaves one padding
slot in the last leaf. Leaves alternate shade to show where they divide. No edges run between the two halves:
the mapping is by leaf number rather than by layout offset, so drawing it honestly would need
the index's in-order ranks, and the shared shading carries it instead.

The example also prints each tree level by level next to the array offsets the levels start at,
which is the quickest way to see how the picture and the flat array correspond.

## Performance

**Single lookups are not vectorised.** GHC exposes no compare-to-mask SIMD primops (and none
at all on aarch64 as of 9.10), so `lowerBoundIdx` uses a scalar loop that folds comparisons
into a `Word64` mask and takes `countTrailingZeros`. Use
`Data.STree.Batch` when you have more than a handful of queries; the numbers below show what
that is worth.

`cabal bench` runs criterion benchmarks over sizes from 1000 to 10^7 keys — 4 KB to 40 MB of
`Int32`, so inside L1 through well past L3. Pass a group name to run a subset, e.g.
`cabal bench --benchmark-options='"lower bound by node width, 1000 queries, n=1000000"'`.

### The baselines

A tree that cannot beat binary search over the same sorted array has no reason to exist, so
what it is measured against matters as much as the tree. Three variants run alongside it, since
a single hand-written baseline invites the question of whether it is a strawman:

| baseline | ns/query at 10^6 |
| --- | --- |
| textbook branchy loop | 102 |
| branchless form | 111 |
| `vector-algorithms` `binarySearchL` | **90** |

Two results worth having measured. The **library implementation is the fastest**, by 9-21%
across sizes — the hand-written baseline was mildly a strawman, so every ratio below is quoted
against `vector-algorithms` instead. And the **branchless form is no faster**, which is the
opposite of what it exists for; GHC appears to compile the branchy comparison to a select
already, leaving no misprediction to remove.

The benchmark checks that all three agree before timing anything. A baseline that quietly
returned the wrong index would make every comparison here meaningless.

### What it shows

Numbers below are from an Apple-silicon arm64 laptop, GHC 9.10.3, `-O2`; treat them as ratios,
not absolutes.

**Node width is the decisive parameter, and the right choice depends on which path you use.**
Nanoseconds per query at 10^6 keys, all from one benchmark session so the rows compare:

| L | 2 | 4 | 8 | 16 | 32 | 64 |
| --- | --- | --- | --- | --- | --- | --- |
| single (`lowerBoundIdx`) | 85 | 95 | 111 | 133 | 180 | 319 |
| batch (`lowerBoundIdxMany`) | 39 | 40 | 15 | **12.7** | 17 | 25 |
| speedup | 2.2x | 2.4x | 7.6x | 10.5x | 10.9x | 12.9x |

The best baseline costs **90 ns** on the same data in the same session.

On the **scalar path** cost grows monotonically with width, because every extra key in a node
is one more comparison with no vector compare to amortise it over. The optimum is at the narrow
end, `L=2` or `L=4`, and **it does not beat a good binary search there**: 85 ns against 90 ns is
inside the variance this pair shows between builds, where `L=2` has measured anywhere from 85 to
100 ns moved by code layout alone. Treat the scalar path as level with binary search, not ahead
of it. At `L=16` and 10^7 keys it loses outright.

**Vectorising inverts that.** Wide nodes become nearly free, so fewer levels wins and the
optimum moves to `L=16`, where the tree finally does what it is supposed to: **12.7 ns per
query, 7.1x faster than the best baseline.** So choose `BTree 16` if your queries go through
`Data.STree.Batch`; if they do not, `BTree 2` or `BTree 4` is the best this structure offers,
and a binary search would serve about as well.

Batched cost by tree size at `L=32`, in ns/query: 4.8 (10^3), 8.6 (10^4), 12.2 (10^5),
16.0 (10^6), 23.6 (10^7).

Other measurements, all at `L=16` and `Int32`:

* `index` costs about 9 ns and is flat in `n` — most positions are leaves, so the walk back up
  the layout usually terminates immediately;
* `unsafeBuild` runs at about 2.6 ns per key (2.6 ms for 10^6, 34 ms for 10^7); the sortedness
  check in `build` adds roughly 0.6 ns per key on top;
* `readSTree` takes about 2.2 ms for 10^6 keys with the page cache warm and 18 ms for 10^7,
  i.e. a bit over 2 GB/s; `writeSTree` is comparable.

For the B+tree, **both** widths want to be narrow on the scalar path, and by more than is
obvious. Position queries at `n = 10^6`, ns per query:

| config | `li=2 ll=8` | `li=4 ll=4` | `li=4 ll=8` | `li=4 ll=16` | `li=8 ll=16` | `li=16 ll=16` | `li=32 ll=16` |
| --- | --- | --- | --- | --- | --- | --- | --- |
| | **88** | 92 | 97 | 98 | 110 | 134 | 177 |

`li = ll = 16` — the obvious default, and what this file recommended until it was measured — is
**34% off the optimum**. Narrow wins on both axes for the same reason it does for the B-tree:
every extra key in a node is another scalar comparison, and neither the index nor the leaf scan
is vectorised. Widening `ll` also has a second cost here, since it shrinks the index and pushes
work into the leaf scan, where there is no tree to prune with.

So `li=2 ll=8` on the scalar path. That will move wide again once the batch path covers this
type, exactly as it did for the B-tree.

### Picking the width at runtime

The table below is this machine. Since the optimum turns out to depend on the hardware, the key
type, the number of keys and which query path you use, `Data.STree.Calibrate` measures it where
the program is actually running:

```haskell
cal <- calibrateBTreeBatch defaultConfig sortedKeys sampleQueries
print (calBest cal, calMeasurements cal)

let idxs = withWidth (calBest cal) $ \(_ :: Proxy l) ->
             lowerBoundIdxMany (unsafeBuild sortedKeys :: BTree l Int32) queries
```

It builds a tree at each of the six widths over your keys, times your queries against them, and
reports every result along with the tree height. `withWidth` reifies the answer back to a type,
so the width need not be known when the program is compiled. On this machine it picks `L=16` for
the batch path and `L=2` for the scalar one, matching the sweep below.

Two details that are deliberate rather than incidental. **Ties break toward the narrower width**
(`cfgTolerance`, 5% by default): the scalar path is flat over a 4x range of widths, so without a
tie band the choice there is settled by noise, and narrower wastes less on node padding. And both
functions are `INLINABLE` so they **specialise to your key type** — unspecialised, the timed loops
box a key per access and dispatch every comparison through a dictionary, which elsewhere in this
library measured 38x slower, and calibrating code 38x slower than the code you will run can
easily prefer the wrong width.

**Check `calResolvable` before trusting `calBest`.** If any pass finished inside the clock's
granularity its figure is quantisation rather than measurement, and the width would be chosen by
noise; the flag says so, and the fix is more queries. This is easy to hit — 200 keys and 50
queries are already too few to time.

`calibrateBPlusScalar` does the same for the map, which has **two** widths to choose. They interact —
the leaf width fixes how many separators exist, and so the size of the index that the index width
is being chosen for — so it measures the **full 6x6 grid**. Coordinate descent is six times
cheaper and was tried first; it converged 12% off the optimum, because the surface is not monotone
in the leaf width. Thirty-six trees costs less than it sounds, since a B+tree build needs no
permutation of its leaves.

Its tie-break runs the *other* way: among equally fast configurations it prefers the **wider**
leaf, because that means fewer separators and so a smaller index (`n/ll` of them), which dominates
the at most `ll-1` keys of leaf padding it costs. On this machine that matters — `li=4 ll=4` and
`li=4 ll=16` measured within 0.2% of each other, and the second has a four times smaller index.

It costs six tree builds — thirty-six for the B+tree — so it is a startup step, not a per-query one. Calibrating on a
subsample does not work: the optimum moves with the number of keys precisely because it is
decided by how much of the tree stays in cache.

### When a wider node stops paying

`cabal bench s-tree-fanout` holds a million keys fixed and varies only the node width, reporting
the tree's height and the derived per-level cost next to the total — the totals say where the
crossover is, the per-level column says why.

| L | 2 | 4 | 8 | 16 | 32 | 64 |
| --- | --- | --- | --- | --- | --- | --- |
| height | 13 | 9 | 7 | 5 | 4 | 4 |
| scalar ns/query | 118 | **109** | 114 | 112 | 136 | 254 |
| scalar ns/level | 9.1 | 12.1 | 16.3 | 22.3 | 34.0 | 63.5 |
| batch ns/query | 41 | 46 | 36 | **11.7** | 15.1 | 31.7 |
| batch ns/level | 3.1 | 5.1 | 5.1 | 2.4 | 3.8 | 7.9 |

**Both paths turn at `L=32`**, and neither should ever go past it: `L=64` costs +134% scalar and
+170% batch. Below that they behave completely differently.

On the **scalar path** the per-level cost fits `6 + 0.9L` ns almost exactly — the fixed cost of a
level plus one comparison per key. Since height falls as `1/log(L+1)`, the two effects very nearly
cancel and the total is *flat within 9% from `L=2` to `L=16`*. There is no meaningful optimum in
that range; the only real decision is not to exceed 16.

On the **vectorised path** a level's compute is a handful of vector instructions regardless of
`L`, so what remains is one dependent memory access per level — and then fewer levels simply wins.
That is why `L=16` at 5 levels beats `L=4` at 9 levels by 4x despite doing four times the
comparisons per node: with 17-way fanout only the last level or two misses cache, where 5-way
fanout misses on four or five. Past `L=16` the vector work per node finally overtakes what the
shallower tree saves.

Memory is identical at every width here, since 10^6 is divisible by 64 and no padding is needed,
so this is purely a time trade-off. At other sizes a wider node can waste up to `L-1` keys.

### The B+tree's two widths

`cabal bench s-tree-fanout-bplus` sweeps both axes at a million keys. It is a presentation layer
over `calibrateBPlusScalar` rather than its own grid — the library already instantiates all
thirty-six combinations, so a second copy would only be something to drift — and it adds the
derived columns plus the B-tree calibrations in the same process, so the cross-structure numbers
finally come from one session.

ns per query (rows: index width, columns: leaf width):

| | ll=2 | ll=4 | ll=8 | ll=16 | ll=32 | ll=64 |
| --- | --- | --- | --- | --- | --- | --- |
| **li=2** | 135 | 126 | 128 | 130 | 140 | 165 |
| **li=4** | 128 | 128 | **122** | 128 | 137 | 158 |
| **li=8** | 126 | 129 | 130 | 121 | 132 | 170 |
| **li=16** | 176 | 155 | 149 | 133 | 158 | 185 |
| **li=32** | 203 | 195 | 195 | 179 | 190 | 202 |
| **li=64** | 293 | 265 | 259 | 269 | 273 | 298 |

**Read this as a region, not a point.** Six of the thirty-six configurations are within 5% of the
best and eight within 10%, and which one wins moves between runs — the reported optimum has been
`li=2 ll=8`, `li=4 ll=8`, `li=4 ll=16` and `li=8 ll=16` on different runs of the same machine. The
actionable answer is a **narrow index (`li` at most 8) and a moderate leaf (`ll` 8 to 16)**; past
`li=16` it degrades steeply, which is the only sharp edge in the table.

The per-level column shows `li` is what drives cost — 10 ns per level at `li=2` rising to 74 at
`li=64` — while `ll` decides how much has to be stored: the index holds `n/ll` separators, ranging
from 500,000 keys at `ll=2` to 15,624 at `ll=64`. That 32-fold spread over configurations that time
within a few per cent of each other is why `calibrateBPlusScalar` breaks ties toward the wider leaf.

Same session, for scale: B-tree scalar at its best width 128 ns, B+tree scalar at its best 122 ns —
**level with each other**, as the structural analysis predicts, since both do one scalar rank per
level. The B-tree's batch path is 11.8 ns, an order of magnitude below either, and it remains the
only vectorised path there is.

### Where a sequential scan wins

`cabal bench --benchmark-options='"scan vs tree, 1000 queries, Int32"'` pits the tree against a
straight scan from the start of the sorted array — `O(n)` but with no dependent loads, perfect
prefetch and no log factor. Queries come from the same distribution as the keys, so a query's
rank among them is uniform and the scan does `n/2` comparisons on average; measuring it with
out-of-range queries would either exit immediately or walk everything.

Nanoseconds per query:

| n | 4 | 16 | 64 | 128 | 256 | 1024 | 16384 |
| --- | --- | --- | --- | --- | --- | --- | --- |
| sequential scan | 7.3 | 11.3 | 30 | 51 | 93 | 333 | 5288 |
| binary search | 3.2 | 5.5 | 8.1 | 9.9 | 13 | 27 | 45 |
| scalar tree, `L=4` | 7.7 | 11.7 | 17 | 21 | 24 | 32 | 54 |
| scalar tree, `L=16` | 23.5 | 24.0 | 42 | 46 | 46 | 67 | 98 |
| **batch tree, `L=16`** | 2.3 | **2.3** | **3.2** | **3.4** | **3.4** | **4.6** | **6.8** |
| batch tree, `L=4` | **1.6** | 3.0 | 4.6 | 7.0 | 10 | 10 | 18 |

**There is no crossover for the vectorised path** — it beats a scan at every size measured,
including `n=4`, where one vector comparison already covers the whole array. From 8 keys upward
`L=16` is the one to use and it stays flat: 2.3 ns at 16 keys, 6.8 ns at 16384.

The **scalar** path is the one with a crossover, and where it falls depends entirely on the node
width: `L=4` overtakes a scan at **n≈16–32**, `L=16` not until **n≈128**. The reason is padding —
a rank always reads a whole node, so at `n=4` the `L=16` tree compares against 12 sentinels it
does not need and costs 3x what `L=4` does. If you are stuck on the scalar path and your trees
are small, the width matters more than anything else.

One caveat on reading this table as vindication: **binary search beats the scalar tree at every
size here**, from 3.2 against 7.7 at `n=4` to 45 against 54 at 16384. The scalar tree only ever
wins against the scan, never against a good binary search. It is the vectorised path that earns
the structure its place.

### Threads

`cabal bench s-tree-threads` sweeps thread counts against slice sizes. Lookups are read-only
against an immutable tree, so there is nothing to synchronise; what the harness is actually for
is finding what stops it scaling. Peak on this 8+2-core machine, 4·10^6 keys:

| | 1 thread | 8 threads | 10 threads |
| --- | --- | --- | --- |
| vectorised batch | 25 Mq/s | 180 (7.1x) | 189 (7.5x) |
| scalar | 3.7 Mq/s | 32 (8.8x) | 35 (9.5x) |

**The slower path scales better**, which is the opposite of the intuitive result and the useful
one: the batch path is already 7x faster per query, so it runs into the machine's random-access
limit at a much lower thread count. Chunk size makes no difference to throughput at any size
from 256 to 250,000, which rules out the foreign call, the per-batch allocation and the yield as
the limit — it is memory.

**Static equal slices are the wrong default here.** Two of the ten cores are efficiency cores at
roughly a third the speed, so equal division leaves the last worker finishing 8–16 ms after the
first:

| 10 threads, batch | static (200k) | 65536 | 8192 | 1024 | 256 |
| --- | --- | --- | --- | --- | --- |
| speedup | 4.95x | 6.92x | 7.42x | 7.29x | **7.49x** |
| spread | 7.7 ms | 2.5 ms | 0.4 ms | 0.1 ms | 0.2 ms |

Static costs 34% against a shared queue. The rule that falls out: size the chunk so there are
**at least ten times as many chunks as workers**, keeping the tail under a tenth of a worker's
share, and keep each chunk to at least a few microseconds so the atomic and the foreign call
disappear into it. For this workload anything from 1024 to 8192 queries satisfies both, and the
choice within that range does not matter.

### Where the query time goes, and whether batching would help

Running the width sweep over an L1-resident tree as well as a large one separates the descent's
work from the memory it waits on. Per level, at `L=4`:

| tree | 4 KB (L1) | 4 MB | 40 MB | 400 MB |
| --- | --- | --- | --- | --- |
| ns per level | 6.8 | 13.2 | 13.1 | 15.0 |

Compute is about 6.8 ns per level and does not change; everything above that is memory. So
**roughly half of a query is spent waiting**, and — note the flat middle columns — the penalty
saturates as soon as the tree leaves L1. What is being waited on is mostly L2/SLC latency, not
100 ns DRAM stalls; TLB pressure plausibly contributes, since there are no huge pages here.

None of that waiting is currently overlapped. Serialising the descents — same key sequence,
but each query's key address made data-dependent on the previous query's result — costs
nothing at any size:

| | 4 MB | 40 MB | 400 MB |
| --- | --- | --- | --- |
| independent (`L=4`) | 119 | 144 | 180 |
| dependent chain (`L=4`) | 118 | 141 | 179 |

If the out-of-order engine were hiding memory latency across consecutive queries, forcing them
into a chain would have exposed it. It does not, so it is not. That headroom is unexploited,
and batching — interleaving several descents so their loads overlap — is the only way to reach
it, since within a single descent the next node's address depends on the current node's rank
and so cannot be prefetched.

The ceiling is therefore about 1.8×, if every stall were hidden perfectly.

**The vectorised batch has since collected most of that anyway**, without any explicit
interleaving. At `L=16` and 10^6 keys it answers a query in 12 ns across 5 levels — 2.5 ns per
level, *below* the ~5.5 ns per level of memory cost measured above. The C loop's per-query body
is small enough that many queries fit in the reorder buffer at once, so their misses overlap on
their own, which the much bulkier Haskell loop never achieved. Explicitly interleaving K
cursors is therefore a lot less attractive than the analysis above suggested; measure before
building it.

### Why the pragmas matter

The first version of this benchmark reported `build` at 46 ms per 10^6 keys and `readSTree` at
26 ms, because the library's overloaded functions had no `INLINE`/`INLINABLE` pragmas. GHC then
compiles each one once with dictionary arguments, and a caller in another module cannot
specialise it: `-fspecialise` only specialises *imported* functions marked `INLINABLE`. The
inner loops were therefore boxing a key per array access and dispatching every comparison
through a dictionary. Adding the pragmas gave 14× on `build`, 7× on `unsafeBuild`, 12× on
`readSTree` and 3× on `index`, with the query path unaffected because it was already `INLINE`.

Two things are worth knowing if you touch this:

* Building the *library* with `-fexpose-all-unfoldings -fspecialise-aggressively` does **not**
  substitute for the pragmas — specialisation happens at the call site, so the flags would have
  to go on every consuming module.
* A pragma on the caller is not enough on its own. `isSortedAsc` stayed overloaded inside an
  already-specialised `build`, because the specialiser does not revisit calls in the
  specialisations it creates during that pass; it needed `INLINE` of its own, which cut it from
  27 ms to 0.6 ms.

## Design notes

* **Height** is the smallest `h` with `B^h > n`, computed with integer arithmetic. Deriving it
  from a floating-point logarithm risks rounding to one below the correct value when `n` is an
  exact power of `B`, which would make the virtual size smaller than `n` and corrupt the layout.
* **Powers of `B` are computed on demand**, so nothing bounds the height.
* **Floating-point padding is `+Infinity`**, not the largest finite value: it satisfies the
  sentinel contract for strictly more inputs, at no cost.
* **`n = 0` is supported**: queries return `0`.
* **No huge pages, and no over-aligned allocation.** Nothing needs aligned loads — the
  vectorised kernel loads via `memcpy` — and `MADV_HUGEPAGE` is not portable.
* **A `NaN` query returns 0, not an out-of-range position.** The rank predicates are written
  `not (k < x)` and `not (k <= x)` rather than `k >= x` and `k > x`. On any total order those
  are the same test at the same cost, but a `NaN` is not ordered: every `<` against one is
  False, so the negated form stops the rank at a node's first key instead of running past its
  last into the sentinel padding. Without it the position could land beyond the data — which in
  the B+tree meant `lookup` asking for a slice of negative length and throwing. Clamping the
  result with a `min` also fixes it and measured 7% slower on the B-tree's query path and 14%
  on the B+tree's; the predicate costs nothing. Which position a `NaN` gets is arbitrary; that
  it is in `[0, size]` is not.
* **The descent is not specialised on a statically known height.** Fixing the height at compile
  time would let the level constants be folded, but GHC gains nothing measurable from it.

## Tests

`cabal test` covers, for every supported width × every key type:

* sizes `0, 1, 2, 3, 7, 8, 20, 63, 64, 65, 1000, 100000` **and** `B^k - 1`, `B^k`, `B^k + 1`
  for each `k`, where the height computation and the partial deepest level are most delicate;
* `t ! i == v ! i` for every position — the layout is a permutation that `index` inverts,
  which by itself pins down the whole build;
* every key present in the data is found by both bounds;
* both bounds agree exactly with binary search over the original array, on absent and present
  keys — that being the specification those operations have;
* heavily duplicated data;
* golden layouts for `L=2`, small enough to work out by hand from the layout rules, so the
  permutation itself is pinned and not merely its observable behaviour;
* QuickCheck properties: `blockRank` against an independent formulation, both bounds against
  the oracle on arbitrary data, and results always usable as slice indices;
* two trees of 10^6 keys;
* the vectorised batch path against the pure descent, for all 36 (key type, node width)
  kernels, including sliced inputs, batches shorter than one chunk, and empty batches;
* the Graphviz output: one node per node of the layout, exactly a tree's worth of edges, every
  key drawn exactly once, and record cells accounting for the padding as well;
* the map renderer: every key, value and position drawn exactly once, one leaf header per leaf
  and one highlight per separator;
* the B+tree, over both width axes and every key type: the round trip through `keyAt`/`valueAt`,
  both bounds against the oracle, `lookup` against a naive filter, ranges against a filter, and
  the separator invariant checked against its definition — plus duplicate-heavy and all-keys-equal
  inputs, which make the separator-equality case (below) far more likely to be hit;
* serialization: round trips for every key type and every node width, one spanning several
  staging-buffer passes, plus every rejection path — bad magic, wrong version, wrong key type,
  wrong node width, opposite endianness, absurd key count, truncated file, trailing bytes, and
  a payload that ends early under a caller-supplied budget;
* `unsafeFromLayout (size t) (layout t) == t` at every size and width, which is what pins the
  loader's closed form for the exceeding-node count against what the build loop computes.
