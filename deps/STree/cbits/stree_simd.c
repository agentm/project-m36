/* Vectorised node rank and descent for Data.STree.
 *
 * This is the SIMD counterpart of Data.STree.Internal.Rank and of searchBy in
 * Data.STree.BTree, and it must return exactly the same indices.
 *
 * No intrinsics and no per-architecture code: GCC/clang vector extensions lower
 * to NEON, SSE/AVX, or plain scalar code depending on the target. On aarch64
 * this produces the same instruction sequence as hand-written NEON intrinsics.
 *
 * The usual formulation -- compare to a mask, then count trailing zeros --
 * needs x86's movemask, which is neither portable nor necessary here: because a
 * node's keys are sorted, the rank is just the number of lanes that compare
 * true, so summing the comparison lanes works on every target. Comparison
 * yields -1 per true lane, hence the negation.
 *
 * Three things matter for the generated code, and all three were checked
 * against the disassembly:
 *
 *   - accumulate into one 16-byte vector (NL lanes) rather than declaring a
 *     single L-lane vector; the latter makes clang reduce via a narrow-and-
 *     popcount sequence that is ~70% longer.
 *   - the comparison operator must be a compile-time constant, or both bound
 *     variants end up in the body behind a branch.
 *   - memcpy for the load, so no alignment is required of the caller.
 */

#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* T   element type
 * IT  integer type that a comparison of T yields per lane (same width as T)
 * NL  lanes per vector: min(16 / sizeof(T), L)
 * L   keys per node
 * OP  '<' for a lower bound (keys strictly below the query do not stop the
 *     descent), '<=' for an upper bound
 */
#define STREE_RANK(T, IT, NL, L, SUF, OP)                                      \
  typedef T stree_v_##SUF __attribute__((vector_size(sizeof(T) * (NL))));      \
  typedef IT stree_c_##SUF __attribute__((vector_size(sizeof(IT) * (NL))));    \
                                                                               \
  static inline size_t stree_rank_##SUF(const T *y, T x) {                     \
    stree_v_##SUF xv;                                                          \
    for (int i = 0; i < (NL); i++) xv[i] = x;                                  \
    stree_c_##SUF acc = {0};                                                   \
    for (int j = 0; j < (L) / (NL); j++) {                                     \
      stree_v_##SUF v;                                                         \
      memcpy(&v, y + j * (NL), sizeof v);                                      \
      acc += (v OP xv);                                                        \
    }                                                                          \
    IT s = 0;                                                                  \
    for (int i = 0; i < (NL); i++) s += acc[i];                                \
    return (size_t)(-s);                                                       \
  }                                                                            \
                                                                               \
  /* Mirror of searchBy, including the two-branch conversion of the node       \
     cursor to a data index. */                                                \
  static inline size_t stree_search_##SUF(const T *tree, size_t n, size_t h,   \
                                          size_t vsize, size_t e, T x) {       \
    size_t b = 1, off, r;                                                      \
    for (size_t lev = 0; lev + 1 < h; lev++) {                                 \
      off = (b - 1) * (L);                                                     \
      r = stree_rank_##SUF(tree + off, x);                                     \
      b = off + b + r + 1;                                                     \
    }                                                                          \
    off = (b - 1) * (L);                                                       \
    if (off < n) {                                                             \
      r = stree_rank_##SUF(tree + off, x);                                     \
      b = off + b + r + 1;                                                     \
      return b - vsize / (L) - 1;                                              \
    }                                                                          \
    return (b - (n - e) / (L) - 1) + e;                                        \
  }                                                                            \
                                                                               \
  static void stree_batch_##SUF(const T *tree, size_t n, size_t h,             \
                                size_t vsize, size_t e, const T *keys,         \
                                size_t count, int64_t *out) {                  \
    for (size_t i = 0; i < count; i++)                                         \
      out[i] = (int64_t)stree_search_##SUF(tree, n, h, vsize, e, keys[i]);     \
  }

/* Both bounds at one width. */
#define STREE_WIDTH(T, IT, NL, L, TAG)                                         \
  STREE_RANK(T, IT, NL, L, TAG##_lb_##L, <)                                    \
  STREE_RANK(T, IT, NL, L, TAG##_ub_##L, <=)

/* All supported widths for one element type. NL is min(16/sizeof(T), L): a
 * 16-byte accumulator, except where the node is narrower than that. */
#define STREE_TYPE32(T, IT, TAG)                                               \
  STREE_WIDTH(T, IT, 2, 2, TAG)                                                \
  STREE_WIDTH(T, IT, 4, 4, TAG)                                                \
  STREE_WIDTH(T, IT, 4, 8, TAG)                                                \
  STREE_WIDTH(T, IT, 4, 16, TAG)                                               \
  STREE_WIDTH(T, IT, 4, 32, TAG)                                               \
  STREE_WIDTH(T, IT, 4, 64, TAG)

#define STREE_TYPE64(T, IT, TAG)                                               \
  STREE_WIDTH(T, IT, 2, 2, TAG)                                                \
  STREE_WIDTH(T, IT, 2, 4, TAG)                                                \
  STREE_WIDTH(T, IT, 2, 8, TAG)                                                \
  STREE_WIDTH(T, IT, 2, 16, TAG)                                               \
  STREE_WIDTH(T, IT, 2, 32, TAG)                                               \
  STREE_WIDTH(T, IT, 2, 64, TAG)

/* The one exported entry point per element type. The node width has to be a
 * compile-time constant for the vector types, so it is switched on here -- once
 * per batch, outside the query loop, where it costs nothing. Splitting on the
 * bound here too keeps the comparison constant inside the loop.
 *
 * The three offsets are in elements: an unboxed vector reaches C as the address
 * of its whole backing array, which may be a slice, and chunking a batch shifts
 * the key and output windows. */
#define STREE_ENTRY(T, TAG)                                                    \
  void stree_batch_##TAG(const T *tree, size_t tree_off, size_t n, size_t h,   \
                         size_t vsize, size_t e, size_t l, const T *keys,      \
                         size_t keys_off, size_t count, int64_t *out_base,     \
                         size_t out_off, int lower) {                          \
    const T *t = tree + tree_off;                                              \
    const T *k = keys + keys_off;                                              \
    int64_t *out = out_base + out_off;                                         \
    if (lower) {                                                               \
      switch (l) {                                                             \
      case 2:  stree_batch_##TAG##_lb_2 (t, n, h, vsize, e, k, count, out); break; \
      case 4:  stree_batch_##TAG##_lb_4 (t, n, h, vsize, e, k, count, out); break; \
      case 8:  stree_batch_##TAG##_lb_8 (t, n, h, vsize, e, k, count, out); break; \
      case 16: stree_batch_##TAG##_lb_16(t, n, h, vsize, e, k, count, out); break; \
      case 32: stree_batch_##TAG##_lb_32(t, n, h, vsize, e, k, count, out); break; \
      case 64: stree_batch_##TAG##_lb_64(t, n, h, vsize, e, k, count, out); break; \
      }                                                                        \
    } else {                                                                   \
      switch (l) {                                                             \
      case 2:  stree_batch_##TAG##_ub_2 (t, n, h, vsize, e, k, count, out); break; \
      case 4:  stree_batch_##TAG##_ub_4 (t, n, h, vsize, e, k, count, out); break; \
      case 8:  stree_batch_##TAG##_ub_8 (t, n, h, vsize, e, k, count, out); break; \
      case 16: stree_batch_##TAG##_ub_16(t, n, h, vsize, e, k, count, out); break; \
      case 32: stree_batch_##TAG##_ub_32(t, n, h, vsize, e, k, count, out); break; \
      case 64: stree_batch_##TAG##_ub_64(t, n, h, vsize, e, k, count, out); break; \
      }                                                                        \
    }                                                                          \
  }

STREE_TYPE32(int32_t, int32_t, i32)
STREE_TYPE64(int64_t, int64_t, i64)
STREE_TYPE32(uint32_t, int32_t, u32)
STREE_TYPE64(uint64_t, int64_t, u64)
STREE_TYPE32(float, int32_t, f32)
STREE_TYPE64(double, int64_t, f64)

STREE_ENTRY(int32_t, i32)
STREE_ENTRY(int64_t, i64)
STREE_ENTRY(uint32_t, u32)
STREE_ENTRY(uint64_t, u64)
STREE_ENTRY(float, f32)
STREE_ENTRY(double, f64)
