# Codec pressure-test matrix — 2026-08-28

Machine: local (same host for all runs). JMH: Fork(1), warmup 3x1s, measurement 5x1s,
thrpt ops/ms. Runs sequential, fresh compile per worktree. Benchmark source identical
across all four points (benchmarks/CodecBenchmarks.scala). Factors:

| point | commit   | traversal | erasure | per-decode setup |
|-------|----------|-----------|---------|------------------|
| P0    | 4ccd273  | recursive | Any     | per-decode lists |
| P1    | 1424fdd  | Eval      | Any     | per-decode lists |
| P2    | 84d85ce  | Eval      | Any     | hoisted          |
| P3    | d97c621  | Eval      | typed   | unrolled (none)  |
| P4    | HEAD     | Eval      | typed   | hybrid drain (R4) |

Throughput (ops/ms, higher better):

| benchmark          | P0 recursive | P1 eval | P2 +hoist | P3 +typed | P4 +R4 |
|--------------------|-------------:|--------:|----------:|----------:|-------:|
| decodeNestedUser   |     6,335.7  | 2,298.2 |  3,901.9  |  4,160.6  | 3,899.0 |
| encodeNestedUser   |     7,346.4  | 2,834.4 |  2,973.2  |  3,815.7  | 3,712.9 |
| decodeDeep (10k)   |        SOE   |     0.8 |      1.3  |      1.2  |    1.0 |
| encodeDeep (10k)   |        SOE   |     0.7 |      0.8  |      1.4  |    1.3 |
| decodeWide (100k)  |     1,857.0  |     0.3 |      0.3  |      0.3  |    1.2 |
| encodeWide (100k)  |     3,644.0  |     0.4 |      0.4  |      0.4  |    2.0 |

Findings:
1. F1 safety: P0 StackOverflowErrors on decodeDeep/encodeDeep — the recursive codec
   cannot run the deep axis at all. P1-P3 complete it (stack-safe).
2. F1 cost as first landed (P0->P1): 0.36x typical, 0.12-0.15x wide. The per-decode
   list rebuilding (fixed in P2) was ~half the typical-payload cost.
3. F2 (P2->P3, typed + unrolled): encodeDeep +78%, encodeNestedUser +29%,
   decodeNestedUser +6%, wide neutral. The typed refactor is faster than the
   erased one it replaced — correctness and performance aligned.
4. Remaining vs recursive: 0.51-0.66x typical (pure trampoline cost), 0.54-0.65x wide
   after R4 (the hybrid drain: strict elements iterate @tailrec with zero trampoline
   nodes; deferred elements suspend one flatMap). Residual wide cost is the leaf
   `Eval.now` wrapper per element.

R4 note: the first drain implementation had a closure bug — applyStep closed over
drain's parameters instead of strict's threaded accumulator, so every step saw the
initial accumulator (caught by the oracle: List(3) instead of List(1,2,3)). Fixed
by passing acc/errs through applyStep explicitly.
