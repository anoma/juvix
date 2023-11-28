# Hyperfine Benchmarks

## dev parse

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `juvix-v0.4.3 dev parse fibo.juvix ` | 184.5 ± 5.6 | 179.3 | 193.6 | 1.06 ± 0.05 |
| `juvix-v0.5.0 dev parse fibo.juvix ` | 179.9 ± 6.4 | 169.7 | 192.2 | 1.04 ± 0.05 |
| `juvix-v0.5.1 dev parse fibo.juvix ` | 173.4 ± 6.2 | 167.1 | 182.2 | 1.00 |
| `juvix-v0.5.2 dev parse fibo.juvix ` | 187.8 ± 7.7 | 179.6 | 201.9 | 1.08 ± 0.06 |
| `juvix-v0.5.3 dev parse fibo.juvix ` | 1162.3 ± 25.2 | 1130.6 | 1204.1 | 6.70 ± 0.28 |
| `juvix-v0.5.4 dev parse fibo.juvix ` | 1257.5 ± 4.1 | 1253.5 | 1267.8 | 7.25 ± 0.26 |
| `juvix dev parse fibo.juvix ` | 1281.0 ± 26.5 | 1251.7 | 1330.6 | 7.39 ± 0.30 |

## dev highlight

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `juvix-v0.4.3 dev highlight fibo.juvix ` | 714.6 ± 13.1 | 702.9 | 740.5 | 1.00 |
| `juvix-v0.5.0 dev highlight fibo.juvix ` | 722.0 ± 7.9 | 713.8 | 734.4 | 1.01 ± 0.02 |
| `juvix-v0.5.1 dev highlight fibo.juvix ` | 804.1 ± 13.6 | 791.5 | 833.3 | 1.13 ± 0.03 |
| `juvix-v0.5.2 dev highlight fibo.juvix ` | 828.7 ± 13.5 | 816.2 | 862.3 | 1.16 ± 0.03 |
| `juvix-v0.5.3 dev highlight fibo.juvix ` | 1887.9 ± 29.8 | 1857.5 | 1946.1 | 2.64 ± 0.06 |
| `juvix-v0.5.4 dev highlight fibo.juvix ` | 2076.7 ± 20.4 | 2045.2 | 2123.9 | 2.91 ± 0.06 |
| `juvix dev highlight fibo.juvix ` | 2075.3 ± 18.8 | 2058.8 | 2122.5 | 2.90 ± 0.06 |

## typecheck

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `juvix-v0.4.3 typecheck fibo.juvix ` | 653.9 ± 3.6 | 645.0 | 656.4 | 1.00 |
| `juvix-v0.5.0 typecheck fibo.juvix ` | 667.8 ± 17.0 | 652.3 | 708.4 | 1.02 ± 0.03 |
| `juvix-v0.5.1 typecheck fibo.juvix ` | 768.4 ± 17.4 | 741.6 | 795.3 | 1.18 ± 0.03 |
| `juvix-v0.5.2 typecheck fibo.juvix ` | 775.1 ± 12.5 | 764.6 | 800.5 | 1.19 ± 0.02 |
| `juvix-v0.5.3 typecheck fibo.juvix ` | 1887.2 ± 61.8 | 1822.8 | 2015.2 | 2.89 ± 0.10 |
| `juvix-v0.5.4 typecheck fibo.juvix ` | 2084.6 ± 54.5 | 2036.1 | 2198.7 | 3.19 ± 0.09 |
| `juvix typecheck fibo.juvix ` | 2222.5 ± 35.0 | 2145.7 | 2266.5 | 3.40 ± 0.06 |

## compile -o /dev/null

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `juvix-v0.4.3 compile -o /dev/null fibo.juvix ` | 792.6 ± 16.4 | 759.9 | 813.3 | 1.00 |
| `juvix-v0.5.0 compile -o /dev/null fibo.juvix ` | 799.1 ± 12.2 | 777.5 | 816.4 | 1.01 ± 0.03 |
| `juvix-v0.5.1 compile -o /dev/null fibo.juvix ` | 894.5 ± 16.7 | 875.8 | 916.9 | 1.13 ± 0.03 |
| `juvix-v0.5.2 compile -o /dev/null fibo.juvix ` | 924.4 ± 16.4 | 896.3 | 947.8 | 1.17 ± 0.03 |
| `juvix-v0.5.3 compile -o /dev/null fibo.juvix ` | 2128.0 ± 17.8 | 2099.9 | 2160.9 | 2.68 ± 0.06 |
| `juvix-v0.5.4 compile -o /dev/null fibo.juvix ` | 2322.7 ± 28.0 | 2279.4 | 2366.9 | 2.93 ± 0.07 |
| `juvix compile -o /dev/null fibo.juvix ` | 2372.6 ± 10.7 | 2354.5 | 2387.9 | 2.99 ± 0.06 |

## compile -o /dev/null -t wasm32-wasi

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `juvix-v0.4.3 compile -o /dev/null -t wasm32-wasi fibo.juvix ` | 799.4 ± 15.0 | 774.1 | 824.9 | 1.01 ± 0.03 |
| `juvix-v0.5.0 compile -o /dev/null -t wasm32-wasi fibo.juvix ` | 793.2 ± 16.2 | 769.7 | 813.2 | 1.00 |
| `juvix-v0.5.1 compile -o /dev/null -t wasm32-wasi fibo.juvix ` | 909.9 ± 12.6 | 888.3 | 926.7 | 1.15 ± 0.03 |
| `juvix-v0.5.2 compile -o /dev/null -t wasm32-wasi fibo.juvix ` | 924.5 ± 13.8 | 898.8 | 941.4 | 1.17 ± 0.03 |
| `juvix-v0.5.3 compile -o /dev/null -t wasm32-wasi fibo.juvix ` | 2147.4 ± 12.2 | 2129.0 | 2162.5 | 2.71 ± 0.06 |
| `juvix-v0.5.4 compile -o /dev/null -t wasm32-wasi fibo.juvix ` | 2387.0 ± 27.1 | 2324.4 | 2416.4 | 3.01 ± 0.07 |
| `juvix compile -o /dev/null -t wasm32-wasi fibo.juvix ` | 2377.5 ± 21.5 | 2340.4 | 2403.6 | 3.00 ± 0.07 |

## compile -o /dev/null -t core

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `juvix-v0.4.3 compile -o /dev/null -t core fibo.juvix ` | 740.0 ± 12.1 | 719.5 | 760.2 | 1.01 ± 0.03 |
| `juvix-v0.5.0 compile -o /dev/null -t core fibo.juvix ` | 736.0 ± 15.2 | 712.2 | 756.5 | 1.00 |
| `juvix-v0.5.1 compile -o /dev/null -t core fibo.juvix ` | 844.6 ± 17.5 | 819.3 | 864.1 | 1.15 ± 0.03 |
| `juvix-v0.5.2 compile -o /dev/null -t core fibo.juvix ` | 864.9 ± 16.1 | 844.0 | 887.7 | 1.18 ± 0.03 |
| `juvix-v0.5.3 compile -o /dev/null -t core fibo.juvix ` | 2086.2 ± 12.6 | 2073.3 | 2108.4 | 2.83 ± 0.06 |
| `juvix-v0.5.4 compile -o /dev/null -t core fibo.juvix ` | 2319.2 ± 18.5 | 2294.4 | 2341.6 | 3.15 ± 0.07 |
| `juvix compile -o /dev/null -t core fibo.juvix ` | 2292.8 ± 47.5 | 2216.5 | 2349.8 | 3.12 ± 0.09 |

## compile -o /dev/null -t asm

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `juvix-v0.4.3 compile -o /dev/null -t asm fibo.juvix ` | 704.4 ± 5.1 | 695.3 | 711.3 | 1.00 |
| `juvix-v0.5.0 compile -o /dev/null -t asm fibo.juvix ` | 709.0 ± 10.7 | 694.1 | 729.4 | 1.01 ± 0.02 |
| `juvix-v0.5.1 compile -o /dev/null -t asm fibo.juvix ` | 878.1 ± 15.4 | 849.4 | 900.6 | 1.25 ± 0.02 |
| `juvix-v0.5.2 compile -o /dev/null -t asm fibo.juvix ` | 890.7 ± 29.4 | 839.8 | 917.5 | 1.26 ± 0.04 |
| `juvix-v0.5.3 compile -o /dev/null -t asm fibo.juvix ` | 1995.1 ± 25.1 | 1959.0 | 2045.2 | 2.83 ± 0.04 |
| `juvix-v0.5.4 compile -o /dev/null -t asm fibo.juvix ` | 2220.9 ± 21.9 | 2188.7 | 2254.9 | 3.15 ± 0.04 |
| `juvix compile -o /dev/null -t asm fibo.juvix ` | 2213.5 ± 11.3 | 2201.5 | 2237.4 | 3.14 ± 0.03 |

## eval

| Command | Mean [ms] | Min [ms] | Max [ms] | Relative |
|:---|---:|---:|---:|---:|
| `juvix-v0.4.3 eval fibo.juvix ` | 709.8 ± 7.3 | 700.0 | 723.5 | 1.00 |
| `juvix-v0.5.0 eval fibo.juvix ` | 710.7 ± 8.2 | 698.5 | 722.3 | 1.00 ± 0.02 |
| `juvix-v0.5.1 eval fibo.juvix ` | 807.5 ± 7.4 | 791.2 | 816.8 | 1.14 ± 0.02 |
| `juvix-v0.5.2 eval fibo.juvix ` | 824.9 ± 10.9 | 810.2 | 848.1 | 1.16 ± 0.02 |
| `juvix-v0.5.3 eval fibo.juvix ` | 2036.2 ± 66.2 | 1986.0 | 2194.8 | 2.87 ± 0.10 |
| `juvix-v0.5.4 eval fibo.juvix ` | 2397.2 ± 14.8 | 2379.6 | 2422.4 | 3.38 ± 0.04 |
| `juvix eval fibo.juvix ` | 2294.0 ± 110.5 | 2185.0 | 2455.3 | 3.23 ± 0.16 |
# new-typechecker runs

## typecheck

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `juvix-v0.5.4 typecheck fibo.juvix ` | 2.201 ± 0.023 | 2.165 | 2.243 | 1.01 ± 0.02 |
| `juvix typecheck fibo.juvix ` | 2.186 ± 0.026 | 2.162 | 2.246 | 1.00 |

## typecheck

| Command | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `juvix-v0.5.4 typecheck fibo.juvix --new-typechecker` | 2.222 ± 0.018 | 2.202 | 2.269 | 1.00 |
| `juvix typecheck fibo.juvix --new-typechecker` | 2.236 ± 0.018 | 2.211 | 2.272 | 1.01 ± 0.01 |
