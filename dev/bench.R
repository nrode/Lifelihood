library(microbenchmark)
library(future)
library(future.apply)

# Sequential
plan(sequential)
bench_seq <- microbenchmark(
  seq = simulation(results, event = "all", seed = 123),
  times = 100L
)

# Parallel (multisession: safe across OS)
plan(multisession, workers = 3)
bench_par <- microbenchmark(
  par = simulation(results, event = "all", seed = 123),
  times = 100L
)

print(bench_seq)
cat("\n")
print(bench_par)
