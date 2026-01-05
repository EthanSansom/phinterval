# setup ------------------------------------------------------------------------

load_all()
library(lubridate)
library(ivs)
library(tibble)

set.seed(123)

# data -------------------------------------------------------------------------

n_int <- 25000
dates <- seq(as.Date("1960-01-01"), length.out = n_int)

s1 <- dates
e1 <- dates + ddays(sample(10:20, n_int, TRUE))
s2 <- dates + ddays(sample(0:9, n_int, TRUE))
e2 <- dates + ddays(sample(25:50, n_int, TRUE))

i1 <- interval(s1, e1)
i2 <- interval(s2, e2)

n_phint <- 10000
g <- sample(seq(n_phint), n, TRUE)
p1 <- phinterval(split(i1, g))
p2 <- phinterval(split(i2, g))

# resize -----------------------------------------------------------------------

i1_1000 <- i1[1:1000]
i2_1000 <- i2[1:1000]
i1_25K <- i1
i2_25K <- i2
i1_1M <- rep(i1, 1000*1000/25000)
i2_1M <- rep(i2, 1000*1000/25000)

p1_1000 <- p1[1:1000]
p2_1000 <- p2[1:1000]
p1_10K <- p1
p2_10K <- p2

groups_5K <- tibble(
  start = int_start(i1_25K),
  end = int_end(i1_25K),
  group = sample(seq(5000), length(i1_25K), TRUE)
)
groups_50K <- tibble(
  start = int_start(i1_1M)[1:250000],
  end = int_end(i1_1M)[1:250000],
  group = sample(seq(50000), 250000, TRUE)
)

# save -------------------------------------------------------------------------

if (!dir.exists("benchmarks/data")) dir.create("benchmarks/data")

saveRDS(i1_1000, "benchmarks/data/i1_1000.rds")
saveRDS(i2_1000, "benchmarks/data/i2_1000.rds")
saveRDS(i1_25K,  "benchmarks/data/i1_25K.rds")
saveRDS(i2_25K,  "benchmarks/data/i2_25K.rds")
saveRDS(i1_1M,   "benchmarks/data/i1_1M.rds")
saveRDS(i2_1M,   "benchmarks/data/i2_1M.rds")

saveRDS(p1_1000, "benchmarks/data/p1_1000.rds")
saveRDS(p2_1000, "benchmarks/data/p2_1000.rds")
saveRDS(p1_10K,  "benchmarks/data/p1_10K.rds")
saveRDS(p2_10K,  "benchmarks/data/p2_10K.rds")

saveRDS(groups_5K,  "benchmarks/data/groups_5K.rds")
saveRDS(groups_50K, "benchmarks/data/groups_50K.rds")
