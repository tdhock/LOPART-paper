source("packages.R")

set.seed(1)
N <- 1000000
break.vec <- seq(10, N, by=10)
mu.change <- rep(0, N)
mu.change[break.vec] <- c(2, -2)
mu.vec <- cumsum(mu.change)-1
signal <- rnorm(N, mu.vec)
label.size <- 9
(all.labels <- data.table(
  start=break.vec-label.size,
  end=break.vec,
  changes=1))
plot(signal[1:100])

some.N <- 10000
some.labels <- all.labels[end <= some.N]
some.signal <- signal[1:some.N]
size.vec <- unique(ceiling(10^seq(0, log10(nrow(some.labels)), l=10)))
timing_list <- list()
for(size.i in seq_along(size.vec)){
  size <- size.vec[[size.i]]
  cat(sprintf("%4d / %4d labels=%d\n", size.i, length(size.vec), size))
  labels <- some.labels[1:size]
  seg.dt <- binsegRcpp::binseg_normal(some.signal)
  Lfit <- LOPART::LOPART(some.signal, labels, 5)
  timing <- microbenchmark(
    LOPART={
      LOPART::LOPART(some.signal, labels, 5)
    },
    BinSegAll={
      binsegRcpp::binseg_normal(some.signal)
    },
    BinSeg={
      binsegRcpp::binseg_normal(
        some.signal,
        max.segments=nrow(Lfit[["segments"]]))
    },
    FPOP={
      fpop::Fpop(some.signal, 5)
    },
    SegAnnot={
      SegAnnot::SegAnnot(
        some.signal, as.integer(labels$start), as.integer(labels$end))
    }, 
    OPART={
      LOPART::LOPART(some.signal, labels[0], 5)
    },
    times=3)
  timing_list[[paste(size)]] <- data.table(size, timing)
}
timing.dt <- do.call(rbind, timing_list)
data.table::fwrite(timing.dt, "figure-timings-data-labels.csv")

## experiment 2: variable number of data, constant number of labels
size.vec <- unique(ceiling(10^seq(1, log10(nrow(all.labels)), l=10)))
timing_list <- list()
for(size.i in seq_along(size.vec)){
  size <- size.vec[[size.i]]
  cat(sprintf("%4d / %4d labels=%d\n", size.i, length(size.vec), size))
  labels <- all.labels[1:size]
  n.data <- max(labels$end)
  some.data <- signal[1:n.data]
  for(data.over.labels in c(10, 1000)){
    n.labels <- n.data / data.over.labels
    some.labels <- labels[seq(1, .N, l=n.labels)]
    Lfit <- LOPART::LOPART(some.data, some.labels, 5)
    m.args <- list(
      LOPART=substitute(LOPART::LOPART(some.data, some.labels, 5)),
      SegAnnot=substitute(
        SegAnnot::SegAnnot(
          some.data, as.integer(some.labels$start), as.integer(some.labels$end))
      ),
      BinSegAll=substitute(
        binsegRcpp::binseg_normal(some.data)
      ),
      BinSeg=substitute(
        binsegRcpp::binseg_normal(
          some.data,
          max.segments=nrow(Lfit[["segments"]]))
      ),
      FPOP=substitute(fpop::Fpop(some.data, 5)),
      times=3)
    if(n.data <= 40000){
      m.args[["OPART"]] <- substitute(
        LOPART::LOPART(some.data, labels[0], 5))
    }
    timing <- do.call(microbenchmark, m.args)
    timing_list[[paste(data.over.labels, size)]] <- data.table(
      data.over.labels, size, timing, n.data, n.labels,
      label.density=n.labels/n.data)
  }
}
timing.dt <- do.call(rbind, timing_list)
data.table::fwrite(timing.dt, "figure-timings-data.csv")
