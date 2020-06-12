library(animint2)
library(data.table)
library(LOPART)
works_with_R('3.6.0', animint2="2019.7.3", data.table="1.12.8")

set.seed(2)
signal <- c(
  rnorm(25, mean = 10),
  rnorm(25, mean = 7),
  rnorm(25, mean = 8),
  rnorm(25, mean = 5))
#outliers
signal[86] <- 10
labels <- data.table(
  start = c(20, 45, 80),
  end = c(30, 55, 90),
  changes = c(1, 1, 0))
labels.dt <- data.table(
  labels,
  y=c(9, 4, 11))
cumsum.sq.vec <- cumsum(signal^2)
signal.dt <- data.table(
  signal,
  position=seq_along(signal))
label.colors <- c(
  "1"="#ff7d7d",
  "0"="#f6c48f")
gg <- ggplot()+
  geom_rect(aes(
    xmin=start, xmax=end,
    fill=paste(changes),
    ymin=-Inf, ymax=Inf),
    alpha=0.5,
    data=labels)+
  geom_point(aes(
    position, signal),
    data=signal.dt)+
  scale_fill_manual("label", values=label.colors)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))
label.list <- list(
  OPART=labels[0,],
  LOPART=labels)

N <- length(signal)
up.to.dt <- data.table(expand.grid(
  t=1:N,
  tau=0:(N-1)
))[tau<t]
run.LOPART <- function(n_updates){
  LOPART::LOPART(
    signal, label.dt, penalty, n_updates=n_updates, penalty_labeled=penalty)
}
unique.tau <- up.to.dt[0 < tau, unique(tau)]

seg.dt.list <- list()
cost.dt.list <- list()
for(penalty in 10^seq(-2, 3)){
  segs.up.to <- list()
  for(tau in unique.tau){
    for(model.name in names(label.list)){
      label.dt <- data.table(label.list[[model.name]])
      fit <- run.LOPART(tau)
      segs.up.to[[paste(tau, model.name)]] <- fit$segments
    }
  }
  for(up.to.t in unique(up.to.dt$t)){
    tau.dt <- up.to.dt[t==up.to.t]
    for(model.name in names(label.list)){
      label.dt <- data.table(label.list[[model.name]])
      fit <- run.LOPART(up.to.t)
      Algorithm <- factor(model.name, names(label.list))
      this.cost.dt <- with(fit$cost, data.table(
        cost_candidates,
        cost_mean=(cost_candidates+cumsum.sq.vec[up.to.t])/up.to.t,
        tau=seq_along(cost_candidates)-1,
        change=seq_along(cost_candidates)-0.5
      ))[cost_candidates < Inf]
      if(nrow(this.cost.dt)){
        cost.dt.list[[paste(penalty, Algorithm, up.to.t)]] <- data.table(
          penalty,
          Algorithm,
          up.to.t,
          this.cost.dt)
      }
      for(last.change in this.cost.dt$tau){
        prev.segs <- segs.up.to[[paste(last.change, model.name)]]
        start <- last.change+1
        last.seg <- data.table(
          start, end=up.to.t, mean=mean(signal[start:up.to.t]))
        seg.dt.list[[paste(
          penalty, Algorithm, last.change, up.to.t)]] <- data.table(
          penalty,
          Algorithm,
          last.change,
          up.to.t,
          rbind(prev.segs, last.seg))
      }#last.change
    }#model.name
  }#t
}#penalty
seg.dt <- do.call(rbind, seg.dt.list)
cost.dt <- do.call(rbind, cost.dt.list)

unique(seg.dt[, .(up.to.t, last.change)])
unique(cost.dt[, .(up.to.t, last.change)])
viz.data <- list(
  segs=seg.dt,
  cost=cost.dt,
  labels=labels,
  signal=signal.dt)
saveRDS(viz.data, "figure-candidates-interactive-data.rds")

