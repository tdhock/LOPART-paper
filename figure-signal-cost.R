source("packages.R")

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
  theme(panel.spacing=grid::unit(0, "lines"))
label.list <- list(
  OPART=labels[0,],
  LOPART=labels)
seg.dt.list <- list()
change.dt.list <- list()
cost.dt.list <- list()
for(model.name in names(label.list)){
  label.dt <- data.table(label.list[[model.name]])
  fit <- LOPART::LOPART(signal, label.dt, 10)
  end <- as.integer(fit$end.vec)
  start <- as.integer(c(1, end[-length(end)]+1))
  Algorithm <- factor(model.name, names(label.list))
  tau.dt <- with(fit$cost, data.table(
    cost_candidates,
    tau=seq_along(cost_candidates)-1,
    change=seq_along(cost_candidates)-0.5
  ))[cost_candidates != -Inf]
  cost.dt.list[[model.name]] <- data.table(Algorithm, tau.dt)
  seg.dt.list[[model.name]] <- data.table(Algorithm, fit$segments)
  change.dt.list[[model.name]] <- data.table(Algorithm, fit$changes)
}
seg.dt <- do.call(rbind, seg.dt.list)[, .(Algorithm, mean, start, end)]
change.dt <- do.call(rbind, change.dt.list)
cost.dt <- do.call(rbind, cost.dt.list)
abbrev.vec <- c(
  data="data and models",
  cost="cost of last change")
yfac <- function(l){
  factor(abbrev.vec[[l]], abbrev.vec)
}
COST <- function(dt){
  data.table(y.var=yfac("cost"), dt)
}
DATA <- function(dt){
  data.table(y.var=yfac("data"), dt)
}
sig.text.dt <- signal.dt[c(1, .N), .(
  signal, i=position, position=position+c(-1,1), hjust=c(1, 0))]
cost.text.dt <- cost.dt[
  Algorithm=="OPART"
][c(1, .N), .(
  pos=change, cost_candidates, tau,
  hjust=c(0.5, 0.5),
  vjust=c(2.5, -1.5)
)]
sig.color <- "grey50"
min.dt <- cost.dt[, .SD[which.min(cost_candidates)], by=Algorithm]
min.dt[, hjust := ifelse(Algorithm=="OPART", 0, 1)]
min.dt[, y := cost.dt[is.finite(cost_candidates), mean(range(cost_candidates))] ]
gg.data <- ggplot()+
  geom_text(aes(
    position, signal, hjust=hjust,
    label=sprintf("$x_{%d}$", i)),
    color=sig.color,
    data=DATA(sig.text.dt))+
  geom_rect(aes(
    xmin=start, xmax=end,
    fill=paste(changes),
    ymin=-Inf, ymax=Inf),
    alpha=0.5,
    data=labels)+
  scale_fill_manual("label", values=label.colors)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  geom_point(aes(
    position, signal),
    color=sig.color,
    shape=1,
    data=DATA(signal.dt))+
  geom_text(aes(
    start, y,
    label=sprintf("$\\underline p_{%d}=%d$", seq_along(start), start)),
    hjust=1,
    data=DATA(labels.dt))+
  geom_text(aes(
    (start+end)/2, y+1,
    label=sprintf("$y_{%d}=%d$", seq_along(start), changes)),
    data=DATA(labels.dt))+
  geom_text(aes(
    end, y,
    label=sprintf("$\\overline p_{%d}=%d$", seq_along(start), end)),
    hjust=0,
    data=DATA(labels.dt))
tikz("figure-signal.tex", width=4.5, height=2.2)
print(gg.data+theme(legend.position="none")+xlim(0, 105))
dev.off()
  
gg.model <- gg.data+
  facet_grid(y.var ~ ., scales="free")+
  geom_vline(aes(
    xintercept=change,
    size=Algorithm,
    color=Algorithm),
    data=change.dt)+
  geom_segment(aes(
    start-0.5, mean,
    size=Algorithm,
    color=Algorithm,
    xend=end+0.5, yend=mean),
    data=DATA(seg.dt))+
  scale_size_manual(values=c(
    OPART=1,
    LOPART=0.5))+
  scale_color_manual(values=c(
    OPART="deepskyblue",
    LOPART="black"))+
  ylab("")+
  scale_x_continuous(
    "Position in data sequence",
    limits=c(0, 105),
    breaks=c(1, seq(10, 100, by=10)))+
  geom_text(aes(
    pos, cost_candidates,
    hjust=hjust,
    vjust=vjust,
    label=sprintf("$\\tau = %d$", tau)),
    data=COST(cost.text.dt))+
  geom_text(aes(
    change, y,
    hjust=hjust,
    color=Algorithm,
    label=sprintf("$\\tau^*_{%d} = %d$", nrow(signal.dt), tau)),
    data=COST(min.dt))+
  geom_point(aes(
    change, cost_candidates,
    color=Algorithm, shape=Algorithm),
    data=COST(cost.dt))+
  scale_shape_manual(values=c(OPART=1, LOPART=2))
w=6.5
h=2.4
tikz("figure-signal-cost-standAlone.tex", width=w, height=h, standAlone=TRUE)
print(gg.model)
dev.off()
system("pdflatex figure-signal-cost-standAlone")
tikz("figure-signal-cost.tex", width=w, height=h)
print(gg.model)
dev.off()

