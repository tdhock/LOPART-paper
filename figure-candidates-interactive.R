library(animint2)
library(data.table)
works_with_R('3.6.0', animint2="2019.7.3", data.table="1.12.8")

viz.data <- readRDS("figure-candidates-interactive-data.rds")

dir.create("figure-candidates-interactive-data")
for(data.type in names(viz.data)){
  dt <- viz.data[[data.type]]
  out.csv <- file.path(
    "figure-candidates-interactive-data",
    paste0(data.type, ".csv"))
  data.table::fwrite(dt, out.csv)
}


viz.data$cost[, last.change := tau]
best.overall <- viz.data$cost[up.to.t == 100, {
  .SD[which.min(cost_candidates)]
}, by=.(penalty, Algorithm)]
normalize <- function(x){
  d <- max(x)-min(x)
  if(d==0) 0 else (x-min(x))/d
}
pen.seg.dt <- viz.data$segs[best.overall, .(
  segments=.N
), by=.EACHI, on=.(penalty, Algorithm, up.to.t, last.change)]
viz.data$cost[, relative.cost := normalize(
  cost_candidates), by=.(penalty, up.to.t)]
viz.data$cost[["show.cost"]] <- viz.data$cost[["relative.cost"]]
##viz.data$cost[["show.cost"]] <- viz.data$cost[["cost_mean"]]
viz.data$labels[, Changes := paste(changes)]
last.change.dt <- unique(viz.data$segs[, .(up.to.t, last.change)])
last.change.dt[, model.i := 1:.N]
up.to.dt <- unique(viz.data$cost[, .(up.to.t)])
abbrev.vec <- c(
  data="Data and models",
  cost="Cost")
yfac <- function(l){
  factor(abbrev.vec[[l]], abbrev.vec)
}
COST <- function(dt){
  data.table(y.var=yfac("cost"), dt)
}
DATA <- function(dt){
  data.table(y.var=yfac("data"), dt)
}
sig.color <- "grey50"
my.hjust <- function(x)ifelse(x < nrow(viz.data$signal)/2, 0, 1)
min.dt <- viz.data$cost[, {
  .SD[which.min(cost_candidates)]
}, by=.(penalty, Algorithm, up.to.t)]
t.y <- 12.5
label.colors <- c(
  "1"="#ff7d7d",
  "0"="#f6c48f")
gg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  theme_animint(width=900, height=300)+
  geom_blank(aes(
    x, y),
    data=DATA(data.table(x=1, y=t.y+0.5)))+
  geom_text(aes(
    up.to.t, t.y,
    key=1,
    hjust=my.hjust(up.to.t),
    label=paste0("t=", up.to.t)),
    color=sig.color,
    showSelected="up.to.t",
    data=DATA(up.to.dt))+
  geom_tallrect(aes(
    xmin=start, xmax=end,
    fill=Changes),
    alpha=0.5,
    color=NA,
    data=viz.data$labels)+
  scale_fill_manual("label", values=label.colors)+
  facet_grid(y.var ~ ., scales="free")+
  geom_vline(aes(
    xintercept=last.change+0.5,
    key=last.change),
    showSelected=c("up.to.t", "last.change"),
    data=last.change.dt)+
  geom_text(aes(
    last.change+0.5, 1,
    key=last.change,
    hjust=my.hjust(last.change),
    label=paste0("tau=", last.change)),
    showSelected=c("up.to.t", "last.change"),
    vjust=0,
    data=DATA(last.change.dt))+
  geom_point(aes(
    position, signal),
    color=sig.color,
    size=4,
    data=DATA(viz.data$signal))+
  scale_size_manual(values=c(
    OPART=3,
    LOPART=1.5),
    drop=FALSE)+
  scale_color_manual(values=c(
    OPART="deepskyblue",
    LOPART="black"),
    drop=FALSE)+
  ylab("")+
  scale_x_continuous(
    "position t,tau",
    breaks=seq(0, 100, by=10))
point.size <- c(
  OPART=5,
  LOPART=2)
for(algo in names(point.size)){
  algo.segs <- viz.data$segs[algo==Algorithm]
  algo.segs[, seg.i := .N:1, by=.(up.to.t, last.change)]
  seg.i.counts <- algo.segs[, .(count=.N), by=.(seg.i)]
  algo.last.changes <- unique(algo.segs[, .(last.change, up.to.t)])
  algo.last.changes[, model.i := 1:.N]
  algo.models <- data.table(expand.grid(
    model.i=algo.last.changes$model.i,
    seg.i=seg.i.counts$seg.i
  ))[algo.last.changes, on="model.i"]
  algo.na <- algo.segs[algo.models, on=.(up.to.t, last.change, seg.i)]
  algo.na[, mean := nafill(mean, "locf")]
  algo.na[, Algorithm := Algorithm[1] ]
  gg <- gg+
    geom_segment(aes(
      start-0.5, mean,
      key=paste(start, end),
      size=Algorithm,
      color=Algorithm,
      xend=end+0.5, yend=mean),
      showSelected=c("up.to.t", "last.change", "penalty"),
      chunk_vars=c("penalty", "up.to.t"),
      data=DATA(algo.segs))+
    ## geom_segment(aes(
    ##   ifelse(is.na(start), 0.5, start-0.5), mean,
    ##   key=seg.i,
    ##   size=Algorithm,
    ##   color=Algorithm,
    ##   xend=ifelse(is.na(end), 0.5, end+0.5), yend=mean),
    ##   showSelected=c("up.to.t", "last.change", "penalty"),
    ##   chunk_vars=c("penalty", "up.to.t"),
    ##   data=DATA(algo.na))+
    geom_point(aes(
      change, show.cost,
      key=change,
      color=Algorithm),
      size=point.size[[algo]],
      fill=NA,
      showSelected=c("penalty", "up.to.t"),
      chunk_vars=c("penalty", "up.to.t"),
      data=COST(viz.data$cost[Algorithm==algo]))+
    geom_point(aes(
      change, show.cost,
      key=1,
      color=Algorithm),
      size=point.size[[algo]],
      showSelected=c("penalty", "up.to.t"),
      data=COST(min.dt[Algorithm==algo]))
}
viz <- animint(
  title="LOPART algorithm",
  signalCost=gg+
    ggtitle("Data/model and cost for selected penalty")+
    geom_tallrect(aes(
      key=last.change,
      xmin=last.change,
      xmax=last.change+1),
      showSelected=c("up.to.t"),
      alpha=0.5,
      color=sig.color,
      clickSelects="last.change",
      data=COST(last.change.dt))+
    geom_tallrect(aes(
      xmin=up.to.t-0.5, xmax=up.to.t+0.5),
      color=sig.color,
      alpha=0.5,
      clickSelects="up.to.t",
      data=DATA(up.to.dt)),
  cost=ggplot()+
    ggtitle("Cost for selected penalty")+
    coord_equal()+
    geom_tile(aes(
      up.to.t, last.change,
      key=paste(up.to.t, last.change),
      fill=relative.cost),
      showSelected=c("penalty", "Algorithm"),
      data=viz.data$cost)+
    geom_tallrect(aes(
      xmin=up.to.t-0.5, xmax=up.to.t+0.5),
      color=sig.color,
      alpha=0.3,
      clickSelects="up.to.t",
      data=up.to.dt)+
    ## geom_point(aes(
    ##   up.to.t, last.change,
    ##   key=last.change,
    ##   fill=relative.cost),
    ##   data=viz.data$cost,
    ##   alpha=0.6,
    ##   showSelected="up.to.t",
    ##   clickSelects="last.change")+
    geom_tile(aes(
      up.to.t, last.change,
      key=last.change,
      fill=relative.cost),
      data=viz.data$cost,
      showSelected=c("penalty", "up.to.t", "Algorithm"),
      size=0.5,
      clickSelects="last.change")+
    geom_point(aes(
      up.to.t, last.change,
      key=1),
      data=viz.data$cost,
      showSelected=c("penalty", "up.to.t", "last.change", "Algorithm"))+
    scale_fill_gradient(low="red", high="grey90")+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=500, height=300)+
    facet_grid(. ~ Algorithm),
  penalties=ggplot()+
    ggtitle("Select penalty")+
    theme_bw()+
    theme(
      legend.position="none",
      panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=300, height=300)+
    geom_line(aes(
      log10(penalty), log10(segments),
      color=Algorithm, group=Algorithm),
      showSelected="Algorithm",
      data=pen.seg.dt)+
    scale_color_manual(values=c(
      OPART="deepskyblue",
      LOPART="black"),
      drop=FALSE)+
    geom_tallrect(aes(
      xmin=log10(penalty)-0.5,
      xmax=log10(penalty)+0.5),
      data=pen.seg.dt,
      color="grey",
      alpha=0.5,
      clickSelects="penalty"),
  first=list(penalty=10),
  duration=list(up.to.t=2000, last.change=2000, penalty=2000)
)
animint2dir(viz, "figure-candidates-interactive", open.browser=FALSE)


