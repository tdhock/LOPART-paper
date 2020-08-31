source("packages.R")
options(bitmapType="cairo")
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
gg.data <- ggplot()+
  geom_point(aes(
    position, signal),
    color="grey50",
    data=signal.dt)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))
label.list <- list(
  OPART=labels[0,],
  SegAnnot=labels)
png("figure-baselines-data.png", width=6, height=4, units="in", res=100)
print(gg.data)
dev.off()

gg.labels <- gg.data+
  geom_rect(aes(
    xmin=start, xmax=end,
    fill=paste(changes),
    ymin=-Inf, ymax=Inf),
    alpha=0.5,
    data=labels)+
  scale_fill_manual("label", values=label.colors)
png("figure-baselines-labels.png", width=6, height=4, units="in", res=100)
print(gg.labels)
dev.off()

lt.values <- c(
  correct=0,
  "false negative"=3,
  "false positive"=1)

frame.list <- list()
for(penalty in c(1000, 100, 20, 15, 10, 5, 4, 0, Inf)){
  algo.name <- if(penalty==Inf)"SegAnnot" else "OPART"
  model.color <- if(algo.name=="OPART")"deepskyblue" else "blue"
  fit <- LOPART::LOPART(
    signal,
    label.list[[algo.name]],
    penalty)
  err.list <- penaltyLearning::labelError(
    data.table(problem="sim", fit[["loss"]]),
    labels.dt[, data.table(
      problem="sim", start, end,
      annotation=ifelse(changes==1, "1breakpoint", "0breakpoints"))],
    data.table(problem="sim", penalty_unlabeled=penalty, fit[["changes"]]),
    change.var="change",
    label.vars=c("start", "end"),
    model.vars="penalty_unlabeled",
    problem.vars=c("problem"))
  label.errors <- data.table(
    err.list[["label.errors"]])[, .(
      start, end, error.type=factor(status, names(lt.values)))]
  this.gg <- gg.labels+
    geom_rect(aes(
      xmin=start, xmax=end,
      ymin=-Inf, ymax=Inf,
      linetype=error.type),
      size=1.5,
      color="black",
      fill=NA,
      data=label.errors)+
    scale_linetype_manual(
      "error type",
      values=lt.values,
      drop=FALSE)+
    geom_segment(aes(
      start-0.5, mean,
      xend=end+0.5, yend=mean),
      data=fit[["segments"]],
      size=1.5,
      color=model.color)+
    geom_vline(aes(
      xintercept=change),
      data=fit[["changes"]],
      color=model.color)
  this.png <- paste0(
    "figure-baselines-penalty=",
    gsub("[.]", "_", penalty), ".png")
  png(this.png, width=6, height=4, units="in", res=100)
  print(this.gg)
  dev.off()
  frame.title <- if(algo.name=="SegAnnot"){
    "SegAnnot (no changes in unlabeled regions)"
  }else{
    sprintf(
      "OPART with penalty $\\lambda=%s$ (ignores labels)",
      paste(penalty))
  }
  frame.list[[paste(penalty)]] <- sprintf("
\\begin{frame}
  \\frametitle{%s}
  \\includegraphics[width=\\textwidth]{%s}
\\end{frame}
", frame.title, this.png)
}#penalty
cat(paste(frame.list, collapse="\n"), file="figure-baselines.tex")
##system("pdflatex slides")
