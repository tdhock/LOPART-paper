source("packages.R")

err.dt <- data.table(
  csv=Sys.glob("figure-label-errors-data/*.csv")
)[, data.table::fread(
  csv,
  colClasses=list(character=5)
), by=csv]
err.dt[model.name=="LOPART" & set=="train", table(errors)]
err.dt[model.name=="LOPART" & set=="train" & 0<errors, .(
  csv, test.fold, set, penalty, fp, fn)]

err.test <- err.dt[set=="test"]
err.test[, log.penalty := log(penalty)]
err.test[, min.log.lambda := log.penalty + c(
  -Inf, -diff(log.penalty)/2
), by=.(
  model.name, test.fold, sequenceID)]
err.test[, max.log.lambda := c(
  min.log.lambda[-1], Inf
), by=.(
  model.name, test.fold, sequenceID)]
err.test[, .(min.log.lambda, log.penalty, max.log.lambda)]

err.train <- err.dt[model.name=="OPART" & set=="train", .(
  train.errors=sum(errors)
), by=.(test.fold, penalty)]
best.penalty <- err.train[, .SD[which.min(train.errors)], by=test.fold]
err.pred <- err.test[best.penalty, on=.(test.fold, penalty)]
err.pred[, pred.log.lambda := log(penalty)]

auc.dt <- err.test[, {
  select.dt <- data.table(test.fold, model.name)
  model.dt <- .SD[order(sequenceID, min.log.lambda)]
  roc.list <- penaltyLearning::ROChange(
    model.dt,
    err.pred[select.dt, on=.(test.fold, model.name)],
    problem.vars="sequenceID")
  with(roc.list, data.table(
    roc=list(roc), auc,
    thresholds[threshold=="predicted"]))
}, by=.(test.fold, model.name)]

roc.dt <- auc.dt[, data.table(
  roc[[1]]
), by=.(test.fold, model.name)]
possible.dt <- unique(auc.dt[, .(
  test.fold, possible.fp, possible.fn)])
pred.point.dt <- rbind(
  data.table(
    FPR=0, TPR=0, fp=0, tp=0, auc=NA,
    model.name="SegAnnot", test.fold=1:2
  ),
  auc.dt[, .(
    FPR, TPR, fp, tp, auc,
    model.name, test.fold
  )]
)[possible.dt, on=.(test.fold)]
pred.text.dt <- rbind(
  data.table(test.fold=1, rbind(
    data.table(model.name="LOPART", FPR=0.7, TPR=0.79),
    data.table(model.name="OPART", FPR=0.5, TPR=0.42))),
  data.table(test.fold=2, rbind(
    data.table(model.name="LOPART", FPR=0.7, TPR=0.8),
    data.table(model.name="OPART", FPR=0.6, TPR=0.43))),
  data.table(test.fold=1:2, model.name="SegAnnot", FPR=0.45, TPR=0.1))
segs.dt <- pred.point.dt[pred.text.dt, on=.(test.fold, model.name)]
algo.colors <- c(
  OPART="deepskyblue",
  LOPART="black",
  SegAnnot="darkgreen")
gg <- ggplot()+
  theme_bw()+
  scale_color_manual(values=algo.colors)+
  scale_size_manual(values=c(
    LOPART=1.5,
    OPART=1))+
  geom_path(aes(
    FPR, TPR,
    color=model.name,
    size=model.name,
    group=paste(model.name, test.fold)),
    data=roc.dt)+
  geom_point(aes(
    FPR, TPR,
    color=model.name),
    size=3,
    data=pred.point.dt)+
  geom_segment(aes(
    FPR, TPR,
    xend=i.FPR, yend=i.TPR,
    color=model.name),
    data=segs.dt)+
  geom_label(aes(
    i.FPR, i.TPR,
    color=model.name,
    label=sprintf(
      "%s\nFPR=%d/%d=%.1f%%\nTPR=%d/%d=%.1f%%%s",
      model.name,
      fp, possible.fp, 100*fp/possible.fp,
      tp, possible.fn, 100*tp/possible.fn,
      ifelse(is.na(auc), "", sprintf("\nauc=%.3f", auc)))),
    size=3,
    data=segs.dt)+
  theme(
    panel.spacing=grid::unit(0, "lines"),
    legend.position="none"
  )+
  facet_grid(. ~ test.fold, labeller=label_both)+
  coord_equal()+
  scale_x_continuous(
    "False Positive Rate (test set labels)",
    breaks=c(0, 0.5, 1),
    labels=c("0", "0.5", "1"))+
  scale_y_continuous(
    "True Positive Rate (test set labels)",
    breaks=c(0, 0.5, 1),
    labels=c("0", "0.5", "1"))
pdf("figure-cv-roc.pdf", width=5, height=3)
print(gg)
dev.off()

(fold.model.err.tall <- err.pred[, .(
  test.errors=sum(errors),
  test.labels=sum(labels),
  possible.fn=sum(possible.fn)
), by=.(test.fold, model.name)])
fold.model.err.tall[, prop.errors := test.errors / test.labels]
fold.model.err.wide <- dcast(
  fold.model.err.tall,
  test.fold ~ model.name,
  value.var = "prop.errors")
fold.model.err.wide[, diff := OPART-LOPART]
fold.model.err.wide

SegAnnot.tall <- melt(
  err.pred[model.name=="LOPART"],
  measure.vars=c("possible.fn", "errors"))
algo.map <- c(
  errors="LOPART",
  possible.fn="SegAnnot")
SegAnnot.tall[, algo := algo.map[variable] ]
SegAnnot.wide <- dcast(
  SegAnnot.tall,
  test.fold + sequenceID ~ algo,
  value.var = "value")
SegAnnot.wide[, diff := SegAnnot-LOPART]
SegAnnot.wide[, .(
  mean=mean(diff),
  sd=sd(diff),
  count=.N
)]
one.id <- SegAnnot.wide[order(diff)][.N]
err.pred[one.id, on=.(test.fold, sequenceID)]
SegAnnot.wide.counts <- SegAnnot.wide[, .(
  count=.N
), keyby=.(test.fold, diff)]
SegAnnot.sign.counts <- SegAnnot.wide.counts[, .(
  sum.count=sum(count)
), keyby=.(test.fold, sign=sign(diff))]
SegAnnot.sign.counts[, total := sum(sum.count), by=test.fold]
SegAnnot.sign.counts[, percent := 100*sum.count/total]
my.title <- ggtitle("Learned constant penalty")
gg <- ggplot()+
  my.title+
  geom_tile(aes(
    diff, factor(test.fold),
    fill=log10(count)),
    data=SegAnnot.wide.counts)+
  coord_equal()+
  scale_fill_gradient(
    "log10(seqs)",
    low="white",
    high="orange")+
  theme_bw()+ 
  geom_text(aes(
    diff, factor(test.fold),
    label=count),
    data=SegAnnot.wide.counts)+
  ylab("Test fold")+
  xlab("Test error difference (SegAnnot-LOPART)")
print(gg)
pdf("figure-cv-SegAnnot.pdf", width=5, height=2)
print(gg)
dev.off()

prob.err.wide <- dcast(
  err.pred,
  test.fold + sequenceID ~ model.name,
  value.var = "errors")
prob.err.wide[, diff := OPART-LOPART]
prob.err.wide.counts <- prob.err.wide[, .(
  count=.N
), keyby=.(test.fold, diff)]

prob.err.sign.counts <- prob.err.wide.counts[, .(
  sum.count=sum(count)
), keyby=.(test.fold, sign=sign(diff))]
prob.err.sign.counts[, total := sum(sum.count), by=test.fold]
prob.err.sign.counts[, percent := 100*sum.count/total]

gg <- ggplot()+
  my.title+
  geom_tile(aes(
    diff, factor(test.fold),
    fill=log10(count)),
    data=prob.err.wide.counts)+
  coord_equal()+
  scale_fill_gradient(
    "log10(seqs)",
    low="white",
    high="orange")+
  theme_bw()+ 
  geom_text(aes(
    diff, factor(test.fold),
    label=count),
    data=prob.err.wide.counts)+
  ylab("Test fold")+
  xlab("Test error difference (OPART-LOPART)")
pdf("figure-cv.pdf", width=4, height=2)
print(gg)
dev.off()
