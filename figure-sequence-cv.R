source("packages.R")

err.dt <- data.table(
  csv=Sys.glob("figure-sequence-cv-data/*.csv")
)[, data.table::fread(
  csv,
  colClasses=list(character=5)
), by=csv]
err.dt[model.name=="LOPART" & set=="train", table(errors)]
err.dt[model.name=="LOPART" & set=="train" & 0<errors, .(
  csv, test.fold, set, penalty, fp, fn)]

fn.dt <- err.dt[set=="test", .(
  fn=possible.fn[1]
), by=.(sequenceID, test.fold)]

SegAnnot.compare <- err.dt[
  set=="test" & model.name=="LOPART",
  .SD[which.min(errors)],
  by=.(sequenceID, test.fold)]
## segannot always has FN = possible.fn and FP=0, so how often are we
## better on one or both axes?
SegAnnot.compare[, fewer.FN := possible.fn-fn]
SegAnnot.compare[, table(fp, fewer.FN)]
SegAnnot.compare[, table(fewer.FN>fp)]

SegAnnot.compare.counts <- SegAnnot.compare[, .(
  test.sets=.N
), by=.(fewer.FN, fp)]
SegAnnot.compare.counts[, .(
  test.sets=sum(test.sets)
), by=.(same = fp==fewer.FN)]
my.title <- ggtitle("Best penalty")
scale.fill <- scale_fill_gradient(
  "log10(seqs)",
  low="white",
  high="violet")
gg <- ggplot()+
  ##ggtitle("LOPART is more accurate than SegAnnot")+
  my.title+
  geom_abline(aes(
    slope=slope, intercept=intercept, color=test.errors),
    data=data.table(slope=1, intercept=0, test.errors="equal"))+
  scale_color_manual(values=c(equal="grey"), guide=FALSE)+
  geom_tile(aes(
    fewer.FN, fp, fill=log10(test.sets)),
    alpha=0.8,
    data=SegAnnot.compare.counts)+
  geom_text(aes(
    fewer.FN, fp, label=test.sets),
    data=SegAnnot.compare.counts)+
  scale.fill+
  coord_equal()+
  theme_bw()+
  scale_x_continuous(
    "Number of LOPART true positive labels
(SegAnnot always has true positives=0)")+
  scale_y_continuous(
    "Number of LOPART
false positive labels
(SegAnnot always has
false positives=0)")
pdf("figure-sequence-cv-SegAnnot.pdf", width=5, height=2.3)
print(gg)
dev.off()

total.dt <- dcast(
  data.table(data.frame(err.dt)),
  sequenceID + test.fold + model.name + penalty ~ set,
  value.var="errors")
total.dt[, train.test := test+train]
total.min <- total.dt[, .SD[
  which.min(train.test)], by=.(sequenceID, test.fold, model.name)]
total.min.wide <- dcast(
  total.min,
  sequenceID + test.fold ~ model.name,
  value.var=c("test", "train", "train.test"))
total.min.wide[, diff := train.test_OPART-train.test_LOPART]
total.min.wide[, .(
  prob.folds=.N
), keyby=.(diff)]
total.min.wide[diff<0]
total.min.wide[, train.test.diff := train.test_OPART - train.test_LOPART]
mytab <- function(dt, col.name){
  errors <- dt[, .(
    count=.N,
    percent=100*.N/nrow(dt)
  ), by=col.name]
  is.zero <- errors[[col.name]] == 0
  nonzero <- errors[!is.zero]
  sum.wide <- data.table(
    sum.count=sum(errors$count),
    zero.count=errors$count[is.zero],
    nonzero.count=sum(nonzero$count),
    nonzero.min=min(nonzero[[col.name]]),
    nonzero.max=max(nonzero[[col.name]]))
  sum.tall <- melt(sum.wide, measure.vars=names(sum.wide))
  sum.tall[grepl("count", variable), percent := 100*value/nrow(dt) ]
  list(
    errors=errors,
    summary=sum.tall)
}
mytab(total.min.wide, "train_OPART")

total.min.wide[, test.diff := test_OPART-test_LOPART]
mytab(total.min.wide, "test.diff")

train.test.counts <- total.min.wide[, .(
  splits=.N
), by=.(train_OPART, test.diff)]
gg <- ggplot()+
  ##ggtitle("LOPART is more accurate\nthan OPART")+
  my.title+
  geom_hline(yintercept=0, color="grey")+
  geom_vline(xintercept=0, color="grey")+
  geom_tile(aes(
    train_OPART, test.diff, fill=log10(splits)),
    alpha=0.8,
    data=train.test.counts)+
  geom_text(aes(
    train_OPART, test.diff, label=splits),
    data=train.test.counts)+
  scale.fill+
  coord_equal()+
  theme_bw()+
  scale_x_continuous(
    "OPART train label errors
(LOPART is always=0)")+
  scale_y_continuous(
    "Test label error difference
(OPART-LOPART)")
pdf("figure-sequence-cv-OPART.pdf", width=3, height=2.3)
print(gg)
dev.off()

err.dt[, log.penalty := log(penalty)]
err.dt[, min.log.lambda := log.penalty + c(
  -Inf, -diff(log.penalty)/2
), by=.(
  model.name, test.fold, set, sequenceID)]
err.dt[, max.log.lambda := c(
  min.log.lambda[-1], Inf
), by=.(
  model.name, test.fold, set, sequenceID)]
err.test <- err.dt[set=="test"]

err.train <- err.dt[set=="train" & model.name=="OPART"]

feature.dt <- data.table::fread(
  "data-for-LOPART-signals.csv.gz"
)[, .(
  log.log.data=log(log(.N))
), by=sequenceID]
feature.mat <- feature.dt[, matrix(
  log.log.data,
  ncol=1,
  dimnames=list(sequenceID=sequenceID, feature="log.log.data"))]

pred.dt <- err.train[, {
  best.penalty <- .SD[, .(
    train.errors=sum(errors)
  ), by=penalty][which.min(train.errors)]
  target.dt <- penaltyLearning::targetIntervals(.SD, "sequenceID")
  target.mat <- target.dt[
    rownames(feature.mat),
    cbind(min.log.lambda, max.log.lambda),
    on="sequenceID"]
  keep <- -Inf < target.mat[, 1] | target.mat[,2] < Inf
  fit <- penaltyLearning::IntervalRegressionUnregularized(
    feature.mat[keep, , drop=FALSE], target.mat[keep, ])
  print(fit)
  rbind(
    data.table(
      sequenceID=rownames(feature.mat),
      Penalty="constant",
      Parameters=1,
      pred.log.lambda=log(best.penalty$penalty)),
    data.table(
      sequenceID=rownames(feature.mat),
      Penalty="BIC",
      Parameters=0,
      pred.log.lambda=as.numeric(feature.mat)),
    data.table(
      sequenceID=rownames(feature.mat),
      Penalty="linear",
      Parameters=2,
      pred.log.lambda=as.numeric(fit$predict(feature.mat))))
}, by=test.fold]

auc.dt <- err.test[, {
  select.dt <- data.table(test.fold)
  pred.fold <- pred.dt[select.dt, on="test.fold"]
  model.dt <- .SD[order(sequenceID, min.log.lambda)]
  pred.fold[, {
    roc.list <- penaltyLearning::ROChange(
      model.dt,
      .SD[select.dt, on="test.fold"],
      problem.vars="sequenceID")
    with(roc.list, data.table(
      roc=list(roc), auc,
      thresholds[threshold=="predicted"]))
  }, by=.(Penalty, Parameters)]
}, by=.(test.fold, model.name)]

roc.dt <- auc.dt[, data.table(
  roc[[1]]
), by=.(test.fold, model.name, Penalty, Parameters)]
possible.dt <- unique(auc.dt[, .(
  test.fold, possible.fp, possible.fn)])
pred.point.dt <- rbind(
  auc.dt[model.name=="LOPART", data.table(
    FPR=0, TPR=0, fp=0, tp=0, auc=NA, labels,
    model.name="SegAnnot", test.fold, Penalty, Parameters
  )],
  auc.dt[, .(
    FPR, TPR, fp, tp, auc, labels,
    model.name, test.fold, Penalty, Parameters
  )]
)[possible.dt, on=.(test.fold)]
algo.colors <- c(
  OPART="#0077CC",
  LOPART="black",
  SegAnnot="#22CC22")
gg <- ggplot()+
  theme_bw()+
  scale_color_manual(values=algo.colors)+
  scale_size_manual(values=c(
    LOPART=1.5,
    OPART=1))+
  directlabels::geom_dl(aes(
    FPR, TPR,
    color=model.name,
    label=paste0(model.name, ifelse(is.na(auc), "", sprintf(
      " AUC=%.3f", auc
    )))),
    method=list(
      cex=0.8,
      directlabels::polygon.method(
        "right",
        offset.cm=0.5,
        padding.cm=0.05)),
    data=pred.point.dt)+
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
    shape=21,
    fill="white",
    data=pred.point.dt)+
  theme(
    panel.spacing=grid::unit(0, "lines"),
    legend.position="none"
  )+
  facet_grid(test.fold ~ Penalty + Parameters, labeller=label_both)+
  coord_equal()+
  scale_x_continuous(
    "False Positive Rate (test set labels)",
    breaks=c(0, 0.5, 1),
    labels=c("0", "0.5", "1"))+
  scale_y_continuous(
    "True Positive Rate (test set labels)",
    breaks=c(0, 0.5, 1),
    labels=c("0", "0.5", "1"))
##print(gg)
expansion <- 2.5
pdf("figure-sequence-cv-roc.pdf", width=3*expansion, height=2*expansion)
print(gg)
dev.off()

auc.wide <- dcast(
  auc.dt,
  test.fold + Parameters + Penalty ~ model.name,
  value.var = "auc")
auc.wide[, diff := OPART-LOPART]
auc.wide

pred.point.dt[, fn := possible.fn-tp ]
pred.point.dt[, errors := fn + fp ]
pred.point.dt[, percent.error := 100*errors/labels]
pred.point.dt[, Penalty.Params := paste0(Penalty, ".", Parameters)]
gg <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  geom_point(aes(
    percent.error, Penalty.Params, color=model.name),
    data=pred.point.dt)+
  facet_grid(. ~ test.fold, labeller=label_both)+
  scale_color_manual(values=algo.colors)

pred.point.dt[, percent.accuracy := 100-percent.error]
pred.point.vars <- melt(
  pred.point.dt,
  measure.vars=c("percent.accuracy", "auc"))
ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  geom_point(aes(
    value, Penalty.Params, color=model.name),
    data=pred.point.vars)+
  facet_grid(test.fold ~ variable, labeller=label_both, scales="free")+
  scale_color_manual(values=algo.colors)

ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  geom_point(aes(
    value, Penalty.Params, color=model.name),
    data=pred.point.vars)+
  facet_grid(. ~ variable + test.fold, labeller=label_both, scales="free")+
  scale_color_manual(values=algo.colors)

pred.point.wide <- dcast(
  pred.point.dt,
  test.fold + Penalty.Params ~ model.name,
  value.var="percent.error")
pred.point.tall <- melt(
  pred.point.wide,
  measure.vars=c("OPART", "SegAnnot"),
  variable.name="competitor",
  value.name="percent.error")

ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(. ~ competitor)+
  geom_abline(aes(
    slope=slope, intercept=intercept),
    color="grey",
    data=data.table(slope=1, intercept=0))+
  geom_point(aes(
    LOPART, percent.error, color=Penalty.Params),
    data=pred.point.tall)+
  coord_equal()

pdf("figure-sequence-cv.pdf", width=4, height=2)
print(gg)
dev.off()
