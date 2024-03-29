source("packages.R")

common.names <- c(
  "test.fold", "penalty", "set", "sequenceID", "count", "cache.csv", 
  "model.name", "penalty", "possible.fp", "fp", "possible.fn", 
  "fn", "labels", "errors")
err.dt <- data.table(
  csv=Sys.glob("figure-label-errors-data*/*.csv")
)[, {
  name.vec <- names(data.table::fread(csv, nrow=0))
  seq.i <- which(name.vec=="sequenceID")
  data.table::fread(
    csv,
    colClasses=list(character=seq.i)
  )[, common.names, with=FALSE]
}, by=csv]
err.dt[model.name=="LOPART" & set=="train", table(errors)]
err.dt[model.name=="LOPART" & set=="train" & 0<errors, .(
  csv, test.fold, set, penalty, fp, fn)]

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

feature.dt <- data.table::fread(
  "data-for-LOPART-signals.csv.gz"
)[, .(
  log.log.data=log(log(.N))
), by=sequenceID]
feature.mat <- feature.dt[, matrix(
  log.log.data,
  ncol=1,
  dimnames=list(sequenceID=sequenceID, feature="log.log.data"))]

err.train <- err.dt[set=="train" & model.name %in% c("OPART", "BinSeg")]
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
  lm.keep <- rowSums(is.finite(target.mat))==2
  lm.label <- rowMeans(target.mat[lm.keep,])
  lm.dt <- data.table(log.log.data=feature.mat[lm.keep,], log.penalty=lm.label)
  lm.fit <- lm(log.penalty ~ log.log.data, lm.dt)
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
      Penalty="lm",
      Parameters=2,
      pred.log.lambda=predict(lm.fit, data.table(feature.mat))),
    data.table(
      sequenceID=rownames(feature.mat),
      Penalty="linear",
      Parameters=2,
      pred.log.lambda=as.numeric(fit$predict(feature.mat))))
}, by=.(model.name, test.fold)]
table(pred.dt$model.name)
table(err.test$model.name)
pred.with.err.dt <- pred.dt[, .(
  sequenceID, test.fold, train.model=model.name,
  Penalty, Parameters, pred.log.lambda
)][
  err.test[, .(
    sequenceID, test.fold, train.model=ifelse(model.name=="BinSeg", "BinSeg", "OPART"),
    model.name,
    min.log.lambda, max.log.lambda, possible.fp, possible.fn, fp, fn, labels, errors
  )],
  .(sequenceID, test.fold, model.name, Penalty, Parameters, pred.log.lambda, possible.fp, possible.fn, fp, fn, labels, errors),
  on=.(sequenceID, test.fold, train.model, pred.log.lambda>min.log.lambda, pred.log.lambda<max.log.lambda),
  nomatch=0L
]
table(pred.with.err.dt$model.name)
data.table::fwrite(pred.with.err.dt, "figure-cv-BIC-pred.csv")

auc.dt <- err.test[, {
  select.dt <- data.table(
    test.fold,
    model.name=if(model.name=="BinSeg")"BinSeg" else "OPART")
  pred.fold <- pred.dt[select.dt, on=names(select.dt)]
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
(pred.err.sorted <- pred.with.err.dt[, .(
  labels=sum(labels),
  errors=sum(errors)
), keyby=.(test.fold, model.name, Penalty)])
(auc.err.sorted <- setkey(auc.dt[
, .(test.fold, model.name, Penalty, labels, errors)
], test.fold, model.name, Penalty))
all.equal(pred.err.sorted, auc.err.sorted)

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
  SegAnnot="#22CC22",
  OPART="#0077CC",
  LOPART="black")
algo.colors <- c(
  SegAnnot="blue",
  BinSeg="#ECAE5E",
  OPART="deepskyblue",
  LOPART="black",
  FPOP="red")
gg <- ggplot()+
  theme_bw()+
  scale_color_manual(values=algo.colors)+
  scale_size_manual(values=c(
    BinSeg=1.25,
    LOPART=1.5,
    OPART=1))+
  directlabels::geom_dl(aes(
    FPR, TPR,
    color=model.name,
    label=paste0(model.name, ifelse(is.na(auc), "", sprintf(
      " AUC=%.3f", auc
    )))),
    method=list(
      cex=0.75,
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
    alpha=0.7,
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
    limits=c(0, 1.2),
    labels=c("0", "0.5", "1"))+
  scale_y_continuous(
    "True Positive Rate (test set labels)",
    breaks=c(0, 0.5, 1),
    labels=c("0", "0.5", "1"))
##print(gg)
data.table::fwrite(roc.dt, "figure-cv-BIC-roc.csv")
expansion <- 2
pdf("figure-cv-BIC-roc.pdf", width=3*expansion, height=2*expansion)
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
pred.point.dt[, percent.accuracy := 100-percent.error]
pred.point.vars <- melt(
  pred.point.dt,
  measure.vars=c("percent.accuracy", "auc"))
pred.point.diff <- dcast(
  pred.point.vars,
  test.fold + Penalty.Params + variable ~ model.name,
  value.var="value")
pred.point.compare <- melt(
  pred.point.diff,
  measure.vars=c("OPART", "BinSeg"),
  variable.name="baseline")
pred.point.compare[, improvement := LOPART - value]
pred.point.compare[, .(
  min=min(improvement),
  max=max(improvement)
), by=.(baseline, variable)]
pred.point.diff[, OPART.diff := LOPART - OPART ]
pred.point.diff[, BinSeg.diff := LOPART - BinSeg ]
pred.point.diff[order(variable, Penalty.Params, test.fold), .(
  variable, test.fold, Penalty.Params, OPART.diff, BinSeg.diff)]
pred.point.diff[, SegAnnot.diff := LOPART - SegAnnot ]
pred.point.diff[
  variable=="percent.accuracy"
][order(test.fold, Penalty.Params), .(
  test.fold, Penalty.Params, SegAnnot.diff)]
gg.vars <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  geom_point(aes(
    value, Penalty.Params, color=model.name),
    data=pred.point.vars)+
  facet_grid(test.fold ~ variable, labeller=label_both, scales="free")+
  scale_color_manual(values=algo.colors)
gg.vars.wide <- ggplot()+
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
gg.comp <- ggplot()+
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
data.table::fwrite(pred.point.dt, "figure-cv-BIC-error-rates.csv")
gg <- ggplot()+
  theme_bw()+
  theme(
    legend.position="bottom",
    panel.spacing=grid::unit(0, "lines"))+
  geom_point(aes(
    percent.accuracy, Penalty.Params, color=model.name),
    shape=18,
    size=4,
    data=pred.point.dt)+
  facet_grid(. ~ test.fold, labeller=label_both)+
  scale_color_manual(
    "Algorithm",
    values=algo.colors,
    breaks=names(algo.colors)
    )+
  scale_x_continuous(
    "Test accuracy (percent correctly predicted labels)",
    ##limits=c(15, 85),
    breaks=seq(20, 80, by=20))
pdf("figure-cv-BIC.pdf", width=6, height=1.8)
print(gg)
dev.off()
