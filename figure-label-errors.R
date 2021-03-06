source("packages.R")

algo.colors <- c(
  OPART="#0077CC",
  LOPART="black",
  SegAnnot="#22CC22")
algo.colors <- c(
  OPART="deepskyblue",
  LOPART="black",
  BinSeg="#ECAE5E",
  SegAnnot="blue")
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
scale.for <- function(algo){
  scale_fill_gradient(
    "log10(seqs)",
    low="white",
    high=algo.colors[[algo]])
}  
gg <- ggplot()+
  ##ggtitle("LOPART is more accurate than SegAnnot")+
  ggtitle("Best case comparison with SegAnnot")+
  ##my.title+
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
  ##scale.fill+
  scale.for("SegAnnot")+
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
pdf("figure-label-errors-SegAnnot.pdf", width=5, height=2.3)
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
  is.pos <- errors[[col.name]] > 0
  is.neg <- errors[[col.name]] < 0
  pos <- errors[is.pos]
  neg <- errors[is.neg]
  sum.wide <- data.table(
    sum.count=sum(errors$count),
    zero.count=errors$count[is.zero],
    pos.count=sum(pos[["count"]]),
    pos.min=min(pos[[col.name]]),
    pos.max=max(pos[[col.name]]),
    neg.count=sum(neg[["count"]]),
    neg.min=min(neg[[col.name]]),
    neg.max=max(neg[[col.name]]))
  sum.tall <- melt(sum.wide, measure.vars=names(sum.wide))
  sum.tall[grepl("count", variable), percent := 100*value/nrow(dt) ]
  list(
    errors=errors,
    summary=sum.tall)
}
mytab(total.min.wide, "train_OPART")

total.min.wide[, test.diff_BinSeg := test_BinSeg-test_LOPART]
mytab(total.min.wide, "test.diff_BinSeg")
mytab(total.min.wide, "train_BinSeg")
train.test.BinSeg <- total.min.wide[, .(
  splits=.N
), by=.(train_BinSeg, test.diff_BinSeg)]
gg <- ggplot()+
  ggtitle("Best case comparison
with BinSeg")+
  geom_hline(yintercept=0, color="grey")+
  geom_vline(xintercept=0, color="grey")+
  geom_tile(aes(
    train_BinSeg, test.diff_BinSeg, fill=log10(splits)),
    alpha=0.8,
    data=train.test.BinSeg)+
  geom_text(aes(
    train_BinSeg, test.diff_BinSeg, label=splits),
    data=train.test.BinSeg)+
  scale.for("BinSeg")+
  coord_equal()+
  theme_bw()+
  scale_x_continuous(
    "BinSeg train label errors
(LOPART is always=0)")+
  scale_y_continuous(
    "Test label error difference
(BinSeg-LOPART)")
pdf("figure-label-errors-BinSeg.pdf", width=3, height=2.3)
print(gg)
dev.off()

total.min.wide[, test.diff_OPART := test_OPART-test_LOPART]
mytab(total.min.wide, "test.diff_OPART")
train.test.counts <- total.min.wide[, .(
  splits=.N
), by=.(train_OPART, test.diff_OPART)]
gg <- ggplot()+
  ##ggtitle("LOPART is more accurate\nthan OPART")+
  ##my.title+
  ggtitle("Best case comparison
with OPART")+
  geom_hline(yintercept=0, color="grey")+
  geom_vline(xintercept=0, color="grey")+
  geom_tile(aes(
    train_OPART, test.diff_OPART, fill=log10(splits)),
    alpha=0.8,
    data=train.test.counts)+
  geom_text(aes(
    train_OPART, test.diff_OPART, label=splits),
    data=train.test.counts)+
  ##scale.fill+
  scale.for("OPART")+
  coord_equal()+
  theme_bw()+
  scale_x_continuous(
    "OPART train label errors
(LOPART is always=0)")+
  scale_y_continuous(
    "Test label error difference
(OPART-LOPART)")
pdf("figure-label-errors.pdf", width=3, height=2.3)
print(gg)
dev.off()
