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
gg <- ggplot()+
  ggtitle("LOPART is more accurate than SegAnnot")+
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
  scale_fill_gradient(low="white", high="red")+
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

total.min.wide[, test.diff := test_OPART-test_LOPART]
train.test.counts <- total.min.wide[, .(
  splits=.N
), by=.(train_OPART, test.diff)]
gg <- ggplot()+
  ggtitle("LOPART is more accurate
than OPART")+
  geom_hline(yintercept=0, color="grey")+
  geom_vline(xintercept=0, color="grey")+
  geom_tile(aes(
    train_OPART, test.diff, fill=log10(splits)),
    alpha=0.8,
    data=train.test.counts)+
  geom_text(aes(
    train_OPART, test.diff, label=splits),
    data=train.test.counts)+
  scale_fill_gradient(low="white", high="violet")+
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