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

err.train <- err.dt[model.name=="OPART" & set=="train", .(
  train.errors=sum(errors)
), by=.(test.fold, penalty)]
best.penalty <- err.train[, .SD[which.min(train.errors)], by=test.fold]
err.test <- err.dt[set=="test"]
err.pred <- err.test[best.penalty, on=.(test.fold, penalty)]

(fold.model.err.tall <- err.pred[, .(
  test.errors=sum(errors),
  test.labels=sum(labels)
), by=.(test.fold, model.name)])
fold.model.err.tall[, prop.errors := test.errors / test.labels]
fold.model.err.wide <- dcast(
  fold.model.err.tall,
  test.fold ~ model.name,
  value.var = "prop.errors")
fold.model.err.wide[, diff := OPART-LOPART]
fold.model.err.wide

prob.err.wide <- dcast(
  err.pred,
  test.fold + sequenceID ~ model.name,
  value.var = "errors")
prob.err.wide[, diff := OPART-LOPART]
prob.err.wide.counts <- prob.err.wide[, .(
  count=.N
), keyby=.(test.fold, diff)]

gg <- ggplot()+
  geom_tile(aes(
    diff, factor(test.fold),
    fill=log10(count)),
    data=prob.err.wide.counts)+
  coord_equal()+
  scale_fill_gradient(
    "log10(sequences)",
    low="white",
    high="orange")+
  theme_bw()+ 
  geom_text(aes(
    diff, factor(test.fold),
    label=count),
    data=prob.err.wide.counts)+
  ylab("Test fold")+
  xlab("Difference of incorrectly predicted
labels in test set (OPART-LOPART)")
pdf("figure-cv.pdf", width=4, height=2)
print(gg)
dev.off()
