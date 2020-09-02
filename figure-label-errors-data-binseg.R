source("packages.R")

labeled.data <- list()
sizes.list <- list()
for(data.type in c("labels", "signals")){
  csv.gz <- sprintf("data-for-LOPART-%s.csv.gz", data.type)
  type.dt <- data.table::fread(csv.gz)
  labeled.data[[data.type]] <- type.dt
  sizes.list[[data.type]] <- type.dt[, data.table(
    data.type,
    count=.N
  ), by=sequenceID][order(count)]
}
sizes <- do.call(rbind, sizes.list)
sizes[, .(range=range(count)), by=data.type]

seq.dt <- sizes[data.type=="signals", .(sequenceID, count)]
seq.dt[, cache.csv := file.path(
  "figure-label-errors-data-binseg", paste0(sequenceID, ".csv"))]
seq.todo <- seq.dt[!file.exists(cache.csv)]
for(sequenceID.i in seq_along(seq.todo$sequenceID)){
  row.todo <- seq.todo[sequenceID.i]
  cat(sprintf("%4d / %4d %s\n", sequenceID.i, nrow(seq.todo), row.todo$sequenceID))
  data.list <- list()
  for(data.type in names(labeled.data)){
    data.list[[data.type]] <- labeled.data[[data.type]][row.todo, on="sequenceID"]
  }
  computed.err <- data.table(test.fold=unique(data.list$labels$fold))[, {
    fold.regions <- data.table(data.list$labels)
    fold.regions[, set := ifelse(fold==test.fold, "test", "train")]
    fold.regions[, annotation := ifelse(
      changes==0, "0breakpoints", "1breakpoint")]
    train.label.dt <- fold.regions[set=="train"]
    fit.dt <- binsegRcpp::binseg_normal(
      data.list[["signals"]][["logratio"]])
    data.table(penalty=10^seq(-5, 5, by=0.5))[, {
      end <- fit.dt[, end[1:which.min(loss+segments*penalty)] ]
      meta.dt <- data.table(row.todo, model.name="BinSeg", penalty)
      change.dt <- data.table(change=end[-1]+0.5)
      fold.regions[, {
        err.list <- penaltyLearning::labelError(
          models=meta.dt,
          labels=data.table(meta.dt, .SD),
          changes=data.table(meta.dt, change.dt),
          change.var = "change",
          label.vars = c("start", "end"),
          problem.vars = "sequenceID",
          model.vars = c("model.name", "penalty"))
        err.list[["model.errors"]]
      }, by=set]
    }, by=penalty]
  }, by=test.fold]
  if(FALSE){
    ggplot()+
      geom_line(aes(
        penalty, errors, color=model.name),
        data = computed.err[, .(penalty, errors, set, test.fold, model.name)])+
      theme_bw()+
      theme(panel.spacing=grid::unit(0, "lines"))+
      facet_grid(set + test.fold ~ .)+
      scale_x_log10()
  }
  computed.err[set=="test", .(
    min=min(errors),
    penalties=.N
  ), by=.(test.fold, model.name)]
  dir.create(dirname(row.todo$cache.csv), showWarnings = FALSE, recursive = TRUE)
  data.table::fwrite(computed.err, row.todo$cache.csv)
}

