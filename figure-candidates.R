source("packages.R")

unlink("figure-candidates", recursive=TRUE)
dir.create("figure-candidates")

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

up.to.dt <- rbind(
  data.table(t=2, tau=c(0:1)),
  data.table(t=3, tau=c(0:2)),
  data.table(t=25, tau=c(0, 10, 19:20, 24)),
  data.table(t=51, tau=c(0, 10, 20, 25, 29:30, 50)),
  ##data.table(t=25, tau=c(0, 5, 10, 18:24)),
  data.table(t=75, tau=c(0, 5, 10, 43:45, 51, 74)),
  data.table(t=87, tau=86),
  data.table(t=100, tau=c(0, 50, 75, 86, 99)))

penalty <- 10
run.LOPART <- function(n_updates){
  LOPART::LOPART(
    signal, label.dt, penalty, n_updates=n_updates, penalty_labeled=penalty)
}

unique.tau <- up.to.dt[0 < tau, unique(tau)]
segs.up.to <- list()
for(tau in unique.tau){
  for(model.name in names(label.list)){
    label.dt <- data.table(label.list[[model.name]])
    fit <- run.LOPART(tau)
    segs.up.to[[paste(tau, model.name)]] <- fit$segments
  }
}

frame.list <- list()
for(up.to.t in unique(up.to.dt$t)){
  fit.list <- list()
  for(model.name in names(label.list)){
    label.dt <- data.table(label.list[[model.name]])
    fit.list[[model.name]] <- run.LOPART(up.to.t)
  }
  tau.dt <- up.to.dt[t==up.to.t]
  for(tau in tau.dt$tau){
    seg.dt.list <- list()
    cost.dt.list <- list()
    for(model.name in names(fit.list)){
      fit <- fit.list[[model.name]]
      prev.segs <- segs.up.to[[paste(tau, model.name)]]
      start <- tau+1
      last.seg <- data.table(
        start, end=up.to.t, mean=mean(signal[start:up.to.t]))
      Algorithm <- factor(model.name, names(label.list))
      this.cost.dt <- with(fit$cost, data.table(
        cost_candidates,
        tau=seq_along(cost_candidates)-1,
        change=seq_along(cost_candidates)-0.5
      ))[cost_candidates < Inf]
      if(tau %in% this.cost.dt$tau){
        seg.dt.list[[model.name]] <- data.table(
          Algorithm, rbind(prev.segs, last.seg))
      }
      if(nrow(this.cost.dt)){
        cost.dt.list[[model.name]] <- data.table(Algorithm, this.cost.dt)
      }
    }#model.name
    seg.dt <- do.call(rbind, seg.dt.list)[, .(Algorithm, mean, start, end)]
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
    sig.color <- "grey50"
    change.dt <- data.table(tau, change=tau+0.5)
    t.dt <- data.table(up.to.t)
    my.hjust <- function(x)ifelse(x < length(signal)/2, 0, 1)
    min.dt <- cost.dt[, {
      .SD[which.min(cost_candidates)]
    }, by=Algorithm][order(tau)]
    min.dt[, hjust := c(1, 0)[1:.N] ]
    min.dt[, y := cost_candidates ]
    min.text.dt <- if(up.to.t < 10)min.dt[-1] else min.dt
    min.dl.dt <- data.table(min.dt)
    min.dl.dt[, hjust := 0.5 ]
    cost.range <- range(cost.dt$cost_candidates)
    cost.h <- cost.range[2]-cost.range[1]
    blank.dt <- data.table(
      position=1, cost=cost.range[1]-cost.h/4)
    data.col <- "top"
    space <- 0.1
    my.method <- list(
      vjust=1,
      gapply.fun(d[which.min(d$y), ]),
      "calc.boxes",
      function(d, ...) {
        for(xy in c("x", "y")) {
          d[[sprintf("%s.%s", data.col, xy)]] <- d[[xy]]
        }
        d$y <- min(d$y)-space
        d
      },
      function(d, ...) {
        d$w <- d$w + space
        d
      },
      "calc.borders",
      qp.labels("x", "left", "right", limits=xlimits),
      "calc.borders",
      function(d,...){
        for(side in c("left", "right", "top", "bottom")){
          for(xy in c("x", "y")){
            col.name <- paste0(side, ".", xy)
            if(!col.name %in% names(d)){
              d[[col.name]] <- NA
            }
          }
        }
        if(! "box.color" %in% names(d)){
          d$box.color <- "black"
        }
        for(i in 1:nrow(d))with(d[i,], {
          L <- list(
            x=c(left.x, left, top.x, right, right.x, right, bottom.x, left),
            y=c(left.y, top, top.y, top, right.y, bottom, bottom.y, bottom))
          for(xy.name in names(L)){
            xy <- L[[xy.name]]
            L[[xy.name]] <- xy[!is.na(xy)]
          }
          grid::grid.polygon(
            L$x, L$y,
            default.units="cm",
            gp=grid::gpar(col=box.color, fill=colour)
          )
        })
        d$colour <- "white"
        d
      })
    gg <- ggplot()+
      geom_blank(aes(
        position, cost),
        data=COST(blank.dt))+
      geom_vline(aes(
        xintercept=up.to.t),
        color=sig.color,
        data=t.dt)+
      geom_text(aes(
        up.to.t, 13,
        hjust=my.hjust(up.to.t),
        label=sprintf(
          "$t=%s$", up.to.t)),
        color=sig.color,
        data=DATA(t.dt))+
      geom_rect(aes(
        xmin=start, xmax=end,
        fill=paste(changes),
        ymin=-Inf, ymax=Inf),
        alpha=0.5,
        data=labels)+
      scale_fill_manual("label", values=label.colors)+
      theme_bw()+
      theme(panel.spacing=grid::unit(0, "lines"))+
      facet_grid(y.var ~ ., scales="free")+
      geom_text(aes(
        change, 1,
        hjust=my.hjust(change),
        label=sprintf(
          "$\\tau = %d$", tau)),
        vjust=0,
        data=DATA(change.dt))+
      geom_vline(aes(
        xintercept=change),
        data=change.dt)+
      geom_segment(aes(
        start-0.5, mean,
        size=Algorithm,
        color=Algorithm,
        xend=end+0.5, yend=mean),
        data=DATA(seg.dt))+
      geom_point(aes(
        position, signal),
        color=sig.color,
        shape=1,
        data=DATA(signal.dt))+
      scale_size_manual(values=c(
        OPART=1.5,
        LOPART=0.5),
        drop=FALSE)+
      scale_shape_manual(values=c(
        OPART=1,
        LOPART=2),
        drop=FALSE)+
      scale_color_manual(values=c(
        OPART="deepskyblue",
        LOPART="black"),
        drop=FALSE)+
      ylab("")+
      scale_x_continuous(
        "position $t,\\tau$",
        breaks=seq(0, 100, by=10))+
      geom_dl(aes(
        change, y,
        hjust=hjust,
        color=Algorithm,
        group=Algorithm,
        label=sprintf("$\\tau^*_{%d} = %d$", up.to.t, tau)),
        method="my.method",
        data=COST(min.dl.dt))+
      geom_point(aes(
        change, cost_candidates,
        color=Algorithm, shape=Algorithm),
        data=COST(cost.dt))+
      geom_point(aes(
        change, y,
        color=Algorithm),
        data=COST(min.dt))
    ##print(gg)
    
    this.tex <- sprintf(
      "figure-candidates/t%d-tau%d.tex",
      up.to.t, tau)
    tikz(this.tex, width=5.4, height=2.5)
    print(gg)
    dev.off()
    frame.list[[paste(up.to.t, tau)]] <- sprintf("\\begin{frame}
  \\frametitle{Demonstration of LOPART with penalty $\\lambda=%.0f$}
  \\Wider{\\input{%s}}
$$
W_t =
\\min_{
  \\tau \\in T_t
}
\\underbrace{
  W_{\\tau}
}_{
  \\text{optimal cost up to $\\tau$}
}
+
\\underbrace{
  \\lambda
}_{
  \\text{penalty}
}
+
\\underbrace{
  L(\\tau + 1, t, \\mathbf x)
}_{
  \\text{optimal cost of last segment}
}
$$
\\end{frame}
", penalty, this.tex)
  }#tau
}#t
cat(paste(frame.list, collapse="\n"), file="figure-candidates.tex")
if(FALSE){
  system("pdflatex slides")
}
