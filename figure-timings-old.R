panel.titles <- c(
  "Variable number of labels $M$\nFixed number of data $N=10^5$",
  "Variable number of data $N$\nFixed number of labels $M=5$",
  "Variable number of data $N$\nFixed number of labels $M=500$")
panel <- function(x){
  factor(x, panel.titles)
}

timing.dt <- read.csv("vignettes/TimingVsLabels.csv")
timing.dt$V1 <- panel(panel.titles[1])

lab.df <- data.frame(
  V1=panel(panel.titles[3]),
  seconds = c(1,60),
  times = c("1 second", "1 minute"))

timing.dt1 <- read.csv("vignettes/TimingList.csv")
timing.dt1$time <- timing.dt1$time / (10^(9))
timing.dt1$V1 <- panel(panel.titles[2])

timing.dt2 <- read.csv("vignettes/TimingVsSizeLabels500.csv")
timing.dt2$V1 <- panel(panel.titles[3])

comb_data <- rbind(timing.dt2, timing.dt1, timing.dt)
comb_data$expr <- as.character(comb_data$expr)
comb_data$expr <- ifelse(comb_data$expr == "labelled_opart", "LOPART", comb_data$expr)
comb_data$expr <- ifelse(comb_data$expr == "opart", "OPART", comb_data$expr)
comb_data$expr <- ifelse(comb_data$expr == "fpop", "FPOP", comb_data$expr)


algo.colors <- c(
  OPART="deepskyblue",
  LOPART="black",
  FPOP="red")

comb_dt <- data.table(comb_data)

comb_dt[, log10.size := log10(size)]
rsize <- 20
comb_dt[, log10.round := round(log10.size*rsize)/rsize]
comb_dt[, size.round := 10^log10.round]
comb_round <- comb_dt[, .(
  median=median(time),
  q25=quantile(time, 0.25),
  q75=quantile(time, 0.75)
), by=.(V1, size.round, expr)]
blank_dt <- comb_round[, {
  x <- log10(range(size.round))
  .(size=c(10^((x[2]-x[1])/2 + x[2])))
}, by=V1]
vline.dt <- rbind(
  data.table(V1=panel(panel.titles[1]), size=c(5, 500)),
  data.table(V1=panel(panel.titles[2:3]), size=1e5))
plt <- ggplot() +
  geom_hline(aes(
    yintercept=seconds),
    data=data.table(lab.df)[, .(seconds)],
    color="grey")+
  geom_text(aes(
    500, seconds, label=times),
    data=lab.df,
    size=3,
    vjust=1.25,
    color="grey50")+
  geom_vline(aes(
    xintercept=size),
    data=vline.dt,
    color="grey")+
  geom_text(aes(
    size*1.1, 1e-3, label=sprintf(
      "$%s=%s$",
      ifelse(V1==panel.titles[1], "M", "N"),
      ifelse(size==1e5, "10^5", size))),
    data=vline.dt,
    hjust=0,
    size=3,
    color="grey50")+
  scale_color_manual(values=algo.colors)+
  scale_fill_manual(values=algo.colors)+
  geom_blank(aes(
    size, 1),
    data=blank_dt)+
  geom_line(aes(
    x = size.round, y = median, col = expr),
    data = comb_round) +
  geom_ribbon(aes(
    x = size.round, ymin = q25, ymax=q75, fill = expr),
    alpha=0.5,
    data = comb_round) +
  facet_grid(. ~ V1, scales="free")+
  scale_x_log10(
    "Variable number of data/labels",
    breaks=10^seq(1, 5, by=2)
    ) +
  scale_y_log10(
    "Computation time (seconds)",
    breaks=10^seq(-4, 2))+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))
gg <- directlabels::direct.label(plt, list(cex=0.8, "last.polygons"))
w=6
h=2
tikz("figure-timings-standAlone.tex", width=w, height=h, standAlone=TRUE)
print(gg)
dev.off()
system("pdflatex figure-timings-standAlone")
tikz("figure-timings.tex", width=w, height=h)
print(gg)
dev.off()


