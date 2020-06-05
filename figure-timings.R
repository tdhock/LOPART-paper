source("packages.R")

timings.dt <- data.table::fread("figure-timings-data-labels.csv")
timings.dt[, seconds := time/1e9]
timing.stats <- timings.dt[, .(
  min=min(seconds),
  max=max(seconds),
  median=median(seconds)
), by=.(Algorithm=expr, size)]
ref.dt <- data.table(seconds=1, label="1 second")
algo.colors <- c(
  OPART="deepskyblue",
  LOPART="black",
  FPOP="red")
gg <- ggplot()+
  ggtitle(
    "$N=10,000$ data")+
  geom_hline(aes(
    yintercept=seconds),
    data=ref.dt,
    color="grey")+
  geom_text(aes(
    10, seconds, label=label),
    size=3,
    data=ref.dt,
    vjust=1.5,
    color="grey50")+
  geom_ribbon(aes(
    size, ymin=min, ymax=max, fill=Algorithm),
    data=timing.stats,
    alpha=0.5)+
  scale_fill_manual(values=algo.colors)+
  scale_color_manual(values=algo.colors)+
  geom_line(aes(
    size, median, color=Algorithm),
    data=timing.stats)+
  scale_x_log10(
    "Number of labels $M$",
    limits=c(1, 8000),
    breaks=10^seq(0, 3))+
  scale_y_log10()+
  theme_bw()
dl <- directlabels::direct.label(gg, list(cex=0.7, "last.polygons"))
tikz("figure-timings-labels.tex", width=2.5, height=3)
print(dl)
dev.off()

timings.dt <- data.table::fread("figure-timings-data.csv")
timings.dt[, seconds := time/1e9]
timing.stats <- timings.dt[, .(
  min=min(seconds),
  max=max(seconds),
  median=median(seconds)
), by=.(Algorithm=expr, size, data.over.labels, label.density)]
ref.dt <- data.table(
  seconds=1, label="1 second",
  label.density=timing.stats$label.density[1])
algo.colors <- c(
  OPART="deepskyblue",
  LOPART="black",
  FPOP="red")
gg <- ggplot()+
  ggtitle("LOPART is $O(N\\log N)$ time
with $O(N)$ positive labels")+
  theme_bw()+
  theme(
    legend.position="none",
    panel.spacing=grid::unit(0, "lines"))+
  facet_grid(. ~ label.density, labeller=label_both)+
  geom_hline(aes(
    yintercept=seconds),
    data=ref.dt[, .(seconds)],
    color="grey")+
  geom_text(aes(
    100, seconds, label=label),
    data=ref.dt,
    size=3,
    vjust=-0.5,
    color="grey50")+
  geom_ribbon(aes(
    size, ymin=min, ymax=max, fill=Algorithm),
    data=timing.stats,
    alpha=0.5)+
  scale_fill_manual(values=algo.colors)+
  scale_color_manual(values=algo.colors)+
  geom_line(aes(
    size, median, color=Algorithm),
    data=timing.stats)+
  scale_x_log10(
    "Number of data $N$",
    limits=10^c(0.5, 5.5),
    breaks=10^seq(1, 5, by=2))+
  scale_y_log10()
tikz("figure-timings.tex", width=3.3, height=3)
print(gg)
dev.off()

