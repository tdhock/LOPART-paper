source("packages.R")

## Read clinical data for six patients (relapse or ok, five years
## after treatment).
clinical.limited <- read.csv("figure-neuroblastoma-clinical.csv")
ids.str <- paste(clinical.limited$profile.id)
relapse.profile <- with(clinical.limited, paste(relapse, profile.id))
names(relapse.profile) <- ids.str

## Consider the subset of profiles and labels for these six patients.
someProfiles <- function(all.profiles){
  some <- subset(
    all.profiles,
    profile.id %in% ids.str & chromosome != "Y")
  some$relapse.profile <- relapse.profile[paste(some$profile.id)]
  some
}
data(neuroblastoma)
profiles <- someProfiles(neuroblastoma$profiles)
labels <- someProfiles(neuroblastoma$annotations)

## Plot noisy data sets.
gg <- ggplot()+
  theme(
    panel.border=element_rect(fill=NA, color="grey50"),
    panel.spacing=grid::unit(0, "lines")
  )+
  facet_grid(relapse.profile ~ chromosome, scales="free", space="free_x")+
  geom_point(aes(
    position/1e6, logratio),
    data=profiles,
    shape=1)+
  scale_x_continuous(
    "position on chromosome (mega bases)",
    breaks=c(100, 200))
png(
  "figure-neuroblastoma-clinical.png",
  width=10, height=6, units="in", res=100)
print(gg)
dev.off()
