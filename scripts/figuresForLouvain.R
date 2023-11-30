# Tom Offrede
# figures for Louvain presentation

library(tidyverse)
load(paste0(here::here(), "/data/speechData.RData"))
load(paste0(here::here(), "/data/speechData-allIPUs.RData"))
load(paste0(here::here(), "/data/metadata-clean.RData"))
load(paste0(here::here(), "/data/tempDataChange.RData"))
meta <- m

# set theme
theme_set(theme_minimal()+
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.title = element_text(size=16),
                  strip.text = element_text(size = 16),
                  plot.title = element_text(size=20, hjust=0.5)))

# prepare dataset for f0 adaptation figures
ipus <- merge(ipus, 
              read.csv("C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/dominoTiming.csv", sep=";"),
              by = "dyad", all=TRUE) |> 
  mutate_at(c("dominoOnset", "dominoOffset"), as.numeric) |>  
  mutate(delete = ifelse(
    condition == "impersonal" & grepl("L3", file) & turnOnset > dominoOnset & turnOffset < dominoOffset,
    "del",
    "keep"
  )) |>
  filter(delete == "keep") |> 
  select(-delete) |> 
  group_by(speaker) |> 
  mutate(f0meanz = (f0mean - mean(f0mean, na.rm=TRUE)) / sd(f0mean, na.rm=TRUE),
         f0medz = (f0med - mean(f0med, na.rm=TRUE)) / sd(f0med, na.rm=TRUE),
         f0sdz = (f0sd - mean(f0sd, na.rm=TRUE)) / sd(f0sd, na.rm=TRUE),
         f0maxz = (f0max - mean(f0max, na.rm=TRUE)) / sd(f0max, na.rm=TRUE)) |> 
  ungroup()
ipus <- merge(ipus,
              datChange |> 
                select(speaker, ROI, tempChangeLists, tempChangeDiapix, tempChangeEntireExp),
              by="speaker")

# f0 median change - conditions
ggplot(ipus |> filter(task=="Lists"), aes(turnOverall, f0medz))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~condition, labeller = labeller(condition = c("close" = "Close condition", "impersonal" = "Impersonal condition")))+
  labs(y = "f0 median (z-scored)", x = "Time", title = "f0 median over time")
ggsave(paste0(here::here(), "/figures/Louvain/medianCondition.png"), dpi = "retina")

# f0 SD change - conditions
ggplot(ipus |> filter(task=="Lists"), aes(turnOverall, f0sdz))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~condition, labeller = labeller(condition = c("close" = "Close condition", "impersonal" = "Impersonal condition")))+
  labs(y = "f0 SD (z-scored)", x = "Time", title = "f0 SD over time")
ggsave(paste0(here::here(), "/figures/Louvain/sdCondition.png"), dpi = "retina")

