# Tom Offrede
# figures for Louvain presentation

library(tidyverse)
library(viridis)
load(paste0(here::here(), "/data/speechData.RData"))
load(paste0(here::here(), "/data/metadata-clean.RData"))
load(paste0(here::here(), "/data/tempDataChange.RData"))
meta <- m

# set theme
theme_set(theme_minimal()+
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.title = element_text(size=16),
                  axis.text.x = element_text(size=12),
                  strip.text = element_text(size = 16),
                  plot.title = element_text(size=20, hjust=0.5),
                  legend.title = element_text(size=16),
                  legend.text = element_text(size=12)))

# prepare dataset for f0 adaptation figures
load(paste0(here::here(), "/data/speechData-allIPUs.RData"))
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

# examples of simple temperature change
load(paste0(here::here(), "/data/tempData.RData"))
## Eyes: KDA
ggplot(dat |> filter(dyad == "KDA", ROI=="Eyes"), aes(time, temperature, color=speaker))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title = "Dyad 1: Eyes", y = "Temperature (°C)", x="Time", color="Speaker")+
  scale_color_viridis_d(labels = c("A", "B"))
ggsave(paste0(here::here(), "/figures/Louvain/dyad1-Eyes.png"), dpi = "retina")

ggplot(dat |> filter(dyad == "TTY", ROI=="Forehead"), aes(time, temperature, color=speaker))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title = "Dyad 2: Forehead", y = "Temperature (°C)", x="Time", color="Speaker")+
  scale_color_viridis_d(labels = c("A", "B"))
ggsave(paste0(here::here(), "/figures/Louvain/dyad2-Forehead.png"), dpi = "retina")

ggplot(dat |> filter(dyad == "SUK", ROI=="Nose"), aes(time, temperature, color=speaker))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title = "Dyad 3: Nose", y = "Temperature (°C)", x="Time", color="Speaker")+
  scale_color_viridis_d(labels = c("A", "B"))
ggsave(paste0(here::here(), "/figures/Louvain/dyad3-Nose.png"), dpi = "retina")

ggplot(dat |> filter(dyad == "MJG", ROI=="Cheeks"), aes(time, temperature, color=speaker))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title = "Dyad 4: Cheeks", y = "Temperature (°C)", x="Time", color="Speaker")+
  scale_color_viridis_d(labels = c("A", "B"))
ggsave(paste0(here::here(), "/figures/Louvain/dyad4-Cheeks.png"), dpi = "retina")

ggplot(dat |> filter(dyad == "SGB", ROI=="Nose"), aes(time, temperature, color=speaker))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title = "Dyad 5: Nose", y = "Temperature (°C)", x="Time", color="Speaker")+
  scale_color_viridis_d(labels = c("A", "B"))
ggsave(paste0(here::here(), "/figures/Louvain/dyad5-Nose.png"), dpi = "retina")

# prepare dataset for categorical temperature results
load(paste0(here::here(), "/data/tempData.RData"))
all <- data.frame(matrix(nrow=0, ncol=7))
names(all) <- c("speaker", "condition", "ROI", "direction", "t", "p", "section")
for(s in unique(dat$speaker)){
  for(r in unique(dat$ROI)){
    d <- dat |> filter(speaker==s, ROI==r)
    if(nrow(d)<=1){next}
    c <- tidy(lm(temperature ~ time, d))
    all[nrow(all)+1,] <- c(s, unique(d$condition), r,
                           ifelse(c$estimate[2] < 0, "negative", "positive"),
                           c$statistic[2],
                           c$p.value[2],
                           "entireExp")
    for(section in c("Lists", "Diapix")){
      d <- dat |> filter(speaker==s, ROI==r, task==section)
      if(nrow(d)==0){next}
      c <- tidy(lm(temperature ~ time, d))
      all[nrow(all)+1,] <- c(s, unique(d$condition), r,
                             ifelse(c$estimate[2] < 0, "negative", "positive"),
                             c$statistic[2],
                             c$p.value[2],
                             section)
    }
  }
}
all <- all |> 
  mutate_at(c("t", "p"), as.numeric) |> 
  mutate(sign = ifelse(p < 0.05, "yes", "ns"),
         effect = case_when(
           sign == "yes" & direction == "positive" ~ "increase",
           sign == "yes" & direction == "negative" ~ "decrease",
           sign == "ns" ~ "ns",
         )) |> 
  mutate_at(c("direction", "speaker", "effect", "condition", "section", "ROI"), as.factor)
all$effect <- factor(all$effect, levels=c("ns", "decrease", "increase"))

# for lists section
d <- merge(all |> 
             filter(section == "Lists") |>
             select(-condition) |> 
             mutate_at(c("sign", "effect"), as.factor),
           meta, by="speaker") |> 
  mutate_at("condition", as.factor)

# Lists - forehead - condition no effect
ggplot(d |> filter(ROI=="Forehead"), aes(condition, fill=effect))+
  geom_bar()+
  scale_fill_manual(values = c("decrease" = "lightblue", "increase" = "red", "ns" = "gray"))+
  labs(x = "Condition", y="", fill="Temp. change", title="Forehead")
ggsave(paste0(here::here(), "/figures/Louvain/foreheadListsCondition.png"), dpi = "retina", width=1700, height=1700, unit="px")

# Lists - nose - becomeFriends
ggplot(d |> filter(ROI=="Nose"), aes(effect, becomeFriends, color=effect))+
  geom_boxplot(size=1)+
  labs(x = "Temperature change", y="Desire to be friends", title="Nose")+
  scale_color_manual(values = c("decrease" = "lightblue", "increase" = "red", "ns" = "gray"))+
  theme(legend.position="none")
ggsave(paste0(here::here(), "/figures/Louvain/noseListsFriends.png"), dpi = "retina", width=1700, height=1700, unit="px")


# for diapix section
d <- merge(all |> 
             filter(section == "Diapix") |>
             select(-condition) |> 
             mutate_at(c("sign", "effect"), as.factor),
           meta, by="speaker") |> 
  mutate_at("condition", as.factor)

# Diapix - eyes - closeness
ggplot(d |> filter(ROI=="Eyes"), aes(effect, closeness, color=effect))+
  geom_boxplot(size=1)+
  labs(x = "Temperature change", y="Closeness", title="Eyes")+
  scale_color_manual(values = c("decrease" = "lightblue", "increase" = "red", "ns" = "gray"))+
  theme(legend.position="none")
ggsave(paste0(here::here(), "/figures/Louvain/eyesDiapixCloseness.png"), dpi = "retina", width=1700, height=1700, unit="px")
  
# Diapix - forehead - likeability
ggplot(d |> filter(ROI=="Forehead"), aes(effect, likeability, color=effect))+
  geom_boxplot(size=1)+
  labs(x = "Temperature change", y="Likes their partner", title="Forehead")+
  scale_color_manual(values = c("decrease" = "lightblue", "increase" = "red", "ns" = "gray"))+
  theme(legend.position="none")
ggsave(paste0(here::here(), "/figures/Louvain/foreheadDiapixLikeability.png"), dpi = "retina", width=1700, height=1700, unit="px")

# Diapix - forehead - condition
ggplot(d |> filter(ROI=="Forehead"), aes(condition, fill=effect))+
  geom_bar()+
  scale_fill_manual(values = c("decrease" = "lightblue", "increase" = "red", "ns" = "gray"))+
  labs(x = "Condition", y="", fill="Temp. change", title="Forehead")
ggsave(paste0(here::here(), "/figures/Louvain/foreheadDiapixCondition.png"), dpi = "retina", width=1700, height=1700, unit="px")

# Diapix - cheeks - condition
ggplot(d |> filter(ROI=="Cheeks"), aes(condition, fill=effect))+
  geom_bar()+
  scale_fill_manual(values = c("decrease" = "lightblue", "increase" = "red", "ns" = "gray"))+
  labs(x = "Condition", y="", fill="Temp. change", title="Cheeks")
ggsave(paste0(here::here(), "/figures/Louvain/cheeksDiapixCondition.png"), dpi = "retina", width=1700, height=1700, unit="px")

# thermal comfort
ggplot(meta, aes(condition, comfortPre))+
  geom_boxplot(size=1)+
  labs(x = "Condition", y="Thermal Comfort", title="Before Interaction")
ggsave(paste0(here::here(), "/figures/Louvain/thermalPre.png"), dpi = "retina", width=1700, height=1700, unit="px")

ggplot(meta, aes(condition, comfortPost))+
  geom_boxplot(size=1)+
  labs(x = "Condition", y="Thermal Comfort", title="After Interaction")
ggsave(paste0(here::here(), "/figures/Louvain/thermalPost.png"), dpi = "retina", width=1700, height=1700, unit="px")

ggplot(meta, aes(condition, comfortPost, color=condition))+
  geom_boxplot(size=1)+
  labs(x = "Condition", y="Thermal Comfort", title="After Interaction")+
  scale_color_manual(values = c("impersonal" = "lightblue", "close" = "red"))+
  theme(legend.position="none")
ggsave(paste0(here::here(), "/figures/Louvain/thermalPostColor.png"), dpi = "retina", width=1700, height=1700, unit="px")









