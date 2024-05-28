# Tom Offrede
# Figures for Speech Prosody paper
Sys.setenv(LANG="En")

library(tidyverse)
library(broom)

load(paste0(here::here(), "/data/speechData-allIPUs.RData"))
load(paste0(here::here(), "/data/individualTemp.RData"))
load(paste0(here::here(), "/data/metadata-clean.RData"))
load(paste0(here::here(), "/data/tempDataChange.RData"))
load(paste0(here::here(), "/data/tempData.RData"))
meta <- m

folderFig <- paste0(here::here(), "/figures/SpeechProsody/")

# set theme for figures
theme_set(theme_minimal()+
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.title = element_text(size=12),
                  axis.text.x = element_text(size=10),
                  strip.text = element_text(size = 12),
                  plot.title = element_text(size=14, hjust=0.5),
                  legend.title = element_text(size=11),
                  legend.text = element_text(size=10),
                  plot.background = element_rect(colour = "black", fill=NA)
            ))

# f0 median change
ggplot(ipus |> filter(task=="Lists"), aes(turnNormalTask, f0medz))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~condition, labeller = labeller(condition = c("close" = "Personal condition", "impersonal" = "Impersonal condition")))+
  labs(x="Time (normalized)", y = "f0 median (z-scored)")+
  scale_x_continuous(breaks=c(0, 0.5, 1), labels=c("0", "0.5", "1"))
ggsave(paste0(folderFig, "f0median.png"), dpi="retina", height=1000, width=1500, unit="px")


#################################################################################

# f0 SD difference and temperature change

ts <- merge(ipus,
            datChange |> 
              select(speaker, ROI, tempChangeLists),
            by="speaker") |> 
  filter(task=="Lists") |> 
  select(speaker, task, turnNormalTask, f0sd, f0sdz, tempChangeLists, ROI) |> 
  mutate(f0sdChange = NA,
         f0sdzChange = NA)

for(s in unique(ts$speaker)){
  min <- unique(ts$f0sd[ts$speaker==s & ts$task=="Lists" & ts$turnNormalTask==min(ts$turnNormalTask[ts$speaker==s & ts$task=="Lists" & !is.na(ts$f0sd)])])
  minz <- unique(ts$f0sdz[ts$speaker==s & ts$task=="Lists" & ts$turnNormalTask==min(ts$turnNormalTask[ts$speaker==s & ts$task=="Lists" & !is.na(ts$f0sdz)])])
  max <- unique(ts$f0sd[ts$speaker==s & ts$task=="Lists" & ts$turnNormalTask==max(ts$turnNormalTask[ts$speaker==s & ts$task=="Lists" & !is.na(ts$f0sd)])])
  maxz <- unique(ts$f0sdz[ts$speaker==s & ts$task=="Lists" & ts$turnNormalTask==max(ts$turnNormalTask[ts$speaker==s & ts$task=="Lists" & !is.na(ts$f0sdz)])])
  sdChange <- max - min
  sdzChange <- maxz - minz
  ts$f0sdChange[ts$speaker==s] <- sdChange
  ts$f0sdzChange[ts$speaker==s] <- sdzChange
}

ggplot(ts |> filter(ROI%in%c("Eyes", "Forehead"), f0sdzChange > -4), aes(tempChangeLists, f0sdzChange))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~ROI)+
  labs(x="Temperature change (°C)", y = "Change in f0 SD (z-scored)")
ggsave(paste0(folderFig, "f0sd_forehead.png"), dpi="retina", height=1000, width=1500, unit="px")


#################################################################################

# nose temperature and desire to become friends
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
all$section <- factor(all$section, levels=c("Lists", "Diapix", "entireExp"))
all$effect <- relevel(all$effect, ref="ns")
all$effect <- factor(all$effect, levels=c("decrease", "ns", "increase"))
d <- merge(all |> 
             filter(section == "Lists") |>
             select(-condition) |> 
             mutate_at(c("sign", "effect"), as.factor),
           meta, by="speaker") |> 
  mutate_at("condition", as.factor)

ggplot(d |> filter(ROI=="Nose"), aes(effect, becomeFriends, color=effect))+
  geom_boxplot(width=0.15, fatten = NULL)+
  ggdist::stat_halfeye(adjust = .6,  width = .7, justification = -.2, .width = c(.5, .95))+
  scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
  scale_x_discrete(labels=c("Decrease", "Not significant", "Increase"))+
  theme(legend.position="none")+
  labs(x="Nose temperature change", y="Likelihood of friendship")+
  theme(axis.text.x = element_text(size=12, color="black"))+
  ggsignif::geom_signif(comparisons = list(c("increase", "ns")),
                        annotations = c("*"), textsize=7, color="black")+
  scale_y_continuous(limits=c(2,9.67), breaks=c(2,4,6,8,10), labels=c("2","4","6","8",""))
# ggsave(paste0(folderFig, "noseFriends.png"), dpi="retina", height=1050, width=1500, unit="px")
ggsave("C:/Users/offredet/Documents/1HU/Thesis/noseFriends2.png", dpi="retina", height=1050, width=1500, unit="px")


#############################################

########## FOR POSTER

impersonalcol <- "#3FB6ADFF"
personalcol <-  "#F46718FF"
# "C:/Users/offredet/Documents/1HU/ExperimentTemperature/SpeechProsodyPoster/"

library(tidyverse)
library(broom)
library(ggh4x)
library(ggsignif)
library(ggpubr)

load(paste0(here::here(), "/data/speechData-allIPUs.RData"))
load(paste0(here::here(), "/data/individualTemp.RData"))
load(paste0(here::here(), "/data/metadata-clean.RData"))
load(paste0(here::here(), "/data/tempDataChange.RData"))
load(paste0(here::here(), "/data/tempData.RData"))
meta <- m

# set theme for figures
theme_set(theme_minimal()+
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.title.x = element_text(size=12),
                  axis.title.y = element_text(size=14),
                  axis.text.x = element_text(size=10),
                  strip.text = element_text(size = 12),
                  plot.title = element_text(size=14, hjust=0.5),
                  legend.title = element_text(size=11),
                  legend.text = element_text(size=10),
                  plot.background = element_rect(colour = "black", fill=NA)
            ))

striptheme <- strip_themed(text_x = elem_list_text(color=c(personalcol, impersonalcol)))

# f0 median change
ggplot(ipus |> filter(task=="Lists"), aes(turnNormalTask, f0medz))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap2(~condition,
              labeller = labeller(condition = c("close" = "Personal condition", "impersonal" = "Impersonal condition")),
              strip=striptheme)+
  labs(x="Time (normalized)", y = "f0 median (z-scored)")+
  scale_x_continuous(breaks=c(0, 0.5, 1), labels=c("0", "0.5", "1"))
ggsave("C:/Users/offredet/Documents/1HU/ExperimentTemperature/SpeechProsodyPoster/f0median.png", dpi="retina", height=1000, width=1500, unit="px")


################

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
all$section <- factor(all$section, levels=c("Lists", "Diapix", "entireExp"))
all$effect <- relevel(all$effect, ref="ns")
all$effect <- factor(all$effect, levels=c("decrease", "ns", "increase"))
d <- merge(all |> 
             filter(section == "Lists") |>
             select(-condition) |> 
             mutate_at(c("sign", "effect"), as.factor),
           meta, by="speaker") |> 
  mutate_at("condition", as.factor)

theme_set(theme_minimal()+
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.title.x = element_text(size=12),
                  axis.title.y = element_text(size=16),
                  axis.text.x = element_text(size=16, color="black"),
                  strip.text = element_text(size = 12),
                  plot.title = element_text(size=14, hjust=0.5),
                  legend.title = element_text(size=11),
                  legend.text = element_text(size=10),
                  plot.background = element_rect(colour = "black", fill=NA)
            ))


ggplot(d |> filter(ROI=="Nose"), aes(effect, becomeFriends, color=effect))+
  geom_boxplot(width=0.15, fatten = NULL)+
  ggdist::stat_halfeye(adjust = .6,  width = .7, justification = -.2, .width = c(.5, .95))+
  scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
  scale_x_discrete(labels=c("Decrease", "Not signif.", "Increase"))+
  theme(legend.position="none")+
  labs(x=NULL, y="Likelihood of friendship")+
  ggsignif::geom_signif(comparisons = list(c("increase", "ns")),
                        annotations = c("*"), textsize=7, color="black")+
  scale_y_continuous(limits=c(2,9.67), breaks=c(2,4,6,8,10), labels=c("2","4","6","8",""))

ggsave("C:/Users/offredet/Documents/1HU/ExperimentTemperature/SpeechProsodyPoster/noseFriends.png", dpi="retina", height=1050, width=1500, unit="px")

theme_set(theme_minimal()+
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.title.x = element_text(size=12),
                  axis.title.y = element_text(size=12),
                  axis.text.x = ggtext::element_markdown(size=14),
                  strip.text = element_text(size = 12),
                  plot.title = element_text(size=14, hjust=0.5, face="bold"),
                  legend.title = element_text(size=11),
                  legend.text = element_text(size=10)
            ))

meta <- m |> 
  mutate(condition = ifelse(condition == "close", "Pers.", "Impers.")) |> 
  mutate(condition.label = paste0("<span style = 'color: ",
                                 ifelse(condition == "Pers.", personalcol, impersonalcol),
                                 ";'>",
                                 condition,
                                 "</span>"),
         # condition.label = fct_reorder(list("Personal", "Impersonal")),
         )
meta$condition.label <- factor(as.character(meta$condition.label), levels=c("<span style = 'color: #F46718FF;'>Pers.</span>", "<span style = 'color: #3FB6ADFF;'>Impers.</span>"))

(c <- ggplot(meta, aes(condition.label, closeness))+
    geom_boxplot(width=0.15, fatten = NULL, size=1)+
    ggdist::stat_halfeye(adjust = .8,  width = .7, justification = -.2, .width = c(.5, .95))+
    geom_signif(comparisons = list(c("<span style = 'color: #F46718FF;'>Pers.</span>",
                                     "<span style = 'color: #3FB6ADFF;'>Impers.</span>")),
                annotations = c("*"), textsize=5, color="black",
                y=8.2)+
    labs(x=NULL,
         y=NULL,
         title="Closeness")+
    scale_y_continuous(limits=c(1,9), breaks=c(2,4,6,8)))

(l <- ggplot(meta, aes(condition.label, likeability))+
    geom_boxplot(width=0.15, fatten = NULL, size=1)+
    ggdist::stat_halfeye(adjust = .8,  width = .7, justification = -.2, .width = c(.5, .95))+
    geom_signif(comparisons = list(c("<span style = 'color: #F46718FF;'>Pers.</span>",
                                     "<span style = 'color: #3FB6ADFF;'>Impers.</span>")),
                annotations = c("*"), textsize=5, color="black",
                y=9.175)+
    labs(x=NULL,
         y=NULL,
         title="Partner is Liked")+
    scale_y_continuous(limits=c(4,9.75), breaks=c(5,7,9)))

annotate_figure(ggarrange(c,l),
                bottom=text_grob("Condition", size=14),
                left=text_grob("Rating", size=14, rot=90))+
  border()

ggsave("C:/Users/offredet/Documents/1HU/ExperimentTemperature/SpeechProsodyPoster/questionnaire.png", dpi="retina", height=950, width=1500, unit="px")

