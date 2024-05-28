# Tom Offrede
# figures for defense

Sys.setenv(LANG="En")

library(tidyverse)
library(broom)

load(paste0(here::here(), "/data/speechData-allIPUs.RData"))
# load(paste0(here::here(), "/data/individualTemp.RData"))
load(paste0(here::here(), "/data/metadata-clean.RData"))
load(paste0(here::here(), "/data/tempDataChange.RData"))
load(paste0(here::here(), "/data/tempData.RData"))
meta <- m

impersonalcol <- "#3FB6ADFF"
personalcol <-  "#F46718FF"

theme_set(theme_minimal()+
            theme(axis.ticks.y=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title = element_text(size=24),
                  axis.text.x = element_text(color="black", size=24),
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  plot.title = element_text(hjust = 0.5, size=24),
                  strip.background = element_blank(),
                  strip.text = element_text(size=24)
            ))

# f0 median

ggplot(ipus |> filter(task=="Lists"), aes(turnNormalTask, f0medz))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap2(~condition,
              labeller = labeller(condition = c("close" = "Personal condition", "impersonal" = "Impersonal condition")),
              strip=strip_themed(text_x = elem_list_text(color=c(personalcol, impersonalcol))))+
  labs(x="Time (normalized)", y = "f0 median (z-scored)")+
  scale_x_continuous(breaks=c(0, 0.5, 1), labels=c("0", "0.5", "1"))
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/temp-f0Med.png",  width=5750, height=3000, units="px", dpi=500)

# f0 SD

ggplot(ipus |> filter(task=="Lists"), aes(turnNormalTask, f0sdz))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap2(~condition,
              labeller = labeller(condition = c("close" = "Personal condition", "impersonal" = "Impersonal condition")),
              strip=strip_themed(text_x = elem_list_text(color=c(personalcol, impersonalcol))))+
  labs(x="Time (normalized)", y = "f0 SD (z-scored)")+
  scale_x_continuous(breaks=c(0, 0.5, 1), labels=c("0", "0.5", "1"))
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/temp-f0SD.png",  width=5750, height=3000, units="px", dpi=500)

#################################
{all <- data.frame(matrix(nrow=0, ncol=7))
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
  mutate_at("condition", as.factor)}
################################

# Nose temperature - friendship likelihood

ggplot(d |> filter(ROI=="Nose"), aes(effect, becomeFriends, color=effect))+
  geom_boxplot(width=0.15, fatten = NULL)+
  ggdist::stat_halfeye(adjust = .6,  width = .7, justification = -.2, .width = c(.5, .95))+
  scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
  scale_x_discrete(labels=c("Decrease", "Not signif.", "Increase"))+
  theme(legend.position="none")+
  labs(x=NULL, y="Self-Rating", title="Likelihood of friendship")+
  ggsignif::geom_signif(comparisons = list(c("increase", "ns")),
                        annotations = c("*"), textsize=7, color="black")+
  scale_y_continuous(limits=c(2,9.67), breaks=c(2,4,6,8,10), labels=c("2","4","6","8",""))
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/temp-noseFriends.png",  width=3300, height=3000, units="px", dpi=500)

# f0 - temperature

{ipus <- merge(ipus,
              datChange |> 
                select(speaker, ROI, tempChangeLists, tempChangeDiapix, tempChangeEntireExp),
              by="speaker") |> 
    mutate(tempChangeCateg = case_when(
      tempChangeLists <= -2 & ROI == "Nose" ~ "A",
      tempChangeLists > -2 & tempChangeLists <= 0 & ROI == "Nose" ~ "B",
      tempChangeLists > 0 & tempChangeLists <= 0.5 & ROI == "Nose" ~ "C",
      tempChangeLists > 0.5 & tempChangeLists <= 1 & ROI == "Nose" ~ "D",
      tempChangeLists > 1 & ROI == "Nose" ~"E",
      tempChangeDiapix <= 0 & ROI == "Eyes" ~ "A",
      tempChangeDiapix > 0 & tempChangeDiapix <= 0.2  & ROI == "Eyes" ~ "B",
      tempChangeDiapix > 0.2 & tempChangeDiapix <= 0.35  & ROI == "Eyes" ~ "C",
      tempChangeDiapix > 0.35  & ROI == "Eyes" ~ "D"
    ))}

ggplot(ipus |> filter(tempChangeCateg == "B", ROI=="Nose"), aes(turnNormal, f0medz))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Time (normalized)", y = "f0 median (z-scored)", title="Temp. change: -2 – 0 °C")
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/temp-f0MedTemp1.png",  width=5750, height=3000, units="px", dpi=500)

ggplot(ipus |> filter(tempChangeCateg == "C", ROI=="Nose"), aes(turnNormal, f0medz))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Time (normalized)", y = "f0 median (z-scored)", title="Temp. change: 0 – 0.5 °C")
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/temp-f0MedTemp2.png",  width=5750, height=3000, units="px", dpi=500)

ggplot(ipus |> filter(tempChangeCateg == "D", ROI=="Nose"), aes(turnNormal, f0medz))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Time (normalized)", y = "f0 median (z-scored)", title="Temp. change: 0.5 – 1 °C")
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/temp-f0MedTemp3.png",  width=5750, height=3000, units="px", dpi=500)


ggplot(ipus |> filter(tempChangeCateg == "A", ROI=="Eyes"), aes(turnNormal, f0sdz))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Time (normalized)", y = "f0 median (z-scored)", title="Temp. change: -0.2 – 0 °C")
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/temp-f0SDTemp1.png",  width=5750, height=3000, units="px", dpi=500)

ggplot(ipus |> filter(tempChangeCateg == "B", ROI=="Eyes"), aes(turnNormal, f0sdz))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Time (normalized)", y = "f0 median (z-scored)", title="Temp. change: 0 – 0.2 °C")
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/temp-f0SDTemp2.png",  width=5750, height=3000, units="px", dpi=500)

ggplot(ipus |> filter(tempChangeCateg == "C", ROI=="Eyes"), aes(turnNormal, f0sdz))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Time (normalized)", y = "f0 median (z-scored)", title="Temp. change: 0.2 – 0.35 °C")
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/temp-f0SDTemp3.png",  width=5750, height=3000, units="px", dpi=500)


ggplot(ipus |> filter(tempChangeCateg == "D", ROI=="Eyes"), aes(turnNormal, f0sdz))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Time (normalized)", y = "f0 median (z-scored)", title="Temp. change: 0.35 – 0.4 °C")
ggsave("C:/Users/offredet/Documents/1HU/Defense/figures-PreviousExp/temp-f0SDTemp4.png",  width=5750, height=3000, units="px", dpi=500)


















