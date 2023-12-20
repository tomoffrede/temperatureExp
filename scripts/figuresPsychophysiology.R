# Tom Offrede
# Figures for Psychophysiology paper

library(tidyverse)
library(ggpubr)
library(ggsignif)
library(broom)
library(viridis)

load(paste0(here::here(), "/data/speechData-allIPUs.RData"))
load(paste0(here::here(), "/data/individualTemp.RData"))
load(paste0(here::here(), "/data/metadata-clean.RData"))
load(paste0(here::here(), "/data/tempDataChange.RData"))
load(paste0(here::here(), "/data/tempData.RData"))
meta <- m

folderFig <- paste0(here::here(), "/figures/Psychophysiology/")

colorF <- "#FDE725FF"
colorE <- "#31688EFF"
colorC <- "#35B779FF"
colorM <- "#440154FF"

# to get color codes from viridis:
scales::show_col(viridis_pal(option="D")(4))

# set theme for figures
theme_set(theme_minimal()+
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.title = element_text(size=12),
                  axis.text.x = element_text(size=12, color="black"),
                  strip.text = element_text(size = 12),
                  plot.title = element_text(size=14, hjust=0.5),
                  legend.title = element_text(size=11),
                  legend.text = element_text(size=10)))

############################################################

# thermal comfort

(p1 <- ggplot(meta, aes(condition, comfortPre))+
    geom_boxplot(size=1)+
    # ggdist::stat_halfeye(adjust = .7,  width = .5, justification = -.15, .width = c(.5, .95))+
    labs(title="Before Interaction", y="Thermal Comfort", x="")+
    theme(plot.title = element_text(size=12))+
    scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    ylim(c(3.5,9)))

(p2 <- ggplot(meta, aes(condition, comfortPost))+
    geom_boxplot(size=1)+
    geom_signif(comparisons = list(c("close", "impersonal")),
                annotations = c("*"),
                y=8.5, textsize=6, size=1)+
    # ggdist::stat_halfeye(adjust = .7,  width = .5, justification = -.15, .width = c(.5, .95))+
    labs(title="After Interaction", y="", x="")+
    theme(plot.title = element_text(size=12))+
    scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    ylim(c(3.5,9)))

annotate_figure(ggarrange(p1, p2),
                top=text_grob("Group Difference in Thermal Comfort", size=14),
                bottom=text_grob("Condition", size=12))
ggsave(paste0(folderFig, "thermalComfort.png"), dpi="retina", height = 1000, width=1500, units = "px")

############################################################

# Questionnaire items

(i1 <- ggplot(meta, aes(condition, closeness))+
    geom_boxplot(size=1)+
    geom_signif(comparisons = list(c("close", "impersonal")),
                annotations = c("*"),
                y=8.3, textsize=6, size=1)+
    labs(title="Closeness", y="Self-Rating", x="")+
    theme(plot.title = element_text(size=12))+
    scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    ylim(c(1,9)))

(i2 <- ggplot(meta, aes(condition, likeability))+
    geom_boxplot(size=1)+
    geom_signif(comparisons = list(c("close", "impersonal")),
                annotations = c("*"),
                y=2.5, textsize=6, size=1,
                vjust=2, tip_length = -0.03)+
    labs(title="Degree of Liking", y="", x="")+
    theme(plot.title = element_text(size=12))+
    scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    ylim(c(1,9)))

annotate_figure(ggarrange(i1, i2),
                top=text_grob("Perception of Partner", size=14),
                bottom = text_grob("Conditon", size=12))
ggsave(paste0(folderFig, "questionnaire.png"), dpi="retina", height = 1000, width=1500, units = "px")


############################################################
all <- data.frame(matrix(nrow=0, ncol=7))
names(all) <- c("speaker", "condition", "ROI", "direction", "t", "p", "section")
for(s in unique(dat$speaker)){
  for(r in unique(dat$ROI)){
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
d <- merge(all |> 
             select(-condition) |> 
             mutate_at(c("sign", "effect"), as.factor),
           meta, by="speaker") |> 
  mutate_at("condition", as.factor)
############################################################

# Social Emotion - Questions section

# Nose-becomeFriends

(n <- ggplot(d |> filter(ROI=="Nose", section=="Lists"), aes(effect, becomeFriends))+
   geom_boxplot(size=1, aes(color=effect))+
   geom_signif(comparisons = list(c("ns", "increase")),
               annotations = c("*"),
               textsize=6, size=1,
               y=9.2)+
   labs(title="Nose", y = "Likelihood of friendship", x="Temperature change")+
   # scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
   scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
   scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
   theme(legend.position="none")
)

annotate_figure(n,
                top=text_grob(expression("1"^"st"* " half of experiment: Temperature change and emotion"), size=14))
ggsave(paste0(folderFig, "emotion-lists.png"), dpi="retina", height = 1000, width=1700, units = "px")

############################################################

# Social Emotion - Diapix section

# Nose-becomeFriends
# Eyes-closeness
# Forehead-likeability

(n <- ggplot(d |> filter(ROI=="Nose", section=="Diapix"), aes(effect, becomeFriends))+
   geom_boxplot(size=1, aes(color=effect))+
   geom_signif(comparisons = list(c("ns", "decrease")),
               annotations = c("*"),
               textsize=6, size=1,
               y=9.3)+
   labs(title="Nose", y = "Likelihood of friendship", x="")+
   scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
   scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
   scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
   theme(legend.position="none")
)

(e <- ggplot(d |> filter(ROI=="Eyes", section=="Diapix"), aes(effect, closeness))+
    geom_boxplot(size=1, aes(color=effect))+
    geom_signif(comparisons = list(c("ns", "increase")),
                annotations = c("*"),
                textsize=6, size=1,
                y=8.4)+
    labs(title="Eyes", y = "Closeness", x="Temperature change")+
    scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
    theme(legend.position="none")
)

(f <- ggplot(d |> filter(ROI=="Forehead", section=="Diapix"), aes(effect, likeability))+
    geom_boxplot(size=1, aes(color=effect))+
    geom_signif(comparisons = list(c("ns", "increase")),
                annotations = c("*"),
                textsize=6, size=1,
                y=9.25)+
    labs(title="Forehead", y = "Degree of liking", x="")+
    scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
    theme(legend.position="none")
)

annotate_figure(ggarrange(n, e, f, nrow=1),
                top=text_grob(expression("2"^"nd"* " half of experiment: Temperature change and emotion"), size=14))
ggsave(paste0(folderFig, "emotion-diapix.png"), dpi="retina", height = 2000, width=3000, units = "px")
############################################################

# Individual differences - Questions section

# Cheeks-agreeableness

(c <- ggplot(d |> filter(ROI=="Cheeks", section=="Lists"), aes(effect, agreeableness))+
   geom_boxplot(size=1, aes(color=effect))+
   geom_signif(comparisons = list(c("ns", "increase")),
               annotations = c("*"),
               textsize=6, size=1,
               y=4.5)+
   labs(title="Cheeks", y = "Agreeableness", x="Temperature change")+
   # scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
   scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
   scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
   theme(legend.position="none")
)

annotate_figure(c,
                top=text_grob(expression("1"^"st"* " half of experiment: Temperature change and personality"), size=14))
ggsave(paste0(folderFig, "bfi-lists.png"), dpi="retina", height = 1000, width=1700, units = "px")
############################################################

# Individual differences - Diapix section

# Nose-extraversion
# Forehead-extraversion

# Nose-openness
# Forehead-openness

# Eyes-agreeableness
# Cheeks-agreeableness

(n1 <- ggplot(d |> filter(ROI=="Nose", section=="Diapix"), aes(effect, extraversion))+
   geom_boxplot(size=1, aes(color=effect))+
   geom_signif(comparisons = list(c("ns", "increase")),
               annotations = c("*"),
               textsize=6, size=1)+
   labs(title="Nose", y = "Score", x="")+
   # scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
   scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
   scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
   theme(legend.position="none")
)

(f1 <- ggplot(d |> filter(ROI=="Forehead", section=="Diapix"), aes(effect, extraversion))+
    geom_boxplot(size=1, aes(color=effect))+
    geom_signif(comparisons = list(c("ns", "increase")),
                annotations = c("*"),
                textsize=6, size=1)+
    labs(title="Forehead", y = "", x="")+
    # scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
    theme(legend.position="none")
)

(n2 <- ggplot(d |> filter(ROI=="Nose", section=="Diapix"), aes(effect, openness))+
    geom_boxplot(size=1, aes(color=effect))+
    geom_signif(comparisons = list(c("ns", "decrease"), c("ns", "increase")),
                annotations = c("*", "**"),
                textsize=6, size=1,
                y=c(5.1, 5.3))+
    labs(title="Nose", y = "Score", x="")+
    # scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
    theme(legend.position="none")
)

(f2 <- ggplot(d |> filter(ROI=="Forehead", section=="Diapix"), aes(effect, openness))+
    geom_boxplot(size=1, aes(color=effect))+
    geom_signif(comparisons = list(c("ns", "increase")),
                annotations = c("*"),
                textsize=6, size=1)+
    labs(title="Forehead", y = "", x="")+
    # scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
    theme(legend.position="none")
)

(e <- ggplot(d |> filter(ROI=="Eyes", section=="Diapix"), aes(effect, agreeableness))+
    geom_boxplot(size=1, aes(color=effect))+
    geom_signif(comparisons = list(c("ns", "increase")),
                annotations = c("*"),
                textsize=6, size=1)+
    labs(title="Eyes", y = "Score", x="Temperature change")+
    # scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
    theme(legend.position="none")
)

(c <- ggplot(d |> filter(ROI=="Cheeks", section=="Diapix"), aes(effect, agreeableness))+
    geom_boxplot(size=1, aes(color=effect))+
    geom_signif(comparisons = list(c("ns", "increase")),
                annotations = c("*"),
                textsize=6, size=1)+
    labs(title="Cheeks", y = "", x="Temperature change")+
    # scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
    theme(legend.position="none")
)

ggarrange(annotate_figure(ggarrange(n1,f1), top=text_grob("Extraversion", size=14)),
          annotate_figure(ggarrange(n2,f2), top=text_grob("Openness", size=14)),
          annotate_figure(ggarrange(e,c), top=text_grob("Agreeableness", size=14)),
          nrow=3)
############################################################

# Condition - Diapix section

# Forehead
# Cheeks



