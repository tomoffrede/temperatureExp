# Tom Offrede
# Figures for Psychophysiology paper

library(tidyverse)
library(ggpubr)
library(ggsignif)
library(broom)
library(viridis)
library(patchwork)

load(paste0(here::here(), "/data/speechData-allIPUs.RData"))
load(paste0(here::here(), "/data/individualTemp.RData"))
load(paste0(here::here(), "/data/metadata-clean.RData"))
load(paste0(here::here(), "/data/tempDataChange.RData"))
load(paste0(here::here(), "/data/tempData.RData"))
meta <- m

folderFig <- paste0(here::here(), "/figures/Psychophysiology/")

colorF <- "#5F1F53FF"
colorE <- "#B01759FF"
colorN <- "#F58A62FF"
colorC <- "#EB493EFF"

# to get color codes from viridis:
# scales::show_col(viridis_pal(option="F")(30))

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

# TESTING COLORS
{
  
  # # D: -- this palette is beautiful but it doesn't have clear boundaries between colors
  # colorF <- "#75D054FF"
  # colorE <- #"#31688EFF"
  #   #"#39558CFF"
  #   "#287C8EFF"
  # colorC <- #"#35B779FF"
  #   "#20A486FF"
  # colorN <- "#440154FF"
  
  # # C:
  # colorF <- "#F48849FF"
  # colorE <- "#0D0887FF"
  # colorC <- "#DB5C68FF"
  # colorN <- "#6E00A8FF"
  
  # # F:
  # colorF <- #"#6A1F56FF"
  #   "#421B45FF"
  # colorE <- #"#B91657FF"
  #   "#B01759FF"
  # colorN <- "#F58A62FF"
  # #"#F59A70FF"
  # # "#F4825AFF"
  # colorC <- #"#ED513EFF"
  #   "#EB493EFF"
  
  # # H:
  # colorF <- "#30123BFF"
  # colorE <- "#476FE7FF"
  # colorN <- "#F46718FF"
  # colorC <- "#9E1001FF"
  
  # n <- ggplot(d |> filter(ROI=="Nose", section=="Lists"), aes(effect, becomeFriends))+
  #   geom_boxplot(size=1, color=colorN)+
  #   geom_signif(comparisons = list(c("ns", "increase")),
  #               annotations = c("*"),
  #               textsize=6, size=1,
  #               y=9.2)+
  #   labs(title="Nose", y = "Likelihood of friendship", x="Temperature change")+
  #   # scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
  #   scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
  #   # scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
  #   theme(legend.position="none",
  #         plot.title = element_text(color=colorN))
  # f <- ggplot(d |> filter(ROI=="Forehead", section=="Lists"), aes(effect, becomeFriends))+
  #   geom_boxplot(size=1, color=colorF)+
  #   geom_signif(comparisons = list(c("ns", "increase")),
  #               annotations = c("*"),
  #               textsize=6, size=1,
  #               y=9.2)+
  #   labs(title="Forehead", y = "Likelihood of friendship", x="Temperature change")+
  #   # scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
  #   scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
  #   # scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
  #   theme(legend.position="none",
  #         plot.title = element_text(color=colorF))
  # c <- ggplot(d |> filter(ROI=="Cheeks", section=="Lists"), aes(effect, becomeFriends))+
  #   geom_boxplot(size=1, color=colorC)+
  #   geom_signif(comparisons = list(c("ns", "increase")),
  #               annotations = c("*"),
  #               textsize=6, size=1,
  #               y=9.2)+
  #   labs(title="Cheeks", y = "Likelihood of friendship", x="Temperature change")+
  #   # scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
  #   scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
  #   # scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
  #   theme(legend.position="none",
  #         plot.title = element_text(color=colorC))
  # e <- ggplot(d |> filter(ROI=="Eyes", section=="Lists"), aes(effect, becomeFriends))+
  #   geom_boxplot(size=1, color=colorE)+
  #   geom_signif(comparisons = list(c("ns", "increase")),
  #               annotations = c("*"),
  #               textsize=6, size=1,
  #               y=9.2)+
  #   labs(title="Eyes", y = "Likelihood of friendship", x="Temperature change")+
  #   # scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
  #   scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
  #   # scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
  #   theme(legend.position="none",
  #         plot.title = element_text(color=colorE))
  # annotate_figure(ggarrange(n,f,c,e),
  #                 top=text_grob(expression("1"^"st"* " half of experiment: Temperature change and emotion"), size=14))
}

###############################################


# thermal comfort

(p1 <- ggplot(meta, aes(condition, comfortPre))+
   geom_boxplot(width=0.15, fatten = NULL, size=1)+
   ggdist::stat_halfeye(adjust = .8,  width = .7, justification = -.2, .width = c(.5, .95))+
   # ggdist::stat_halfeye(adjust = .7,  width = .5, justification = -.15, .width = c(.5, .95))+
   labs(title="Before Interaction", y="Thermal Comfort", x=NULL)+
   theme(plot.title = element_text(size=12))+
   scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
   ylim(c(3.5,9)))

(p2 <- ggplot(meta, aes(condition, comfortPost))+
    geom_boxplot(width=0.15, fatten = NULL, size=1)+
    ggdist::stat_halfeye(adjust = .8,  width = .7, justification = -.2, .width = c(.5, .95))+
    geom_signif(comparisons = list(c("close", "impersonal")),
                annotations = c("*"),
                y=8.5, textsize=6, size=1)+
    # ggdist::stat_halfeye(adjust = .7,  width = .5, justification = -.15, .width = c(.5, .95))+
    labs(title="After Interaction", y=NULL, x=NULL)+
    theme(plot.title = element_text(size=12),
          axis.text.y = element_blank())+
    scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    ylim(c(3.5,9)))

annotate_figure(ggarrange(p1, p2),
                top=text_grob("Group Difference in Thermal Comfort", size=14),
                bottom=text_grob("Condition", size=12))
ggsave(paste0(folderFig, "thermalComfort.png"), dpi="retina", height = 1150, width=1700, units = "px")

############################################################

# Questionnaire items

(i1 <- ggplot(meta, aes(condition, closeness))+
   geom_boxplot(width=0.15, fatten = NULL, size=1)+
   ggdist::stat_halfeye(adjust = .8,  width = .7, justification = -.2, .width = c(.5, .95))+
   geom_signif(comparisons = list(c("close", "impersonal")),
               annotations = c("*"),
               y=8.3, textsize=6, size=1)+
   labs(title="Closeness", y="Self-Rating", x=NULL)+
   scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
   ylim(c(1,9)))

(i2 <- ggplot(meta, aes(condition, likeability))+
    geom_boxplot(width=0.15, fatten = NULL, size=1)+
    ggdist::stat_halfeye(adjust = .8,  width = .7, justification = -.2, .width = c(.5, .95))+
    geom_signif(comparisons = list(c("close", "impersonal")),
                annotations = c("*"),
                y=2.5, textsize=6, size=1,
                vjust=2, tip_length = -0.03)+
    labs(title="Degree of Liking", y=NULL, x=NULL)+
    scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    ylim(c(1,9)))

annotate_figure(ggarrange(i1, i2),
                bottom = text_grob("Conditon", size=12))
ggsave(paste0(folderFig, "questionnaire.png"), dpi="retina", height = 1150, width=1700, units = "px")


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
all$effect <- factor(all$effect, levels=c("decrease", "ns", "increase"))
d <- merge(all |> 
             select(-condition) |> 
             mutate_at(c("sign", "effect"), as.factor),
           meta, by="speaker") |> 
  mutate_at("condition", as.factor)

############################################################

# Social Emotion - Questions section

# Nose-becomeFriends

(n <- ggplot(d |> filter(ROI=="Nose", section=="Lists"), aes(effect, becomeFriends))+
   geom_boxplot(width=0.15, fatten = NULL, color=colorN, size=1)+
   ggdist::stat_halfeye(adjust = .6,  width = .7, justification = -.2, .width = c(.5, .95),
                        color=colorN)+
   geom_signif(comparisons = list(c("ns", "increase")),
               annotations = c("*"),
               textsize=6, size=1,
               y=9.2)+
   labs(title="Nose", y = "Likelihood of friendship", x="Temperature change")+
   scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
   scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
   # scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
   theme(legend.position="none",
         plot.title = element_text(color=colorN))
)

annotate_figure(n,
                top=text_grob(expression("1"^"st"* " half of experiment: Temperature change and emotion"), size=14))
ggsave(paste0(folderFig, "emotion-lists.png"), dpi="retina", height = 1200, width=1700, units = "px")

############################################################

# Social Emotion - Diapix section

# Nose-becomeFriends
# Forehead-closeness
# Forehead-likeability

(n <- ggplot(d |> filter(ROI=="Nose", section=="Diapix"), aes(effect, becomeFriends))+
   geom_boxplot(width=0.15, fatten = NULL, color=colorN, size=1)+
   ggdist::stat_halfeye(adjust = .6,  width = .7, justification = -.2, .width = c(.5, .95),
                        color=colorN)+
   geom_signif(comparisons = list(c("ns", "decrease")),
               annotations = c("*"),
               textsize=6, size=1,
               y=9.1)+
   labs(title="Nose", y = "Likelihood of friendship", x=NULL)+
   scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
   scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
   scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
   theme(plot.title = element_text(color=colorN, size=18),
         axis.title = element_text(size=16),
         axis.text.x = element_text(size=16),
         axis.text.y = element_text(size=12))
) 

(f1 <- ggplot(d |> filter(ROI=="Forehead", section=="Diapix"), aes(effect, closeness))+
    geom_boxplot(width=0.15, fatten = NULL, color=colorF, size=1)+
    ggdist::stat_halfeye(adjust = .6,  width = .7, justification = -.2, .width = c(.5, .95),
                         color=colorF)+
    geom_signif(comparisons = list(c("ns", "increase")),
                annotations = c("*"),
                textsize=6, size=1,
                y=8.4)+
    labs(title="Forehead", y = "Closeness", x=NULL)+
    scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    # scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
    theme(plot.title = element_text(color=colorF, size=18),
          axis.title = element_text(size=16),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=12))
)

(f2 <- ggplot(d |> filter(ROI=="Forehead", section=="Diapix"), aes(effect, likeability))+
    geom_boxplot(width=0.15, fatten = NULL, color=colorF, size=1)+
    ggdist::stat_halfeye(adjust = .6,  width = .7, justification = -.2, .width = c(.5, .95),
                         color=colorF)+
    geom_signif(comparisons = list(c("ns", "increase")),
                annotations = c("*"),
                textsize=6, size=1,
                y=9.25)+
    labs(title="Forehead", y = "Degree of liking", x=NULL)+
    scale_y_continuous(limits=c(1,10), breaks=c(2, 4, 6, 8))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    scale_color_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "honeydew4"))+
    theme(plot.title = element_text(color=colorF, size=18),
          axis.title = element_text(size=16),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=12))
)

# p <- cowplot::ggdraw(cowplot::plot_grid(cowplot::plot_grid(NULL, f, NULL, ncol=3, rel_widths = c(0.25,0.5,0.25)),
#                    cowplot::plot_grid(n, e, ncol=2),
#                    ncol=1))

annotate_figure(ggarrange(ggarrange(f1,f2),
                cowplot::plot_grid(NULL, n, NULL, ncol=3, rel_widths = c(0.25,0.5,0.25)),
                nrow=2),
                top=text_grob(expression("2"^"nd"* " half of experiment: Temperature change and emotion"), size=18),
                bottom=text_grob("Temperature change", size=16))
ggsave(paste0(folderFig, "emotion-diapix.png"), dpi="retina", height = 2000, width=3000, units = "px")
############################################################

# Individual differences - Questions section

# Cheeks-agreeableness

(c <- ggplot(d |> filter(ROI=="Cheeks", section=="Lists"), aes(effect, agreeableness))+
   geom_boxplot(width=0.15, fatten = NULL, color=colorC, size=1)+
   ggdist::stat_halfeye(adjust = .6,  width = .7, justification = -.2, .width = c(.5, .95),
                        color=colorC)+
   geom_signif(comparisons = list(c("ns", "increase")),
               annotations = c("*"),
               textsize=6, size=1,
               y=4.6)+
   labs(title="Cheeks", y = "Agreeableness", x="Temperature change")+
   scale_y_continuous(limits=c(2.5,5), breaks=c(2, 4, 6, 8))+
   scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
   theme(plot.title = element_text(color=colorC))
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
   geom_boxplot(width=0.15, fatten = NULL, color=colorN, size=1)+
   ggdist::stat_halfeye(adjust = .6,  width = .4, justification = -.3, .width = c(.5, .95),
                        color=colorN)+
   geom_signif(comparisons = list(c("ns", "increase")),
               annotations = c("*"),
               textsize=9, size=1,
               y= 5)+
   labs(title="Nose", y =NULL, x=NULL)+
   scale_y_continuous(limits=c(1,6), breaks=c(2, 4, 6))+
   scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
   theme(legend.position="none",
         plot.title = element_text(color=colorN, size=20),
         axis.title = element_text(size=18),
         axis.text.x = element_text(size=18),
         axis.text.y = element_blank())
)

(f1 <- ggplot(d |> filter(ROI=="Forehead", section=="Diapix"), aes(effect, extraversion))+
    geom_boxplot(width=0.15, fatten = NULL, color=colorF, size=1)+
    ggdist::stat_halfeye(adjust = .6,  width = .4, justification = -.3, .width = c(.5, .95),
                         color=colorF)+
    geom_signif(comparisons = list(c("ns", "increase")),
                annotations = c("*"),
                textsize=9, size=1,
                y=5)+
    labs(title="Forehead", y = "Extraversion", x=NULL)+
    scale_y_continuous(limits=c(1,6), breaks=c(2, 4, 6))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    theme(legend.position="none",
          plot.title = element_text(color=colorF, size=20),
          axis.title.x = element_text(size=18),
          axis.title.y = element_text(size=20),
          axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=14))
)

(n2 <- ggplot(d |> filter(ROI=="Nose", section=="Diapix"), aes(effect, openness))+
    geom_boxplot(width=0.15, fatten = NULL, color=colorN, size=1)+
    ggdist::stat_halfeye(adjust = .6,  width = .4, justification = -.3, .width = c(.5, .95),
                         color=colorN)+
    geom_signif(comparisons = list(c("ns", "decrease"), c("ns", "increase")),
                annotations = c("*", "**"),
                textsize=9, size=1#,
                #y=c(5.1, 5.6)
    )+
    labs(title="Nose", y = NULL, x=NULL)+
    scale_y_continuous(limits=c(1,6), breaks=c(2, 4, 6, 8))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    theme(legend.position="none",
          plot.title = element_text(color=colorN, size=20),
          axis.title.x = element_text(size=18),
          axis.title.y = element_text(size=20),
          axis.text.x = element_text(size=18),
          axis.text.y = element_blank())
)

(f2 <- ggplot(d |> filter(ROI=="Forehead", section=="Diapix"), aes(effect, openness))+
    geom_boxplot(width=0.15, fatten = NULL, color=colorF, size=1)+
ggdist::stat_halfeye(adjust = .6,  width = .4, justification = -.3, .width = c(.5, .95),
color=colorF)+
    geom_signif(comparisons = list(c("ns", "increase")),
                annotations = c("*"),
                textsize=9, size=1,
                y=5.35)+
    labs(title="Forehead", y = "Openness", x=NULL)+
    scale_y_continuous(limits=c(1,6), breaks=c(2, 4, 6, 8))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    theme(legend.position="none",
          plot.title = element_text(color=colorF, size=20),
          axis.title.x = element_text(size=18),
          axis.title.y = element_text(size=20),
          axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=14))
)

(e <- ggplot(d |> filter(ROI=="Eyes", section=="Diapix"), aes(effect, agreeableness))+
    geom_boxplot(width=0.15, fatten = NULL, color=colorE, size=1)+
    ggdist::stat_halfeye(adjust = .6,  width = .6, justification = -.3, .width = c(.5, .95),
                         color=colorE)+
    geom_signif(comparisons = list(c("ns", "increase")),
                annotations = c("*"),
                textsize=9, size=1,
                y=5)+
    labs(title="Eyes", y = "Agreeableness", x=NULL)+
    scale_y_continuous(limits=c(1,6), breaks=c(2, 4, 6, 8))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    theme(legend.position="none",
          plot.title = element_text(color=colorE, size=20),
          axis.title.x = element_text(size=18),
          axis.title.y = element_text(size=20),
          axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=14))
)

(c <- ggplot(d |> filter(ROI=="Cheeks", section=="Diapix"), aes(effect, agreeableness))+
    geom_boxplot(width=0.15, fatten = NULL, color=colorC, size=1)+
    ggdist::stat_halfeye(adjust = .6,  width = .4, justification = -.3, .width = c(.5, .95),
                         color=colorC)+
    geom_signif(comparisons = list(c("ns", "increase")),
                annotations = c("*"),
                textsize=9, size=1,
                y=5)+
    labs(title="Cheeks", y = NULL, x=NULL)+
    scale_y_continuous(limits=c(1,6), breaks=c(2, 4, 6, 8))+
    scale_x_discrete(labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    theme(legend.position="none",
          plot.title = element_text(color=colorC, size=20),
          axis.title = element_text(size=18),
          axis.text.x = element_text(size=18),
          axis.text.y = element_blank())
)

annotate_figure(ggarrange(ggarrange(f1,n1),
                          ggarrange(f2,n2),
                          ggarrange(e,c),
                          nrow=3),
                top=text_grob(expression("2"^"nd"* " half of experiment: Temperature change and personality"), size=20),
                bottom=text_grob("Temperature change", size=18))
ggsave(paste0(folderFig, "bfi-diapix.png"), dpi="retina", height = 5000, width=3000, units = "px")
############################################################

# Condition - Lists section

# Cheeks

############################################################

# Condition - Diapix section

# Forehead
# Cheeks

d$effect <- factor(d$effect, levels=c("ns", "decrease", "increase"))

data <- d |> 
  filter(section=="Diapix", ROI%in%c("Forehead", "Cheeks")) |> 
  select(speaker, condition, effect, ROI) |> 
  group_by(condition, effect, ROI) |> 
  summarize(effectCount = n())

(f <- ggplot(data |> filter(ROI=="Forehead"), aes(condition, effectCount, fill=effect))+
    geom_bar(stat="identity")+
    geom_signif(comparisons = list(c("close", "impersonal")),
                annotations = c("*"),
                y=20.5,
                size=1, textsize=7)+
    labs(title="Forehead", y = "Frequency of effect", x=NULL, fill="Temperature change")+
    scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    scale_fill_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "gray"),
                      labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    ylim(c(0,23))+
    theme(plot.title = element_text(color=colorF))
)
(c <- ggplot(data |> filter(ROI=="Cheeks"), aes(condition, effectCount, fill=effect))+
    geom_bar(stat="identity")+
    geom_signif(comparisons = list(c("close", "impersonal")),
                annotations = c("*"),
                y=20.5,
                size=1, textsize=7)+
    labs(title="Cheeks", y = NULL, x=NULL, fill="Temperature change")+
    scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    scale_fill_manual(values = c("decrease" = "cadetblue3", "increase" = "red", "ns" = "gray"),
                      labels=c("ns"="Not signif.", "decrease"="Decrease", "increase"="Increase"))+
    ylim(c(0,23))+
    theme(plot.title = element_text(color=colorC))
)



annotate_figure(ggarrange(f,c, common.legend = TRUE, legend="right"),
                top=text_grob(expression("2"^"nd"* " half of experiment: Temperature change and condition"), size=14),
                bottom=text_grob("Condition", size=12))

ggsave(paste0(folderFig, "condition-diapix.png"), dpi="retina", height = 1200, width=2000, units = "px")
