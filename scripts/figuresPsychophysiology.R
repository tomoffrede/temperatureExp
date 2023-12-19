# Tom Offrede
# Figures for Psychophysiology paper

library(tidyverse)
library(ggpubr)
library(ggsignif)
# library(broom)

load(paste0(here::here(), "/data/speechData-allIPUs.RData"))
load(paste0(here::here(), "/data/individualTemp.RData"))
load(paste0(here::here(), "/data/metadata-clean.RData"))
load(paste0(here::here(), "/data/tempDataChange.RData"))
load(paste0(here::here(), "/data/tempData.RData"))
meta <- m

folderFig <- paste0(here::here(), "/figures/Psychophysiology/")

# set theme for figures
theme_set(theme_minimal()+
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  axis.title = element_text(size=12),
                  axis.text.x = element_text(size=10),
                  strip.text = element_text(size = 12),
                  plot.title = element_text(size=14, hjust=0.5),
                  legend.title = element_text(size=11),
                  legend.text = element_text(size=10)))


# thermal comfort

(p1 <- ggplot(meta, aes(condition, comfortPre))+
    geom_boxplot(size=1)+
    # ggdist::stat_halfeye(adjust = .7,  width = .5, justification = -.15, .width = c(.5, .95))+
    labs(title="Before Interaction", y="Thermal Comfort", x="")+
    theme(axis.text.x = element_text(color="black"),
          plot.title = element_text(size=12))+
    scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    ylim(c(3.5,9)))

(p2 <- ggplot(meta, aes(condition, comfortPost))+
    geom_boxplot(size=1)+
    geom_signif(comparisons = list(c("close", "impersonal")),
                annotations = c("*"),
                y=8.5, textsize=5)+
    # ggdist::stat_halfeye(adjust = .7,  width = .5, justification = -.15, .width = c(.5, .95))+
    labs(title="After Interaction", y="", x="")+
    theme(axis.text.x = element_text(color="black"),
          plot.title = element_text(size=12))+
    scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    ylim(c(3.5,9)))

annotate_figure(ggarrange(p1, p2),
                top=text_grob("Group Difference in Thermal Comfort", size=14),
                bottom=text_grob("Condition", size=12))
ggsave(paste0(folderFig, "thermalComfort.png"), dpi="retina", height = 1000, width=1500, units = "px")


# Questionnaire items

(i1 <- ggplot(meta, aes(condition, closeness))+
    geom_boxplot(size=1)+
    geom_signif(comparisons = list(c("close", "impersonal")),
                annotations = c("*"),
                y=8.3, textsize=5)+
    labs(title="Closeness", y="Self-Rating", x="")+
    theme(axis.text.x = element_text(color="black"),
          plot.title = element_text(size=12))+
    scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    ylim(c(1,9)))

(i2 <- ggplot(meta, aes(condition, likeability))+
    geom_boxplot(size=1)+
    geom_signif(comparisons = list(c("close", "impersonal")),
                annotations = c("*"),
                y=2.5, textsize=5,
                vjust=2, tip_length = -0.03)+
    labs(title="Degree of Liking", y="", x="")+
    theme(axis.text.x = element_text(color="black"),
          plot.title = element_text(size=12))+
    scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    ylim(c(1,9)))

annotate_figure(ggarrange(i1, i2),
                top=text_grob("Perception of Partner", size=14),
                bottom = text_grob("Conditon", size=12))
ggsave(paste0(folderFig, "questionnaire.png"), dpi="retina", height = 1000, width=1500, units = "px")

# Social Emotion - Questions section

# Social Emotion - Diapix section

# Individual differences - Questions section

# Individual differences - Diapix section






