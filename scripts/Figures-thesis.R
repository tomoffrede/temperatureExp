# Tom Offrede
# figures for thesis

library(tidyverse)
library(ggsignif)
library(ggpubr)

folderData <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/temperatureExp/data/"
load(paste0(folderData, "metadata-clean.RData"))
m <- m |> 
  mutate(dyad = substr(speaker, 1, 3))

# set theme for figures
theme_set(theme_minimal()+
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  # plot.background = element_rect(colour = "black", fill=NA),
                  axis.title = element_text(size=10),
                  axis.text.x = element_text(size=10, color="black"),
                  strip.text = element_text(size = 10),
                  plot.title = element_text(size=12, hjust=0.5),
                  legend.title = element_text(size=9),
                  legend.text = element_text(size=8)))

(c <- ggplot(m, aes(condition, closeness))+
  geom_boxplot(width=0.15, fatten = NULL, size=1)+
  ggdist::stat_halfeye(adjust = .8,  width = .7, justification = -.2, .width = c(.5, .95))+
  geom_signif(comparisons = list(c("close", "impersonal")),
              annotations = c("*"), textsize=7, color="black",
              y=8.2)+
  labs(x=NULL,
       y="Rating",
       title="Closeness")+
  scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    scale_y_continuous(limits=c(1,9), breaks=c(2,4,6,8)))

(l <- ggplot(m, aes(condition, likeability))+
  geom_boxplot(width=0.15, fatten = NULL, size=1)+
  ggdist::stat_halfeye(adjust = .8,  width = .7, justification = -.2, .width = c(.5, .95))+
  geom_signif(comparisons = list(c("close", "impersonal")),
              annotations = c("*"), textsize=7, color="black",
              y=9.175)+
  labs(x=NULL,
       y=NULL,
       title="Partner is Liked")+
  scale_x_discrete(labels=c("close"="Personal", "impersonal"="Impersonal"))+
    scale_y_continuous(limits=c(4,9.75), breaks=c(5,7,9)))

annotate_figure(ggarrange(c,l),
                bottom=text_grob("Condition", size=10))+
  border()

ggsave("C:/Users/offredet/Documents/1HU/Thesis/RCITratings.png", dpi="retina", height=950, width=1500, unit="px")
