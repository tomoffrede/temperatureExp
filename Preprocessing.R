# Process data and make it ready for analysis

library(tidyverse)
library(tuneR)
library(rPraat)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/"
folderTG <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/ZNV/compare/"
folderSpeech <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/ZNV/compare/"
`%!in%` <- Negate(`%in%`)

m <- read.csv(paste0(folder, "metadata.csv"))

TXT <- list.files(folderSpeech, "\\.txt")
TXT <- TXT[!grepl("Register", TXT)]
# TGv <- list.files(folderTG, "OverlapSp.TextGrid") # v for vector
TGv <- list.files(folderTG, "^[A-Z]{3}-(D|L)[0-9]\\.TextGrid$") # v for vector

files <- data.frame(TXT) %>% 
  mutate(TG = case_when(substr(TXT, 1, 6) %in% substr(TGv, 1, 6) ~ TGv[match(substr(TXT, 1, 6), substr(TGv, 1, 6))]))

f0 <- data.frame(matrix(nrow=0, ncol=11))
names(f0) <- c("file", "speaker", "turn", "IPU", "f0mean", "f0med", "f0sd", "turnOnset", "turnOffset", "ipuOnset", "ipuOffset")

for(i in 1:nrow(files)){
  speaker <- ifelse(grepl("-A", files$TXT[i]), "A", "B")
  turnTier <- paste0("speaker", speaker)
  silenceTier <- paste0("silence", speaker)
  turnCount <- 0
  
  txt <- read.table(paste0(folderSpeech, files$TXT[i]), header = TRUE, na.strings = "--undefined--")
  tg <- tg.read(paste0(folderSpeech, files$TG[i]), encoding=detectEncoding(paste0(folderSpeech, files$TG[i])))
  
    for(p in 1:tg.getNumberOfIntervals(tg, turnTier)){
    if(tg.getLabel(tg, turnTier, p) == "s"){
      ipuCount <- 0
      turnCount <- turnCount + 1
      
      if(p > 2){ # make sure turnCount is right for turns with overlaps
        if(tg.getLabel(tg, turnTier, p-1) == "overlap"){
          if(tg.getLabel(tg, turnTier, p-2) == "s"){ # this means the turn had begun before the overlap
            turnCount <- turnCount - 1
          }
        }
      }
      
      turnOnset <- as.numeric(tg.getIntervalStartTime(tg, turnTier, p)) # if the turn had an overlap, these values aren't the actual onset and offset of the turn, but of the current section of the turn
      turnOffset <- as.numeric(tg.getIntervalEndTime(tg, turnTier, p))
      
      for(s in 1:tg.getNumberOfIntervals(tg, silenceTier)){
        if(tg.getLabel(tg, silenceTier, s)=="sounding"){
          startIPU <- as.numeric(tg.getIntervalStartTime(tg, silenceTier, s))
          endIPU <- as.numeric(tg.getIntervalEndTime(tg, silenceTier, s))
          if((startIPU >= turnOnset & startIPU < turnOffset) | (endIPU > turnOnset & endIPU <= turnOffset)){
            ipuCount <- ipuCount + 1
            f <- data.frame(matrix(nrow=0, ncol=3))
            names(f) <- c("f0mean", "f0med", "f0sd")
            for(l in 1:nrow(txt)){
              if(txt$onset[l] >= startIPU - 0.001){ # adding a 0.001 window because the txt file rounds up the onset and offset times
                if(txt$offset[l] <= endIPU + 0.001){
                  if(txt$onset[l] >= turnOnset - 0.001){
                    if(txt$offset[l] <= turnOffset + 0.001){
                      f[nrow(f)+1,] <- c(as.numeric(txt$f0mean[l]),
                                         as.numeric(txt$f0med[l]),
                                         as.numeric(txt$f0sd[l]))
                    }
                  }
                }
              }
            }
            if(any(!is.na(f))){
              f0[nrow(f0)+1,] <- c(substr(files$TXT[i], 1, 6),
                                   speaker,
                                   turnCount,
                                   ipuCount,
                                   mean(f$f0mean, na.rm=TRUE),
                                   mean(f$f0med, na.rm=TRUE),
                                   mean(f$f0sd, na.rm=TRUE),
                                   turnOnset, turnOffset, startIPU, endIPU)
            }
          }
        }
      }
    }
  }
}

f0save <- f0

table(paste0(f0$file, f0$speaker))

dat <- f0 %>% 
  mutate_at(c("f0mean", "f0med", "turn"), as.numeric) %>%
  mutate(file = ifelse(grepl("D1|D2", file),
                       paste0(file, "-", "BAD"), paste0(file, "-", "GOOD")),
         qual = ifelse(grepl("D1|D2", file), "bad", "good"),
         task = substr(file, 5, 6),
         prevf0 = NA) %>% 
  mutate_at(c("file", "speaker", "IPU"), as.factor) %>% 
  group_by(file, speaker) %>%
  mutate(index = 1:n()) %>% 
  ungroup()

ggplot(dat, aes(index, f0mean))+
  geom_boxplot(aes(color=speaker))+
  facet_wrap(~file, scales="free")

ggplot(dat, aes(speaker, f0mean))+
  geom_boxplot(aes(color=qual))

summary(lm(f0mean ~ qual , dat %>% filter(speaker=="A")))

for(i in 1:nrow(dat)){
  prevf0 <- dat$f0mean[dat$file == dat$file[i] &
                         dat$speaker != dat$speaker[i] &
                         dat$turn == dat$prevTurn[i] &
                         dat$IPU == dat$IPU[i]]
  if(!purrr::is_empty(prevf0)){
    if(!any(is.na(prevf0))){
      dat$prevf0[i] <- prevf0
    }
  }
}

# for(f in files){
#   if(grepl("AML|FWR", f)){cat <- "Bad"}else{cat <- "Good"}
#   t <- read.table(paste0(folderSpeech, f), header = TRUE)
#   t$index <- 1:nrow(t)
#   ggplot(t, aes(index, f0mean))+
#     geom_point()+
#     ggtitle(cat)
#   readline("Press Enter")
# }




























