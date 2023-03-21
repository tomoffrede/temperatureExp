# Process data and make it ready for analysis

library(tidyverse)
library(tuneR)
library(rPraat)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/"
folderTG <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/compare/"
folderSpeech <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/compare/"
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
  speaker <- paste0(substr(files$TXT[i], 1, 4), substr(files$TXT[i], 8, 8))
  sp <- ifelse(grepl("-A", files$TXT[i]), "A", "B")
  turnTier <- paste0("speaker", sp)
  silenceTier <- paste0("silence", sp)
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

dat <- f0 %>% 
  mutate_at(c("f0mean", "f0med", "f0sd", "turn", "turnOnset"), as.numeric) %>%
  mutate(qual = ifelse(grepl("D1|D2", file), "bad", "good"),
         task = substr(file, 5, 6),
         dyad = substr(file, 1, 3),
         f0sd = ifelse(f0sd == "NaN", NA, f0sd),
         firstSp = NA,
         prevf0mean = NA,
         prevf0med = NA,
         prevf0sd = NA) %>% 
  mutate_at(c("file", "speaker", "IPU"), as.factor) %>% 
  group_by(file, speaker, turn) %>%
  mutate(IPU = 1:n()) %>% 
  ungroup()

# determine who is the first speaker in each file
for(f in unique(dat$file)){
  dat$firstSp[dat$file==f] <- unique(as.character(dat$speaker[dat$file==f & dat$turnOnset == min(dat$turnOnset[dat$file==f])]))
}

dat <- dat %>% 
  group_by(file) %>% 
  mutate(prevTurn = ifelse(speaker == firstSp, turn-1, turn)) %>%
  mutate(prevTurn = ifelse(speaker == firstSp & turn == 1, NA, prevTurn)) %>%
  ungroup()

for(i in 1:nrow(dat)){
  prevf0mean <- dat$f0mean[dat$file == dat$file[i] &
                             dat$speaker != dat$speaker[i] &
                             dat$turn == dat$prevTurn[i] &
                             dat$IPU == dat$IPU[i]]
  prevf0med <- dat$f0med[dat$file == dat$file[i] &
                           dat$speaker != dat$speaker[i] &
                           dat$turn == dat$prevTurn[i] &
                           dat$IPU == dat$IPU[i]]
  prevf0sd <- dat$f0sd[dat$file == dat$file[i] &
                         dat$speaker != dat$speaker[i] &
                         dat$turn == dat$prevTurn[i] &
                         dat$IPU == dat$IPU[i]]
  if(!purrr::is_empty(prevf0mean)){
    if(!any(is.na(prevf0mean))){
      dat$prevf0mean[i] <- prevf0mean
    }
  }
  if(!purrr::is_empty(prevf0med)){
    if(!any(is.na(prevf0med))){
      dat$prevf0med[i] <- prevf0med
    }
  }
  if(!purrr::is_empty(prevf0sd)){
    if(!any(is.na(prevf0sd))){
      dat$prevf0sd[i] <- prevf0sd
    }
  }
}


dat <- dat %>% 
  mutate(prevf0meanC = prevf0mean - mean(prevf0mean, na.rm=TRUE),
         prevf0medC = prevf0med - mean(prevf0med, na.rm=TRUE),
         prevf0sdC = prevf0sd - mean(prevf0sd, na.rm=TRUE))

dat0 <- data.frame(matrix(nrow=0, ncol=5))
names(dat0) <- c("speaker", "task", "turn", "IPU", "overallTurn")

for(s in unique(dat$speaker)){
  l1 <- max(dat$turn[dat$speaker==s & dat$task=="L1"])
  l2 <- max(dat$turn[dat$speaker==s & dat$task=="L2"] + l1)
  l3 <- max(dat$turn[dat$speaker==s & dat$task=="L3"] + l2)
  d1 <- max(dat$turn[dat$speaker==s & dat$task=="D1"] + l3)
  current <- dat %>%
    filter(speaker==s) %>% 
    select(speaker, task, turn, IPU) %>% 
    mutate(overallTurn = case_when(
      task=="L1" ~ turn,
      task=="L2" ~ turn + l1,
      task=="L3" ~ turn + l2,
      task=="D1" ~ turn + l3,
      task=="D2" ~ turn + d1
    ))
  dat0 <- rbind(dat0, current)
}

dat <- merge(dat, dat0, by=c("speaker", "task", "turn", "IPU"))

############## f0 extraction complete.








ggplot(dat, aes(overallTurn, f0sd))+
  geom_point()+
  geom_smooth(method="lm")

summary(lmer(f0med ~ prevf0medC : overallTurn + (1|speaker), dat))


# ggplot(dat, aes(index, f0mean))+
#   geom_boxplot(aes(color=speaker))+
#   facet_wrap(~file, scales="free")
# 
# ggplot(dat, aes(speaker, f0mean))+
#   geom_boxplot(aes(color=qual))
# 
# summary(lm(f0mean ~ task, dat %>% filter(qual=="bad", dyad=="HAG", speaker=="A")))








# for(f in files){
#   if(grepl("AML|FWR", f)){cat <- "Bad"}else{cat <- "Good"}
#   t <- read.table(paste0(folderSpeech, f), header = TRUE)
#   t$index <- 1:nrow(t)
#   ggplot(t, aes(index, f0mean))+
#     geom_point()+
#     ggtitle(cat)
#   readline("Press Enter")
# }



























