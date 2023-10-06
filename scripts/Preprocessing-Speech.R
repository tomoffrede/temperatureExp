# Process data and make it ready for analysis
# files needed in one folder:
# 1. TextGrids (with annotation of turns, IPUs ("silences") and overlaps)
# 2. txt files with f0 values

library(tidyverse)
library(tuneR)
library(rPraat)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/"
folderData <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/temperatureExp/data/"
folderAll <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/AllForPreprocessing/"
folderTG <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/compare/"
folderSpeech <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/compare/"
`%!in%` <- Negate(`%in%`)

TXT <- list.files(folderAll, "\\.txt")
TXT <- TXT[!grepl("Register", TXT)]
# TGv <- list.files(folderAll, "OverlapSp.TextGrid") # v for vector
TGv <- list.files(folderAll, "^[A-Z]{3}-(D|L)[0-9]\\.TextGrid$") # v for vector

files <- data.frame(TXT) %>% 
  mutate(TG = case_when(substr(TXT, 1, 6) %in% substr(TGv, 1, 6) ~ TGv[match(substr(TXT, 1, 6), substr(TGv, 1, 6))]))

f0 <- data.frame(matrix(nrow=0, ncol=15))
names(f0) <- c("file", "speaker", "turn", "IPU", "overlap", "f0mean", "f0med", "f0sd", "f0max", "turnOnset", "turnOffset", "ipuOnset", "ipuOffset", "overlapOnset", "overlapOffset")

for(i in 1:nrow(files)){
  speaker <- paste0(substr(files$TXT[i], 1, 4), substr(files$TXT[i], 8, 8))
  sp <- ifelse(grepl("-A", files$TXT[i]), "A", "B")
  turnTier <- paste0("speaker", sp)
  silenceTier <- paste0("silence", sp)
  turnCount <- 0
  overlapCount <- 0
  
  txt <- read.table(paste0(folderAll, files$TXT[i]), header = TRUE, na.strings = "--undefined--") %>% 
    mutate(meanZ = (f0mean - mean(f0mean, na.rm=TRUE)) / sd(f0mean, na.rm=TRUE),
           medZ = (f0med - mean(f0med, na.rm=TRUE)) / sd(f0med, na.rm=TRUE),
           sdZ = (f0sd - mean(f0sd, na.rm=TRUE)) / sd(f0sd, na.rm=TRUE),
           maxZ = (f0max - mean(f0max, na.rm=TRUE)) / sd(f0max, na.rm=TRUE)) %>% 
    mutate(f0mean = ifelse(abs(meanZ) > 2.5, NA, f0mean),
           f0med = ifelse(abs(medZ) > 2.5, NA, f0med),
           f0sd = ifelse(abs(sdZ) > 2.5, NA, f0sd),
           f0may = ifelse(abs(maxZ) > 2.5, NA, f0max))
  tg <- tg.read(paste0(folderAll, files$TG[i]), encoding=detectEncoding(paste0(folderAll, files$TG[i])))
  
  for(p in 1:tg.getNumberOfIntervals(tg, turnTier)){
    if(tg.getLabel(tg, turnTier, p) == "overlap"){
      overlapCount <- overlapCount + 1
      f0[nrow(f0)+1,] <- c(substr(files$TXT[i], 1, 6),
                           speaker,
                           NA, NA,
                           overlapCount,
                           NA, NA, NA, NA, NA, NA, NA, NA,
                           as.numeric(tg.getIntervalStartTime(tg, turnTier, p)),
                           as.numeric(tg.getIntervalEndTime(tg, turnTier, p)))
    }
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
            f <- data.frame(matrix(nrow=0, ncol=4))
            names(f) <- c("f0mean", "f0med", "f0sd", "f0max")
            for(l in 1:nrow(txt)){
              if(txt$onset[l] >= startIPU - 0.001){ # adding a 0.001 window because the txt file rounds up the onset and offset times
                if(txt$offset[l] <= endIPU + 0.001){
                  f[nrow(f)+1,] <- c(as.numeric(txt$f0mean[l]),
                                     as.numeric(txt$f0med[l]),
                                     as.numeric(txt$f0sd[l]),
                                     as.numeric(txt$f0max[l]))
                }
              }
            }
            if(any(!is.na(f))){
              f0[nrow(f0)+1,] <- c(substr(files$TXT[i], 1, 6),
                                   speaker,
                                   turnCount,
                                   ipuCount,
                                   NA,
                                   mean(f$f0mean, na.rm=TRUE),
                                   mean(f$f0med, na.rm=TRUE),
                                   mean(f$f0sd, na.rm=TRUE),
                                   mean(f$f0max, na.rm=TRUE),
                                   turnOnset, turnOffset, startIPU, endIPU,
                                   NA, NA)
            }
            if(all(is.na(f))){ipuCount <- ipuCount - 1}
          }
        }
      }
    }
  }
}

# save(f0, file=gsub("AllForPreprocessing/", "f0-tentative.RData", folderAll))

f0save <- f0

overlaps <- f0 %>% 
  filter(!is.na(overlap)) %>% 
  select(file, speaker, overlap, overlapOnset, overlapOffset) %>% 
  mutate_at(c("overlapOnset", "overlapOffset"), as.numeric) %>%
  mutate(duration = overlapOffset - overlapOnset)

dat <- f0 %>% 
  filter(is.na(overlap)) %>% 
  select(!c(overlap, overlapOnset, overlapOffset)) %>% 
  mutate_at(c("f0mean", "f0med", "f0sd", "f0max", "turn", "IPU", "turnOnset", "turnOffset", "ipuOnset", "ipuOffset"), as.numeric) %>%
  mutate_at(c("file", "speaker"), as.factor) %>% 
  group_by(file, speaker) %>%
  mutate(index = 1:n()) %>% 
  group_by(file, speaker, turn) %>%
  mutate(IPU = 1:n(),
         turnf0mean = mean(f0mean, na.rm=TRUE),
         turnf0med = mean(f0med, na.rm=TRUE),
         turnf0sd = mean(f0sd, na.rm=TRUE),
         turnf0max = mean(f0max, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(turnDur = turnOffset - turnOnset,
         ipuDur = ipuOffset - ipuOnset,
         # qual = ifelse(grepl("D1|D2", file), "bad", "good"),
         task = case_when(grepl("-L", file) ~ "Lists", grepl("-D", file) ~ "Diapix"),
         order = ifelse(grepl("L1", task), 1, ifelse(grepl("L2", task), 2, ifelse(grepl("L3", task), 3, ifelse(grepl("D1", task), 4, 5)))),
         dyad = substr(file, 1, 3),
         f0mean = ifelse(is.nan(f0mean), NA, f0mean),
         f0med = ifelse(is.nan(f0med), NA, f0med),
         f0sd = ifelse(is.nan(f0sd), NA, f0sd),
         f0max = ifelse(is.nan(f0max), NA, f0max),
         turnf0mean = ifelse(is.nan(turnf0mean), NA, turnf0mean),
         turnf0med = ifelse(is.nan(turnf0med), NA, turnf0med),
         turnf0sd = ifelse(is.nan(turnf0sd), NA, turnf0sd),
         firstSp = NA,
         prevf0mean = NA,
         prevf0med = NA,
         prevf0sd = NA,
         prevf0max = NA,
         prevTurnf0mean = NA,
         prevTurnf0med = NA,
         prevTurnf0sd = NA,
         prevTurnf0max = NA)

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
  prevf0max <- dat$f0max[dat$file == dat$file[i] &
                           dat$speaker != dat$speaker[i] &
                           dat$turn == dat$prevTurn[i] &
                           dat$IPU == dat$IPU[i]]
  prevTurnf0mean <- unique(dat$turnf0mean[dat$file == dat$file[i] &
                                            dat$speaker != dat$speaker[i] &
                                            dat$turn == dat$prevTurn[i]])
  prevTurnf0med <- unique(dat$turnf0med[dat$file == dat$file[i] &
                                          dat$speaker != dat$speaker[i] &
                                          dat$turn == dat$prevTurn[i]])
  prevTurnf0sd <- unique(dat$turnf0sd[dat$file == dat$file[i] &
                                        dat$speaker != dat$speaker[i] &
                                        dat$turn == dat$prevTurn[i]])
  prevTurnf0max <- unique(dat$turnf0max[dat$file == dat$file[i] &
                                          dat$speaker != dat$speaker[i] &
                                          dat$turn == dat$prevTurn[i]])
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
  if(!purrr::is_empty(prevf0max)){
    if(!any(is.na(prevf0max))){
      dat$prevf0max[i] <- prevf0max
    }
  }
  if(!purrr::is_empty(prevTurnf0mean)){
    if(!any(is.na(prevTurnf0mean))){
      dat$prevTurnf0mean[i] <- prevTurnf0mean
    }
  }
  if(!purrr::is_empty(prevTurnf0med)){
    if(!any(is.na(prevTurnf0med))){
      dat$prevTurnf0med[i] <- prevTurnf0med
    }
  }
  if(!purrr::is_empty(prevTurnf0sd)){
    if(!any(is.na(prevTurnf0sd))){
      dat$prevTurnf0sd[i] <- prevTurnf0sd
    }
  }
  if(!purrr::is_empty(prevTurnf0max)){
    if(!any(is.na(prevTurnf0max))){
      dat$prevTurnf0max[i] <- prevTurnf0max
    }
  }
}

# dat0 <- data.frame(matrix(nrow=0, ncol=5))
# names(dat0) <- c("speaker", "task", "turn", "IPU", "overallTurn")
# 
# for(s in unique(dat$speaker)){ # the warnings received (no non-missing arguments to max) are because FWR's D1 files and MJG's L3 files are missing
#   l1 <- max(dat$turn[dat$speaker==s & dat$task=="L1"])
#   l2 <- max(dat$turn[dat$speaker==s & dat$task=="L2"] + l1)
#   l3 <- max(dat$turn[dat$speaker==s & dat$task=="L3"] + l2)
#   d1 <- max(dat$turn[dat$speaker==s & dat$task=="D1"] + l3)
#   current <- dat %>%
#     filter(speaker==s) %>% 
#     select(speaker, task, turn, IPU) %>% 
#     mutate(overallTurn = case_when(
#       task=="L1" ~ turn,
#       task=="L2" ~ turn + l1,
#       task=="L3" ~ turn + l2,
#       task=="D1" ~ turn + l3,
#       task=="D2" ~ turn + d1
#     ))
#   dat0 <- rbind(dat0, current)
# }
# 
# dat <- merge(dat, dat0, by=c("speaker", "task", "turn", "IPU"))

dat <- dat %>% 
  group_by(speaker) %>% 
  mutate(prevf0meanC = prevf0mean - mean(prevf0mean, na.rm=TRUE),
         prevf0medC = prevf0med - mean(prevf0med, na.rm=TRUE),
         prevf0sdC = prevf0sd - mean(prevf0sd, na.rm=TRUE),
         prevTurnf0meanC = prevTurnf0mean - mean(prevTurnf0mean, na.rm=TRUE),
         prevTurnf0medC = prevTurnf0med - mean(prevTurnf0med, na.rm=TRUE),
         prevTurnf0sdC = prevTurnf0sd - mean(prevTurnf0sd, na.rm=TRUE)) %>% 
  ungroup()

save(dat, file=gsub("AllForPreprocessing/", "data-noMetadata.RData", folderAll))

############## f0 extraction complete

# clean and join metadata

m <- read.csv(paste0(folderAll, "metadata.csv"))
names(m) <- tolower(names(m))
m <- m %>% 
  rename(speaker = participant,
         L1 = l1,
         preAcquaintance = preacquaintance,
         comfortPre = comfortpre,
         comfortPost = comfortpost,
         tempPre = temppre,
         tempPost = temppost,
         realTempPre = realtemppre,
         realTempDuring = realtempduring,
         realTempPost = realtemppost) %>% 
  mutate(condition = case_when(
    condition == "exp" ~ "close",
    condition == "con" ~ "impersonal",
  ))

f <- read.csv(paste0(folder, "bfi-factors.csv"), sep=";", na.strings = "") %>% 
  filter(!is.na(factor)) %>% 
  mutate(item = paste0("bfi", item))
i <- m %>% 
  select_if(grepl("bfi|speaker", names(.))) %>% 
  select_if(!grepl("5|32|37", names(.))) %>% # in the paper of the Italian BFI (Fossati et al 2011) they left out those items from the PCA table without mentioning anything, which I find shady
  pivot_longer(cols=c("bfi1":"bfi44"),
               names_to="item",
               values_to="rating") %>% 
  mutate(rating = ifelse(rating==7, 5, rating))
bfi <- merge(i, f, by="item") %>% 
  mutate(rating = case_when(reversed == "R" ~ 6 - rating,
                            is.na(reversed) ~ rating)) %>% 
  group_by(speaker, factor) %>% 
  mutate(score = mean(rating, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(factor = case_when(
    factor == "A" ~ "agreeableness",
    factor == "C" ~ "conscientiousness",
    factor == "E" ~ "extraversion",
    factor == "N" ~ "neuroticism",
    factor == "O" ~ "openness",
  )) %>% 
  select(-c(item, rating, reversed)) %>% 
  distinct() %>%
  pivot_wider(names_from = factor,
              values_from = score)

m <- merge(m, bfi, by="speaker") %>% 
  select_if(!grepl("bfi", names(.))) %>% 
  rename(privacy = rcit1, # Pensi di aver avuto un'adeguata privacy nella tua conversazione?
         atEase = rcit2, # Ti sei sentito relativamente a tuo agio in questo ambiente di conversazione?
         convMeet = rcit3, # Consideri la conversazione un buon modo per conoscere qualcuno?
         convHabit = rcit4, # Ti impegni spesso in conversazioni simili a quella in cui ti sei appena impegnato?
         friendsQuestions = rcit5, # Pensi che la maggior parte dei tuoi amici farebbe domande simili a quelle poste in questa conversazione?
         convFriends = rcit6, # Pensi che i tuoi amici considerino la conversazione il modo più importante per conoscere qualcuno?
         closeness = rcit7, # Quanto ti senti legato al partecipante con cui stai lavorando a questo studio?
         similarity = rcit8, # Quanto ti senti simile al partecipante con cui stai lavorando a questo studio?
         likeability = rcit9, # Quanto ti piace il partecipante con cui stai lavorando a questo studio?
         becomeFriends = rcit10) # In futuro, in che misura ritieni di poter essere amico del partecipante con cui stai lavorando a questo studio?

save(m, file=paste0(folderData, "metadata-clean.RData"))

dat0 <- dat # in case we want to check it before it gets merged

dat <- merge(dat, m, by="speaker")
save(dat, file=gsub("AllForPreprocessing/", "data.RData", folderAll))

# Create a dataset that has only the f0 information from beginning and ending stages of 
