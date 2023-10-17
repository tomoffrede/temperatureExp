# Process data and make it ready for analysis
# files needed in one folder:
# 1. TextGrids (with annotation of turns, IPUs ("silences") and overlaps)
# 2. txt files with f0 values

library(tidyverse)
library(tuneR)
library(rPraat)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/"
folderData <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/temperatureExp/data/"
folderAll <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/processed/"
folderTG <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/compare/"
folderSpeech <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/compare/"
`%!in%` <- Negate(`%in%`)

TXT <- list.files(folderAll, "\\.txt", recursive = TRUE)
TXT <- TXT[!grepl("Register", TXT)]
# TGv <- list.files(folderAll, "OverlapSp.TextGrid") # v for vector
TGv <- list.files(folderAll, "^[A-Z]{3}-(D|L)[0-9]\\.TextGrid$", recursive = TRUE) # v for vector

files <- data.frame(TXT) |> 
  mutate(TG = case_when(substr(TXT, 1, 10) %in% substr(TGv, 1, 10) ~ TGv[match(substr(TXT, 1, 10), substr(TGv, 1, 10))]))

f0 <- data.frame(matrix(nrow=0, ncol=14))
names(f0) <- c("file", "speaker", "turn", "IPU", "f0mean", "f0med", "f0sd", "f0max", "turnOnset", "turnOffset", "ipuOnset", "ipuOffset", "task", "firstSp")
f0 <- f0 |> 
  mutate_at(c("turn", "IPU", "f0mean", "f0med", "f0sd", "f0max", "turnOnset", "turnOffset", "ipuOnset", "ipuOffset"), as.numeric)

overlaps <- data.frame(matrix(nrow=0, ncol=5))
names(overlaps) <- c("file", "speaker", "overlap", "overlapOnset", "overlapOffset")

for(i in 1:nrow(files)){
  speaker <- paste0(substr(files$TXT[i], 5, 8), substr(files$TXT[i], 12, 12))
  file <- substr(files$TXT[i], 5, 10)
  sp <- ifelse(grepl("-A", files$TXT[i]), "A", "B")
  turnTier <- paste0("speaker", sp)
  silenceTier <- paste0("silence", sp)
  task <- ifelse(grepl("-L", files$TXT[i]), "Lists", ifelse(grepl("-D", files$TXT[i]), "Diapix", NA))
  overlapCount <- 0
  turnCount <- 0
  
  txt <- read.table(paste0(folderAll, files$TXT[i]), header = TRUE, na.strings = "--undefined--") |> 
    mutate(meanZ = (f0mean - mean(f0mean, na.rm=TRUE)) / sd(f0mean, na.rm=TRUE),
           medZ = (f0med - mean(f0med, na.rm=TRUE)) / sd(f0med, na.rm=TRUE),
           sdZ = (f0sd - mean(f0sd, na.rm=TRUE)) / sd(f0sd, na.rm=TRUE),
           maxZ = (f0max - mean(f0max, na.rm=TRUE)) / sd(f0max, na.rm=TRUE)) |> 
    mutate(f0mean = ifelse(abs(meanZ) > 2.5, NA, f0mean),
           f0med = ifelse(abs(medZ) > 2.5, NA, f0med),
           f0sd = ifelse(abs(sdZ) > 2.5, NA, f0sd),
           f0may = ifelse(abs(maxZ) > 2.5, NA, f0max))
  tg <- tg.read(paste0(folderAll, files$TG[i]), encoding=detectEncoding(paste0(folderAll, files$TG[i])))
  
  for(p in 1:tg.getNumberOfIntervals(tg, turnTier)){
    if(tg.getLabel(tg, turnTier, p) == "overlap"){
      overlapCount <- overlapCount + 1
      overlaps[nrow(overlaps)+1,] <- c(file,
                                       speaker,
                                       overlapCount,
                                       as.numeric(tg.getIntervalStartTime(tg, turnTier, p)),
                                       as.numeric(tg.getIntervalEndTime(tg, turnTier, p)))
    }
    
    if(tg.getLabel(tg, turnTier, p) == "s"){
      ipuCount <- 0
      if(turnCount == 0){
        turnCount <- turnCount + 1
      } else{
        if(any(f0$speaker==speaker & f0$file==file)){
          turnCount <- max(as.numeric(f0$turn[f0$file==file & f0$speaker==speaker])) + 1  
        } else{
          turnCount <- turnCount
        }
      }
      
      if(p > 2){ # make sure turnCount is right for turns with overlaps
        if(tg.getLabel(tg, turnTier, p-1) == "overlap"){
          if(tg.getLabel(tg, turnTier, p-2) == "s"){ # this means the turn had begun before the overlap
            if(any(f0$turnOnset == tg.getIntervalStartTime(tg, turnTier, p-2))){ # check if this previous "s" interval is an actual turn that was saved in the dataset (in at least 1 case it didn't because it was only a few ms without f0)
              if(any(f0$turnOffset == tg.getIntervalEndTime(tg, turnTier, p-2))){
                # turnCount is actually minus 1 (because we added +1 above, but the turn is still the same)
                turnCount <- turnCount - 1
                # ipuCount should also keep going from the running turn before the overlap
                ipuCount <- max(as.numeric(f0$IPU[f0$file==file & f0$speaker==speaker & f0$turn==turnCount])) # the last row is an overlap, so get the IPU count from before it
              }
            }
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
              f0[nrow(f0)+1,] <- c(file,
                                   speaker,
                                   turnCount,
                                   ipuCount,
                                   mean(f$f0mean, na.rm=TRUE),
                                   mean(f$f0med, na.rm=TRUE),
                                   mean(f$f0sd, na.rm=TRUE),
                                   mean(f$f0max, na.rm=TRUE),
                                   turnOnset, turnOffset, startIPU, endIPU,
                                   task,
                                   NA)
            }
            if(all(is.na(f))){
              # if there are no valid f0 values in the IPU, turn back IPU count
              ipuCount <- ipuCount - 1
            }
          }
        }
      }
      
      # if there are no rows containing f0 information for this current turn (e.g. it's a short turn with no sounded vocalizations)
      # add a blank row with only minimal turn info, so that the turn count over the file doesn't get thrown off (and it's important to keep it perfect for the calculation of prevf0)
      if(!any(f0$turn[f0$file==file & f0$speaker==speaker] == turnCount)){
        f0[nrow(f0)+1,] <- c(file,
                             speaker,
                             turnCount,
                             NA, NA, NA, NA, NA,
                             turnOnset, turnOffset,
                             NA, NA,
                             task,
                             NA)  
        
      }
    }
  }
}

# save(f0, file=gsub("AllForPreprocessing/", "f0-tentative.RData", folderAll))

f0save <- f0
# f0 <- f0save

dat <- f0 |> 
  mutate_at(c("f0mean", "f0med", "f0sd", "f0max", "turn", "IPU", "turnOnset", "turnOffset", "ipuOnset", "ipuOffset"), as.numeric) |>
  mutate_at(c("file", "speaker"), as.factor) |> 
  group_by(file, speaker) |>
  mutate(index = 1:n()) |> 
  group_by(file, speaker, turn) |>
  mutate(IPU = 1:n(),
         turnf0mean = mean(f0mean, na.rm=TRUE),
         turnf0med = mean(f0med, na.rm=TRUE),
         turnf0sd = mean(f0sd, na.rm=TRUE),
         turnf0max = mean(f0max, na.rm=TRUE),
         turnOnset = min(turnOnset), # correcting the turnOnsets and Offsets bc those of the turns with overlaps got saved as the intervals between overlaps, not start and end of actual turn
         turnOffset = max(turnOffset)) |> 
  ungroup() |> 
  mutate(turnDur = turnOffset - turnOnset,
         ipuDur = ipuOffset - ipuOnset,
         # qual = ifelse(grepl("D1|D2", file), "bad", "good"),
         order = case_when(task=="Lists" ~ 1, task=="Diapix" ~ 2),
         dyad = substr(file, 1, 3),
         f0mean = ifelse(is.nan(f0mean), NA, f0mean),
         f0med = ifelse(is.nan(f0med), NA, f0med),
         f0sd = ifelse(is.nan(f0sd), NA, f0sd),
         f0max = ifelse(is.nan(f0max), NA, f0max),
         turnf0mean = ifelse(is.nan(turnf0mean), NA, turnf0mean),
         turnf0med = ifelse(is.nan(turnf0med), NA, turnf0med),
         turnf0sd = ifelse(is.nan(turnf0sd), NA, turnf0sd),
         turnf0max = ifelse(is.nan(turnf0max), NA, turnf0max),
         firstSp = NA,
         prevf0mean = NA,
         prevf0med = NA,
         prevf0sd = NA,
         prevf0max = NA,
         prevTurnf0mean = NA,
         prevTurnf0med = NA,
         prevTurnf0sd = NA,
         prevTurnf0max = NA,
         mockPrevf0mean1 = NA,mockPrevf0mean2 = NA,mockPrevf0mean3 = NA,mockPrevf0mean4 = NA,mockPrevf0mean5= NA,mockPrevf0mean6 = NA,mockPrevf0mean7 = NA,mockPrevf0mean8 = NA,mockPrevf0mean9 = NA,mockPrevf0mean10 = NA,
         mockPrevf0med1 = NA,mockPrevf0med2 = NA,mockPrevf0med3 = NA,mockPrevf0med4 = NA,mockPrevf0med5 = NA,mockPrevf0med6 = NA,mockPrevf0med7 = NA,mockPrevf0med8 = NA,mockPrevf0med9 = NA,mockPrevf0med10 = NA,
         mockPrevf0sd1 = NA,mockPrevf0sd2 = NA,mockPrevf0sd3 = NA,mockPrevf0sd4 = NA,mockPrevf0sd5 = NA,mockPrevf0sd6 = NA,mockPrevf0sd7 = NA,mockPrevf0sd8 = NA,mockPrevf0sd9 = NA,mockPrevf0sd10 = NA,
         mockPrevf0max1 = NA,mockPrevf0max2 = NA,mockPrevf0max3 = NA,mockPrevf0max4 = NA,mockPrevf0max5 = NA,mockPrevf0max6 = NA,mockPrevf0max7 = NA,mockPrevf0max8 = NA,mockPrevf0max9 = NA,mockPrevf0max10 = NA)

# determine who is the first speaker in each file
for(f in unique(dat$file)){
  dat$firstSp[dat$file==f] <- unique(as.character(dat$speaker[dat$file==f & dat$turnOnset == min(dat$turnOnset[dat$file==f])]))
}

# save what each speaker's interlocutor's previous turn is (which will inform where previous f0 comes from)
dat <- dat |> 
  group_by(file) |> 
  mutate(prevTurn = ifelse(speaker == firstSp, turn-1, turn)) |>
  mutate(prevTurn = ifelse(speaker == firstSp & turn == 1, NA, prevTurn)) |>
  ungroup()

#############################
# d <- dat |>
#   filter(file=="KDA-D1") |>
#   select(file, speaker, turn, turnOnset, turnOffset, f0mean, f0med, f0max, f0sd) |>
#   arrange(turnOnset) |> 
#   filter(!duplicated(paste(speaker, turn)))

#############################

# save the f0 value of the previous turn (both of the last IPU and the average of the previous turn)
for(i in 1:nrow(dat)){
  # for(i in 859:nrow(dat)){
  if(dat$IPU[i] == 1){
    # get the last IPU of the previous turn
    prevLastIPU <- max(dat$IPU[dat$file == dat$file[i] &
                                 dat$speaker != dat$speaker[i] &
                                 dat$turn == dat$prevTurn[i]])
    
    # and get f0 values from that IPU
    prevf0mean <- dat$f0mean[dat$file == dat$file[i] &
                               dat$speaker != dat$speaker[i] &
                               dat$turn == dat$prevTurn[i] &
                               dat$IPU == prevLastIPU]
    prevf0med <- dat$f0med[dat$file == dat$file[i] &
                             dat$speaker != dat$speaker[i] &
                             dat$turn == dat$prevTurn[i] &
                             dat$IPU == prevLastIPU]
    prevf0sd <- dat$f0sd[dat$file == dat$file[i] &
                           dat$speaker != dat$speaker[i] &
                           dat$turn == dat$prevTurn[i] &
                           dat$IPU == prevLastIPU]
    prevf0max <- dat$f0max[dat$file == dat$file[i] &
                             dat$speaker != dat$speaker[i] &
                             dat$turn == dat$prevTurn[i] &
                             dat$IPU == prevLastIPU]
    
    # get f0 average over all of previous turn
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
}
datsave <- dat
# dat <- datsave

# get the difference between each target IPU and 10 random non-adjacent ones (from the same speaker)
for(s in unique(dat$speaker)){
  interlocutor <- ifelse(grepl("-A", s),
                         gsub("-A", "-B", s),
                         gsub("-B", "-A", s))
  for(f in unique(dat$file[dat$speaker==s])){
    ipusMean <- dat$f0mean[dat$file==f & dat$speaker==interlocutor & !is.na(dat$f0mean)]
    ipusMed <- dat$f0med[dat$file==f & dat$speaker==interlocutor & !is.na(dat$f0med)]
    ipusSD <- dat$f0sd[dat$file==f & dat$speaker==interlocutor & !is.na(dat$f0sd)]
    ipusMax <- dat$f0max[dat$file==f & dat$speaker==interlocutor & !is.na(dat$f0max)]
    
    for(i in 1:nrow(dat)){
      if(dat$speaker[i]==s){
        if(dat$file[i]==f){
          if(dat$IPU[i]==1){
            lengthMean <- ifelse(length(ipusMean[ipusMean %!in% dat$prevf0mean[i]]) >= 10, 10, length(ipusMean[ipusMean %!in% dat$prevf0mean[i]])-1)
            ipusMeanCurrent <- sample(ipusMean[ipusMean %!in% dat$prevf0mean[i]], lengthMean, replace=FALSE)
            dat[i, c(paste0("mockPrevf0mean", 1:lengthMean))] <- as.list(abs(dat$f0mean[i] - ipusMeanCurrent))
            
            lengthMed <- ifelse(length(ipusMed[ipusMed %!in% dat$prevf0med[i]]) >= 10, 10, length(ipusMed[ipusMed %!in% dat$prevf0med[i]])-1)
            ipusMedCurrent <- sample(ipusMed[ipusMed %!in% dat$prevf0med[i]], lengthMed, replace=FALSE)
            dat[i, c(paste0("mockPrevf0med", 1:lengthMed))] <- as.list(abs(dat$f0med[i] - ipusMedCurrent))

            lengthSD <- ifelse(length(ipusSD[ipusSD %!in% dat$prevf0sd[i]]) >= 10, 10, length(ipusSD[ipusSD %!in% dat$prevf0sd[i]])-1)
            ipusSDCurrent <- sample(ipusSD[ipusSD %!in% dat$prevf0sd[i]], lengthSD, replace=FALSE)
            dat[i, c(paste0("mockPrevf0sd", 1:lengthSD))] <- as.list(abs(dat$f0sd[i] - ipusSDCurrent))
            
            lengthMax <- ifelse(length(ipusMax[ipusMax %!in% dat$prevf0max[i]]) >= 10, 10, length(ipusMax[ipusMax %!in% dat$prevf0max[i]])-1)
            ipusMaxCurrent <- sample(ipusMax[ipusMax %!in% dat$prevf0max[i]], lengthMax, replace=FALSE)
            dat[i, c(paste0("mockPrevf0max", 1:lengthMax))] <- as.list(abs(dat$f0max[i] - ipusMaxCurrent))
          }
        }
      }
    }
  }
}

dat <- dat |> 
  filter(IPU == 1) |> 
  group_by(speaker) |> 
  mutate(f0meanDiff = abs(f0mean - prevf0mean),
         f0medDiff = abs(f0med - prevf0med),
         f0sdDiff = abs(f0sd - prevf0sd),
         f0fmaxDiff = abs(f0max - prevf0max),
         # prevf0meanC = prevf0mean - mean(prevf0mean, na.rm=TRUE), # I don't think we'll end up using these columns (they'd be used for a regression, but we're not doing that anymore)
         # prevf0medC = prevf0med - mean(prevf0med, na.rm=TRUE), # so I commented them out
         # prevf0sdC = prevf0sd - mean(prevf0sd, na.rm=TRUE),
         # prevf0maxC = prevf0max - mean(prevf0max, na.rm=TRUE),
         # prevTurnf0meanC = prevTurnf0mean - mean(prevTurnf0mean, na.rm=TRUE),
         # prevTurnf0medC = prevTurnf0med - mean(prevTurnf0med, na.rm=TRUE),
         # prevTurnf0sdC = prevTurnf0sd - mean(prevTurnf0sd, na.rm=TRUE),
         # prevTurnf0maxC = prevTurnf0max - mean(prevTurnf0max, na.rm=TRUE)
         orderFile = case_when(
           grepl("-L1", file) ~ 1,
           grepl("-L2", file) ~ 2,
           grepl("-L3", file) ~ 3,
           grepl("-D1", file) ~ 4,
           grepl("-D2", file) ~ 5,
         )) |>
  arrange(orderFile, turn, .by_group = TRUE) |> # if we end up deciding to keep multiple IPUs per turn, we need to find another way to create `turnOverall`, because this only works with one IPU per turn
  mutate(turnOverall = 1:n()) |> 
  ungroup()

save(dat, file=paste0(folderData, "speechData-noMetadata.RData"))

############## f0 extraction complete

# join metadata

load(paste0(folderData, "metadata-clean.RData"))

dat0 <- dat # in case we want to check it before it gets merged

dat <- merge(dat, m, by="speaker")

save(dat, file=paste0(folderData, "speechData.RData"))
