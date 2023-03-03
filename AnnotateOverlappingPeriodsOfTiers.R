# Tom Offrede
# compare the labels of 2 tiers; if there are periods of overlap between two identically labeled intervals, annotate those overlapping intervals into an additional tier
# ASSUMPTIONS: this code assumes you have TextGrids with at least two tiers, one named "speakerA" and one "speakerB", and each tier has intervals labeled "s" for the moments in which speaker A/B is speaking.
# if you have different names for the tiers or interval labels, change script accordingly

library(tidyverse)
library(rPraat)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/HAG/compare/"

files <- list.files(folder, "\\.TextGrid")
files <- files[!grepl("OG|sil", files)]

intervals <- data.frame(matrix(nrow=0, ncol=6))
names(intervals) <- c("file", "speaker", "count", "label", "onset", "offset")

for(f in files){
  tg <- tg.read(paste0(folder, f), encoding = detectEncoding(paste0(folder, f)))
  
  for(tierSpeaker in c("speakerA", "speakerB")){
    intervalCount <- 0
    for(i in 1:tg.getNumberOfIntervals(tg, tierSpeaker)){
      intervalCount <- intervalCount + 1
      intervals[nrow(intervals)+1,] <- c(gsub(".TextGrid", "", f),
                                         tierSpeaker,
                                         intervalCount,
                                         tg.getLabel(tg, tierSpeaker, i),
                                         tg.getIntervalStartTime(tg, tierSpeaker, i),
                                         tg.getIntervalEndTime(tg, tierSpeaker, i))
    }
  }
}

table(intervals$label) # check if there's anything that isn't "s" or ""

int <- intervals %>% 
  filter(label == "s") %>% 
  select(-label) %>% 
  group_by(file, speaker) %>% 
  mutate(count = 1:n()) %>% 
  ungroup()

ov <- data.frame(matrix(nrow=0, ncol=8))
names(ov) <- c("file", "speaker1", "count1", "speaker2", "count2", "onsetOV", "offsetOV", "typeOverlap")

# types of overlap:
# type 1: one's turn is contained within the other's turn
# type 2: one's turn (turn 2) starts before the other's turn (turn 1) ends and continues on after turn 1 ends

for(f in unique(int$file)){
  dat <- int %>% 
    filter(file == f)
  
  for(a in 1:nrow(dat)){
    for(b in 1:nrow(dat)){
      if(dat$speaker[a] != dat$speaker[b]){
        if(as.numeric(dat$onset[b]) > as.numeric(dat$onset[a])){
          if(as.numeric(dat$offset[b]) < as.numeric(dat$offset[a])){
            ov[nrow(ov)+1,] <- c(f,
                                 dat$speaker[a], # speaker that contains the other's turn
                                 dat$count[a],
                                 dat$speaker[b], # speaker whose turn is contained within the other's turn
                                 dat$count[b],
                                 dat$onset[b],
                                 dat$offset[b],
                                 "type1")
          }
        }
        if(as.numeric(dat$onset[b]) > as.numeric(dat$onset[a])){
          if(as.numeric(dat$onset[b]) < as.numeric(dat$offset[a])){
            if(as.numeric(dat$offset[b]) > as.numeric(dat$offset[a])){
              ov[nrow(ov)+1,] <- c(f,
                                   dat$speaker[a], # speaker that contains the other's turn
                                   dat$count[a],
                                   dat$speaker[b], # speaker whose turn is contained within the other's turn
                                   dat$count[b],
                                   dat$onset[b],
                                   dat$offset[a],
                                   "type2")
            }
          }
        }
      }
    }
  }
}

# Do the following for all files, even if there is no overlap between speakers in a given file, so that all files get the same number of tiers and name.

for(f in files){
  tg0 <- tg.read(paste0(folder, f), encoding=detectEncoding(paste0(folder, f)))
  tg <- tg.insertNewIntervalTier(tg0, newInd = Inf, "overlap", tMin = tg.getStartTime(tg0), tMax = tg.getEndTime(tg0))
  dat <- ov %>% 
    filter(file == gsub(".TextGrid", "", f))
  
  if(nrow(dat) > 0){
    for(i in 1:nrow(dat)){
      tg <- tg.insertInterval(tg, "overlap", tStart = as.numeric(dat$onsetOV[i]), tEnd = as.numeric(dat$offsetOV[i]), label="overlap")
    } 
  }
  
  file <- gsub(".TextGrid", "-Overlap.TextGrid", f)
  tg.write(tg, paste0(folder, file))
}

###########

# Optional additional step:
# In the original `speaker` tiers, transform any overlap period into a blank interval

files <- list.files(folder, "Overlap.TextGrid")

for(f in files){
  tg <- tg.read(paste0(folder, f), encoding=detectEncoding(paste0(folder, f)))
  
  for(i in 1:tg.getNumberOfIntervals(tg, "overlap")){
    if(tg.getLabel(tg, "overlap", i) == "overlap"){
      leftBound <- as.numeric(tg.getIntervalStartTime(tg, "overlap", i))
      rightBound <- as.numeric(tg.getIntervalEndTime(tg, "overlap", i))
      for(speakerTier in c("speakerA", "speakerB", "silenceA", "silenceB")){
        label <- ifelse(grepl("silence", speakerTier), "sounding", "s")
        # following 4 if statements: to make sure the "overlap" boundary isn't the same as the boundary that already exists in the speakerTier
        if(leftBound != tg.getIntervalStartTime(tg, speakerTier, tg.getIntervalIndexAtTime(tg, speakerTier, leftBound))){
          if(leftBound != tg.getIntervalEndTime(tg, speakerTier, tg.getIntervalIndexAtTime(tg, speakerTier, leftBound))){
            tg <- tg.insertBoundary(tg, speakerTier, time=leftBound, label=label)
          }
        }
        if(rightBound != tg.getIntervalStartTime(tg, speakerTier, tg.getIntervalIndexAtTime(tg, speakerTier, leftBound))){
          if(rightBound != tg.getIntervalEndTime(tg, speakerTier, tg.getIntervalIndexAtTime(tg, speakerTier, leftBound))){
            tg <- tg.insertBoundary(tg, speakerTier, time=rightBound, label=label)
          }
        }
      }
    }
    }
  
  for(speakerTier in c("speakerA", "speakerB", "silenceA", "silenceB")){
    for(s in 1:tg.getNumberOfIntervals(tg, speakerTier)){
      for(o in 1:tg.getNumberOfIntervals(tg, "overlap")){
        if(tg.getLabel(tg, "overlap", o) == "overlap"){
          if(tg.getIntervalStartTime(tg, speakerTier, s) == tg.getIntervalStartTime(tg, "overlap", o)){
            if(tg.getIntervalEndTime(tg, speakerTier, s) == tg.getIntervalEndTime(tg, "overlap", o)){
              tg <- tg.setLabel(tg, speakerTier, s, "overlap")
            }
          }
        }
      }
    }
  }
  file <- gsub("Overlap", "OverlapSp", f)
  tg.write(tg, paste0(folder, file))
}
