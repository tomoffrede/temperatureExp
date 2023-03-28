# Tom Offrede
# Rename tiers from textgrids

library(rPraat)
library(tidyverse)

# folderTG <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/All-Overlap/"
folderTG <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/AllTextGrids-manualAnnotationOnly/"

# Rename tiers depending on their name

TGv <- list.files(folderTG, "\\.TextGrid") # v for vector

for(t in TGv){
  tg <- tg.read(paste0(folderTG, t), encoding=detectEncoding(paste0(folderTG, t)))
  if(tg.getTierName(tg, 1) == "speaker1"){
    tg <- tg.setTierName(tg, 1, "speakerA")
  }
  if(tg.getTierName(tg, 2) == "speaker2"){
    tg <- tg.setTierName(tg, 2, "speakerB")
  }
  tg.write(tg, paste0(folderTG, t))
}

# for(t in TGv){
#   tg <- tg.read(paste0(folderTG, t), encoding=detectEncoding(paste0(folderTG, t)))
#   if(grepl("-A-", t)){
#     tg <- tg.setTierName(tg, 1, "silenceA")
#   }
#   if(grepl("-B-", t)){
#     tg <- tg.setTierName(tg, 1, "silenceB")
#   }
#   tg.write(tg, paste0(folderTG, t))
# }

# See if they all have the right name now

tiers <- data.frame(matrix(nrow=0, ncol=3))
names(tiers) <- c("file", "tier1", "tier2")

for(t in TGv){
  tg <- tg.read(paste0(folderTG, t), encoding=detectEncoding(paste0(folderTG, t)))
  tiers[nrow(tiers)+1,] <- c(t,
                             tg.getTierName(tg, 1),
                             tg.getTierName(tg, 2))
}

table(paste0(tiers$tier1, "-", tiers$tier2))

# See if all intervals are properly named (i.e. with no additional characters, blank spaces etc)

lab <- data.frame(matrix(nrow=0, ncol=6))
names(lab) <- c("file", "tier", "interval", "onset", "offset", "label")

for(f in TGv){
  tg <- tg.read(paste0(folderTG, f), encoding=detectEncoding(paste0(folderTG, f)))
  
  for(tier in c("speakerA", "speakerB")){
    for(i in 1:tg.getNumberOfIntervals(tg, tier)){
      lab[nrow(lab)+1,] <- c(f,
                             tier,
                             i,
                             tg.getIntervalStartTime(tg, tier, i),
                             tg.getIntervalEndTime(tg, tier, i),
                             tg.getLabel(tg, tier, i))
    }
  }
}

table(lab$label)
lab %>% 
  filter(label != "s" & label!="")

for(f in TGv){
  tg <- tg.read(paste0(folderTG, f), encoding=detectEncoding(paste0(folderTG, f)))
  
  for(tier in c("speakerA", "speakerB")){
    for(i in 1:tg.getNumberOfIntervals(tg, tier)){
      if(grepl("s", tg.getLabel(tg, tier, i))){
        if(tg.getLabel(tg, tier, i) != "s"){
          tg <- tg.setLabel(tg, tier, i, "s")
        }
      }
    }
  }
  tg.write(tg, paste0(folderTG, f))
}

# See if gaps between turns (manually annotated) aren't shorter than 150ms

# gaps <- data.frame(matrix(nrow=0, ncol=6))
# names(gaps) <- c("file", "tier", "intervalNo", "onset", "offset", "duration")

for(f in TGv){
  tg <- tg.read(paste0(folderTG, f), encoding=detectEncoding(paste0(folderTG, f)))
  
  for(tier in c("speakerA", "speakerB")){
    for(i in 2:(tg.getNumberOfIntervals(tg, tier)-1)){
      if(i > (tg.getNumberOfIntervals(tg, tier)-1)){ # adding this because the number of intervals changes during the loop (as boundaries are removed)
        next
      }
      if(tg.getLabel(tg, tier, i) == ""){
        if(tg.getLabel(tg, tier, i-1) == "s"){
          if(tg.getLabel(tg, tier, i+1) == "s"){
            if(as.numeric(tg.getIntervalDuration(tg, tier, i)) < 0.15){
              # tg <- tg.removeIntervalBothBoundaries(tg, tier, i)
              gaps[nrow(gaps)+1,] <- c(f, tier, i, tg.getIntervalStartTime(tg, tier, i), tg.getIntervalEndTime(tg, tier, i), tg.getIntervalDuration(tg, tier, i))
            }
          }
        }
      }
    }
  }
  
  for(tier in c("speakerA", "speakerB")){
    for(i in 1:(tg.getNumberOfIntervals(tg, tier))){
      if(tg.getLabel(tg, tier, i) == "ss"){
        tg <- tg.setLabel(tg, tier, i, "s")
      }
    }
  }
  
  # tg.write(tg, paste0(folderTG, f))
}

# short <- gaps %>% 
#   filter(duration <= 0.15)

# Check that A’s and B’s turns are always after each other (no turnB then turnB again before a turnA)
# (do this on files where overlaps were already annotated)

turns <- data.frame(matrix(nrow=0, ncol=6))
names(turns) <- c("file", "tier", "intervalNo", "label", "onset", "offset")

for(f in TGv){
  tg <- tg.read(paste0(folderTG, f), encoding=detectEncoding(paste0(folderTG, f)))
  
  for(tier in c("speakerA", "speakerB")){
    for(i in 1:tg.getNumberOfIntervals(tg, tier)){
      turns[nrow(turns)+1,] <- c(f, tier, i, tg.getLabel(tg, tier, i), tg.getIntervalStartTime(tg, tier, i), tg.getIntervalEndTime(tg, tier, i))
    }
  }
}

sorted <- turns %>% 
  filter(label == "s") %>%
  mutate_at(c("intervalNo", "onset", "offset"), as.numeric) %>% 
  group_by(file) %>% 
  arrange(onset, offset, .by_group=TRUE) %>% 
  ungroup() %>% 
  mutate(rep = NA)

for(f in unique(sorted$file)){
  tg <- tg.read(paste0(folderTG, f), encoding=detectEncoding(paste0(folderTG, f)))
  s <- sorted %>%
    filter(file == f)
  
  for(i in 3:nrow(s)){
    if(s$tier[i] == s$tier[i-1] | s$tier[i] == s$tier[i-2]){
      currentTier <- s$tier[i]
      currentOnset <- as.numeric(tg.getIntervalStartTime(tg, s$tier[i], s$intervalNo[i]))
      offset1 <- as.numeric(tg.getIntervalEndTime(tg, s$tier[i-1], s$intervalNo[i-1]))
      offset2 <- as.numeric(tg.getIntervalEndTime(tg, s$tier[i-2], s$intervalNo[i-2]))
      if(s$tier[i-1] == currentTier){sameSpOffset <- offset1} else{otherSpOffset <- offset1}
      if(s$tier[i-2] == currentTier){sameSpOffset <- offset2} else{otherSpOffset <- offset2}
      
      if((currentOnset - sameSpOffset) < (currentOnset - otherSpOffset)){
        s$rep[i] <- "repeatedTurn"
      } else{next}
    }
  }
  
  sorted <- full_join(sorted, s, by=c("file", "tier", "intervalNo", "label", "onset", "offset", "rep"))
}

sorted <- sorted %>% 
  filter(rep == "repeatedTurn")

# remove gaps between turn intervals that are consecutively of the same speaker

for(f in unique(sorted$file)){
  tg <- tg.read(paste0(folderTG, f), encoding=detectEncoding(paste0(folderTG, f)))
  s <- sorted %>% 
    filter(file == f)
  
  for(r in 1:nrow(s)){
    for(i in 1:tg.getNumberOfIntervals(tg, s$tier[r])){
      if(i > (tg.getNumberOfIntervals(tg, s$tier[r]))){
        next
      }
      if(tg.getIntervalStartTime(tg, s$tier[r], i) == s$onset[r]){
        if(tg.getIntervalEndTime(tg, s$tier[r], i) == s$offset[r]){
          tg <- tg.removeIntervalBothBoundaries(tg, s$tier[r], i-1)
        }
      }
    }
  }
  
  tg.write(tg, paste0(folderTG, f))
}

###############################################################

# Copy each TextGrid twice into the same folder, each time taking the name of the corresponding wav file + "_VUV" (there are 2 wav files per TextGrid)
# In the VUV textgrids, leave only the "silenceX" tier

# tg <- list.files(folder, "^[A-Z]{3}-(D|L)[0-9]\\.TextGrid$")
tg <- list.files(folder, "f0\\.TextGrid$")
w <- list.files(folder, "wav")
wA <- w[grepl("-A", w)]
wB <- w[grepl("-B", w)]

files0 <- cbind(wA, wB)
files <- data.frame(cbind(files0, tg)) %>%  
  mutate(worked1 = ifelse(substr(wA, 1, 6) == substr(wB, 1, 6), "worked!", "NO!!!"),
         worked2 = ifelse(substr(wA, 1, 6) == substr(tg, 1, 6), "worked!", "NO!!!"))
table(files$worked1)
table(files$worked2)

for(i in 1:nrow(files)){
  tgA <- tg.read(paste0(folder, files$tg[i]), encoding=detectEncoding(paste0(folder, files$tg[i])))
  for(tier in c("speakerA", "speakerB", "silenceB", "overlap")){
    tgA <- tg.removeTier(tgA, tier)
  }
  fileA <- gsub(".wav", "_VUV.TextGrid", files$wA[i])
  tg.write(tgA, paste0(folder, fileA))
  
  tgB <-  tg.read(paste0(folder, files$tg[i]), encoding=detectEncoding(paste0(folder, files$tg[i])))
  for(tier in c("speakerA", "speakerB", "silenceA", "overlap")){
    tgB <- tg.removeTier(tgB, tier)
  }
  fileB <- gsub(".wav", "_VUV.TextGrid", files$wB[i])
  tg.write(tgB, paste0(folder, fileB))
}
