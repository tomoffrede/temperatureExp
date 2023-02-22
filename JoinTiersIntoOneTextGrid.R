# join TextGrid with pause annotations (created through a Praat script) and one with turn annotations (created manually)

library(rPraat)
library(tidyverse)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/compare/"

fOG <- list.files(folder, "OG.TextGrid")
fSA <- list.files(folder, "A-sil.TextGrid")
fSB <- list.files(folder, "B-sil.TextGrid")

files0 <- cbind(fSA, fSB)
files <- data.frame(cbind(files0, fOG)) %>% 
  rename("A"="fSA", "B"="fSB", "OG"="fOG") %>% 
  mutate(worked1 = ifelse(substr(A, 1, 6) == substr(B, 1, 6), "worked!", "NO!!!"),
         worked2 = ifelse(substr(A, 1, 6) == substr(OG, 1, 6), "worked!", "NO!!!"))
table(files$worked1)
table(files$worked2)

for(i in 1:nrow(files)){
  sA <- tg.read(paste0(folder, files$A[i]), encoding=detectEncoding(paste0(folder, files$A[i])))
  sB <- tg.read(paste0(folder, files$B[i]), encoding=detectEncoding(paste0(folder, files$B[i])))
  og <- tg.read(paste0(folder, files$OG[i]), encoding=detectEncoding(paste0(folder, files$OG[i])))
  
  tg.getTotalDuration(sA) # it seems that the OG textgrid has slightly shorter duration (for all files)
  tg.getTotalDuration(sB) # the S textgrids have the correct duration. so we'll do all the modifications with `s` as a the base textgrid
  tg.getTotalDuration(og) 
  
  sA <- tg.removeTier(sA, 3)
  sA <- tg.removeTier(sA, 2)
  
  sA <- tg.insertNewIntervalTier(sA, newInd=1, tg.getTierName(og, 1), tMin=0, tMax=tg.getEndTime(sA))
  for(n in 1:(tg.getNumberOfIntervals(og, 1))){ # there was a weird issue with the last interval,
    # but since the last interval is usually gonna be outside of the last turn anyway (even for baseline),
    # it's probably ok not to use the last annotated "IPU"
    sA <- tg.insertInterval(sA,
                            tg.getTierName(og, 1),
                            tStart=tg.getIntervalStartTime(og, 1, n),
                            tEnd=tg.getIntervalEndTime(og, 1, n),
                            label=tg.getLabel(og, 1, n))
  }
  sA <- tg.insertNewIntervalTier(sA, newInd=2, tg.getTierName(og, 2), tMin=0, tMax=tg.getEndTime(sA))
  for(n in 1:(tg.getNumberOfIntervals(og, 2))){ # there was a weird issue with the last interval,
    # but since the last interval is usually gonna be outside of the last turn anyway (even for baseline),
    # it's probably ok not to use the last annotated "IPU"
    sA <- tg.insertInterval(sA,
                            tg.getTierName(og, 2),
                            tStart=tg.getIntervalStartTime(og, 2, n),
                            tEnd=tg.getIntervalEndTime(og, 2, n),
                            label=tg.getLabel(og, 2, n))
  }
  sA <- tg.insertNewIntervalTier(sA, newInd=Inf, tg.getTierName(sB, 1), tMin=0, tMax=tg.getEndTime(sA))
  for(n in 1:(tg.getNumberOfIntervals(sB, 1))){ # there was a weird issue with the last interval,
    # but since the last interval is usually gonna be outside of the last turn anyway (even for baseline),
    # it's probably ok not to use the last annotated "IPU"
    sA <- tg.insertInterval(sA,
                            tg.getTierName(sB, 1),
                            tStart=tg.getIntervalStartTime(sB, 1, n),
                            tEnd=tg.getIntervalEndTime(sB, 1, n),
                            label=tg.getLabel(sB, 1, n))
  }
  
  name <- gsub(".-OG", "", files$OG[i])
  tg.write(sA, paste0(folder, name))
}