# join TextGrid with pause annotations (created through a Praat script) and one with turn annotations (created manually)

library(rPraat)
library(tidyverse)
library(tuneR)
`%!in%` <- Negate(`%in%`)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/HAG/compare/"

fOG <- list.files(folder, "OG.TextGrid")
fS <- list.files(folder, "sil.TextGrid")
fSA <- fS[grepl("-A", fS)]
fSB <- fS[grepl("-B", fS)]
fW <- list.files(folder, "wav")
fWA <- fW[grepl("-A", fW)]
fWB <- fW[grepl("-B", fW)]

files0 <- cbind(fSA, fSB)
files1 <- cbind(files0, fWA)
files2 <- cbind(files1, fWB)
files <- data.frame(cbind(files2, fOG)) %>% 
  rename("SA"="fSA", "SB"="fSB", "WA"="fWA", "WB"="fWB", "OG"="fOG") %>% 
  mutate(worked1 = ifelse(substr(SA, 1, 6) == substr(SB, 1, 6), "worked!", "NO!!!"),
         worked2 = ifelse(substr(SA, 1, 6) == substr(OG, 1, 6), "worked!", "NO!!!"),
         worked3 = ifelse(substr(WA, 1, 6) == substr(SA, 1, 6), "worked!", "NO!!!"),
         worked4 = ifelse(substr(WA, 1, 6) == substr(WB, 1, 6), "worked!", "NO!!!"))
table(files$worked1)
table(files$worked2)
table(files$worked3)
table(files$worked4)

files <- files %>% select(-c("worked1":"worked4"))

for(i in 1:nrow(files)){
  sA <- tg.read(paste0(folder, files$SA[i]), encoding=detectEncoding(paste0(folder, files$SA[i])))
  sB <- tg.read(paste0(folder, files$SB[i]), encoding=detectEncoding(paste0(folder, files$SB[i])))
  wA <- readWave(paste0(folder, files$WA[i]))
  wB <- readWave(paste0(folder, files$WB[i]))
  og <- tg.read(paste0(folder, files$OG[i]), encoding=detectEncoding(paste0(folder, files$OG[i])))
  
  sA <- tg.setTierName(sA, "union", "silenceA")
  sB <- tg.setTierName(sB, "union", "silenceB")
  
  dur <- (length(wA)/wA@samp.rate)
  
  new <- tg.createNewTextGrid(0, dur)
  
  new <- tg.insertNewIntervalTier(new, newInd=1, tg.getTierName(og, "speakerA"), tMin=0, tMax=tg.getEndTime(new))
  for(n in 1:(tg.getNumberOfIntervals(og, "speakerA"))){
    new <- tg.insertInterval(new,
                             tg.getTierName(og, "speakerA"),
                             tStart=tg.getIntervalStartTime(og, "speakerA", n),
                             tEnd=ifelse(n == tg.getNumberOfIntervals(og, "speakerA"), # there may be a difference in the duration of original textgrid and wav file, so if this is the last interval, use the wav file duration for tEnd
                                         dur - 0.01, # not allowed to have an interval ending exactly at the end of the file
                                         tg.getIntervalEndTime(og, "speakerA", n)),
                             label=tg.getLabel(og, "speakerA", n))
  }
  new <- tg.insertNewIntervalTier(new, newInd=2, tg.getTierName(og, "speakerB"), tMin=0, tMax=tg.getEndTime(new))
  for(n in 1:(tg.getNumberOfIntervals(og, "speakerB"))){
    new <- tg.insertInterval(new,
                            tg.getTierName(og, "speakerB"),
                            tStart=tg.getIntervalStartTime(og, "speakerB", n),
                            tEnd=ifelse(n == tg.getNumberOfIntervals(og, "speakerB"), # there may be a difference in the duration of original textgrid and wav file, so if this is the last interval, use the wav file duration for tEnd
                                        dur - 0.0000001,
                                        tg.getIntervalEndTime(og, "speakerB", n)),
                            label=tg.getLabel(og, "speakerB", n))
  }
  new <- tg.insertNewIntervalTier(new, newInd=3, tg.getTierName(sA, "silenceA"), tMin=0, tMax=tg.getEndTime(new))
  for(n in 1:(tg.getNumberOfIntervals(sA, "silenceA"))){
    new <- tg.insertInterval(new,
                             tg.getTierName(sA, "silenceA"),
                             tStart=tg.getIntervalStartTime(sA, "silenceA", n),
                             tEnd=ifelse(n == tg.getNumberOfIntervals(sA, "silenceA"), # there was some issue with the last interval, so at the last interval, make the tEnd the end of the textgrid
                                         dur - 0.0000001,
                                         tg.getIntervalEndTime(sA, "silenceA", n)),
                             label=tg.getLabel(sA, "silenceA", n))
  }
  new <- tg.insertNewIntervalTier(new, newInd=4, tg.getTierName(sB, "silenceB"), tMin=0, tMax=tg.getEndTime(new))
  for(n in 1:(tg.getNumberOfIntervals(sB, "silenceB"))){
    new <- tg.insertInterval(new,
                            tg.getTierName(sB, "silenceB"),
                            tStart=tg.getIntervalStartTime(sB, "silenceB", n),
                            tEnd=ifelse(n == tg.getNumberOfIntervals(sB, "silenceB"), # there was some issue with the last interval, so at the last interval, make the tEnd the end of the textgrid
                                        dur - 0.0000001,
                                        tg.getIntervalEndTime(sB, "silenceB", n)),
                            label=tg.getLabel(sB, "silenceB", n))
  }
  
  name <- gsub("-OG", "", files$OG[i])
  tg.write(new, paste0(folder, name))
}
