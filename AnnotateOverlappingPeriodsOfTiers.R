# Tom Offrede
# compare the labels of 2 tiers; if there are periods of overlap between two identically labeled intervals, annotate those overlapping intervals into an additional tier

library(tidyverse)
library(rPraat)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/compare/"

files <- list.files(folder, "\\.TextGrid")

intervals <- data.frame(matrix(nrow=0, ncol=))
names(intervals) <- c("file", "speakerA", "speakerB", "onset", "offset", "label")

for(f in files){
  tg <- tg.read(paste0(folder, f), encoding = detectEncoding(paste0(folder, f)))
  
  for(i in 1:tg.getNumberOfIntervals(tg, tier)){
    
  }
    
    
}




