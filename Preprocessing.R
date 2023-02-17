# Process data and make it ready for analysis

library(tidyverse)
library(tuneR)
library(rPraat)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/"
folderTG <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/AllTextGrids/"
folderSpeech <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/compare/"
`%!in%` <- Negate(`%in%`)

m <- read.csv(paste0(folder, "metadata.csv"))

TXT <- list.files(folderSpeech, "txt")
TGv <- list.files(folderSpeech, "TextGrid") # v for vector

files <- data.frame(TXT) %>% 
  mutate(TG = case_when(substr(TXT, 1, 6) %in% substr(TGv, 1, 6) ~ TGv[match(substr(TXT, 1, 6), substr(TGv, 1, 6))]))

f0 <- data.frame(matrix(nrow=0, ncol=))
names(f0) <- c("file", "speaker", "period", "onset", "offset", "f0raw")

for(i in 1:nrow(files)){
  if(grepl("-A-", files$TXT[i])){speaker <- "A"}
  if(grepl("-B-", files$TXT[i])){speaker <- "B"}
  if(speaker == "A"){tier <- "speakerA"}
  if(speaker == "B"){tier <- "speakerB"}
  
  
  txt <- read.table(paste0(folderSpeech, files$TXT[i]), header = TRUE)
  tg <- tg.read(paste0(folderSpeech, files$TG[i]), encoding=detectEncoding(paste0(folderSpeech, files$TG[i])))
  
  for(for(p in 1:tg.getNumberOfIntervals(tg, tier)))
  
  f0[nrow(f0)+1,] <- c(substr(files$TXT[i], 1, 6),
                       speaker,
                       p,
                       onset,
                       offset,
                       f0mean)
  
  
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




























