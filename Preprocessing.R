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

f0 <- data.frame(matrix(nrow=0, ncol=8))
names(f0) <- c("file", "speaker", "turn", "IPU", "onset", "offset", "f0raw", "label")

for(i in 1:nrow(files)){
  speaker <- ifelse(grepl("-A-", files$TXT[i]), "A", "B")
  turnTier <- paste0("speaker", speaker)
  turnOtherTier <- paste0("speaker", ifelse(speaker=="A", "A" , "B"))
  silenceTier <- paste0("silence", speaker)
  turnCount <- 0
  
  txt <- read.table(paste0(folderSpeech, files$TXT[i]), header = TRUE)
  tg <- tg.read(paste0(folderSpeech, files$TG[i]), encoding=detectEncoding(paste0(folderSpeech, files$TG[i])))
  
  
  for(p in 1:tg.getNumberOfIntervals(tg, turnTier)){
    if(tg.getLabel(tg, turnTier, p) != ""){
      ipuCount <- 0
      turnCount <- turnCount + 1
      turnOnset <- as.numeric(tg.getIntervalStartTime(tg, turnTier, p))
      turnOffset <- as.numeric(tg.getIntervalEndTime(tg, turnTier, p))
      for(p in 1:tg.getNumberOfIntervals(tg, silenceTier)){
        if(tg.getLabel(tg, silenceTier, p)=="sounding"){
          startIPU <- as.numeric(tg.getIntervalStartTime(tg, silenceTier, p))
          endIPU <- as.numeric(tg.getIntervalEndTime(tg, silenceTier, p))
          if(turnOnset <= startIPU){ # doing two separate if() statements because there's some confusion with the use of &(&)
            if(turnOffset >= endIPU){
              ipuCount <- ipuCount + 1
              f <- data.frame(matrix(nrow=0, ncol=3))
              names(f) <- c("f0mean", "f0sd", "f0med")
              for(l in 1:nrow(txt)){
                if(txt$onset[l] >= startIPU){
                  if(txt$offset[l] <= endIPU){
                    extract_textgrid(tg, "speakerA")
                    # if(tg.getLabel(tg, turnOtherTier, tg.getIntervalIndexAtTime()))
                      f[nrow(f)+1,] <- c(as.numeric(txt$f0mean[l]),
                                         as.numeric(txt$f0sd[l]),
                                         as.numeric(txt$f0med[l]))
                  }
                }
              }
              if(any(!is.na(f))){
                f0[nrow(f0)+1,] <- c(substr(files$filesTG[i], 1, 6),
                                     speaker,
                                     turnCount,
                                     ipuCount,
                                     mean(f$f0mean, na.rm=TRUE),
                                     mean(f$f0sd, na.rm=TRUE),
                                     mean(f$f0med, na.rm=TRUE),
                                     tg.getLabel(tg, turnTier, p))
              }
      }
  }
  
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




























