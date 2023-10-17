# Tom Offrede
# Check f0 extraction

library(tidyverse)
library(tuneR)
library(zoo)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/processed/"
folderFig <- paste0(here::here(), "/figures/f0check/")
dyads <- c("AML", "FWR", "FXO", "HAG", "HBR", "HUJ", "KDA", "KPB", "MJG", "NLO", "OAL", "OXQ", "QRT", "SGB", "SUK", "TTN", "TTY", "VDE", "ZNV")

TXT <- list.files(folder, "\\.txt", recursive = TRUE)
TGv <- list.files(folder, "^[A-Z]{3}-(D|L)[0-9]\\.TextGrid$", recursive = TRUE)
files <- data.frame(TXT) |> 
  mutate(TG = case_when(substr(TXT, 1, 10) %in% substr(TGv, 1, 10) ~ TGv[match(substr(TXT, 1, 10), substr(TGv, 1, 10))]))

alltxt <- data.frame(matrix(nrow=0, ncol=21))
names(alltxt) <- c("onset", "offset", "f0mean", "f0med", "f0sd", "f0min", "f0max", "f0middle", "intensityMean", "intensityMax", "f1mean", "f1std", "f1med", "f1middle", "f2mean", "f2std", "f2med", "f2middle", "keep", "speaker", "file")

# first we create a dataset with the values from all speakers
for(i in 1:nrow(files)){
  silenceTier <- ifelse(grepl("-A", files$TXT[i]), "silenceA", "silenceB")
  speakerTier <- gsub("silence", "speaker", silenceTier)
  txt <- read.table(paste0(folder, files$TXT[i]), header = TRUE, na.strings = "--undefined--") |> 
    mutate(keep = "no",
           speaker = paste0(substr(files$TXT[i], 5, 8), substr(files$TXT[i], 12, 12)),
           file = substr(files$TXT[i], 9, 10))
  tg <- tg.read(paste0(folder, files$TG[i]), encoding=detectEncoding(paste0(folder, files$TG[i])))
  
  # we'll make sure that the only values in the dataset are those belonging to actual "IPU" intervals as annotated in the textgrid
  for(j in 1:nrow(txt)){
    for(k in 1:tg.getNumberOfIntervals(tg, silenceTier)){
      if(txt$keep[j] == "ok"){ # if intervals have been checked and there's already an "ok" in `txt`, just skip to the next `k`
        break
      }
      if(txt$onset[j] == round(tg.getIntervalStartTime(tg, silenceTier, k), 3)){
        if(txt$offset[j] == round(tg.getIntervalEndTime(tg, silenceTier, k), 3)){
          if(tg.getLabel(tg, silenceTier, k) == "sounding"){
            if(tg.getLabel(tg, speakerTier, tg.getIntervalIndexAtTime(tg, speakerTier, as.numeric(tg.getIntervalStartTime(tg, silenceTier, k))+0.001)) == "s"){
            txt$keep[j] <- "ok"
            }
          }
        }
      }
    }
  }
  
  txt <- txt |> 
    filter(keep=="ok")
  alltxt <- rbind(alltxt, txt)
}

for(s in unique(alltxt$speaker)){
  dat <- alltxt |> filter(speaker==s)
  
  noNAp <- round(sum(is.na(dat$f0mean)) / sum(!is.na(dat$f0mean)), 3)
  png(filename=paste0(folderFig, "/f0mean/", s, ".png"))
  plot(dat$f0mean, main=paste0(s, " - ", noNAp, "% NAs"))
  dev.off()
  
  noNAp <- round(sum(is.na(dat$f0med)) / sum(!is.na(dat$f0med)), 3)
  png(filename=paste0(folderFig, "/f0median/", s, ".png"))
  plot(dat$f0mean, main=paste0(s, " - ", noNAp, "% NAs"))
  dev.off()
}

################

# folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/TestF0Extraction/AllWAV/NewExtraction/FXO/"
# folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/HAG/"
files <- list.files(folder, "wav")
f <- files[[2]]
# f <- "sample.wav"

# p <- 113500:115000
p <- 270000:272000
# p <- 1950000:1990000

sound::play(input)

input <- tuneR::readWave(paste0(folder, f))NaN
# plot(input@left, type="l")
input@left[abs(diff(input@left))>100] <- 
  # plot(input@left, type="l")
  input@left[abs(input@left)<70] <- NaN
# plot(input@left, type="l")
input@left <- as.integer(zoo::na.approx(input@left))
# plot(input@left, type="l")

tuneR::writeWave(input, paste0(folder, "HAD-D1-A-Filtered.wav"))

#######



files <- list.files(folder, "\\.txt")
files <- files[!grepl("Register", files)]

for(f in files){
  txt <- read.table(paste0(folder, f), header=TRUE)
  plot(txt$f0mean, main=f)
  readline("Press Enter")
}

nrow(read.table(paste0(folder, files[[4]]), header=TRUE, na.strings = "--undefined--") %>% filter(!is.na(f0mean)))

nrow(dat %>% filter(speaker=="B", file=="FXO-L1-GOOD"))