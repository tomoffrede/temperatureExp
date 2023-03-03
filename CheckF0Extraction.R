# Tom Offrede
# Check f0 extraction

library(tidyverse)
library(tuneR)
library(zoo)

# folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/TestF0Extraction/AllWAV/NewExtraction/FXO/"

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/HAG/"
files <- list.files(folder, "wav")
f <- files[[2]]
# f <- "sample.wav"

# p <- 113500:115000
p <- 270000:272000
# p <- 1950000:1990000

sound::play(input)

input <- tuneR::readWave(paste0(folder, f))
# plot(input@left, type="l")
input@left[abs(diff(input@left))>100] <- NaN
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

















