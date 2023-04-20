# Tom Offrede
# Read a bunch of files that seem duplicated, except that some have "(1)" in their name (and some don't)

library(tidyverse)


`%!in%` <- Negate(`%in%`)

folderTXT <- "C:/Users/tomof/Documents/1HU/ExperimentTemperature/Data/TempTXT/"

filesall <- list.files(folderTXT)
files <- filesall[!grepl("\\(1\\)", filesall)]

files1 <- filesall[grepl("\\(1\\)", filesall)]
filesr <- filesall[substr(filesall, 1, 5) %in% substr(files1, 1, 5) & !grepl("\\(1\\)", filesall)]
files1r <- data.frame(cbind(files1, filesr)) %>% 
  mutate(worked = ifelse(substr(files1, 1, 5)==substr(filesr, 1, 5), "worked!!", "NO!!!!!!"),
         sameFile = NA)
table(files1r$worked)

for(i in 1:nrow(files1r)){
  f1 <- read.table(paste0(folderTXT, files1r$files1[i]), sep = "\t", header=TRUE, fileEncoding = guess_encoding(paste0(folderTXT, files1r$files1[i]))$encoding[1])
  fr <- read.table(paste0(folderTXT, files1r$filesr[i]), sep = "\t", header=TRUE, fileEncoding = guess_encoding(paste0(folderTXT, files1r$files1[i]))$encoding[1])
  file <- substr(files1r$files1, 1, 5)
  if(all_equal(f1, fr)){
    files1r$sameFile[i] <- "same!"
  } else{
    files1r$sameFile[i] <- "NO!!!"
  }
}

table(files1r$sameFile)