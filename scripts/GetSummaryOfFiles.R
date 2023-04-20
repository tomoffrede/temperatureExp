# Tom Offrede
# open files from each folder and create a table with a summary

folderGeneral <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/"
dyads <- c("AML", "FWR", "FXO", "HAG", "HBR", "HUJ", "KDA", "KPB", "MJG", "NLO", "OAL", "OXQ", "QRT", "SGB", "SUK", "TTN", "TTY", "VDE", "ZNV")

ranges <- data.frame(matrix(nrow=0, ncol=3))
names(ranges) <- c("dyad", "min", "max")

for(d in dyads){
  folder <-  paste0(folderGeneral, d, "/")
  txt <- read.table(paste0(folder, "SpeakerRegister.txt"), header = TRUE, fill=TRUE) %>% 
    mutate_at(c("f0min", "f0max"), as.numeric)
  
  ranges[nrow(ranges)+1,] <- c(d, min(txt$f0min, na.rm = TRUE)-1, max(txt$f0max, na.rm=TRUE)+1)
}

write.table(ranges, file=paste0(folderGeneral, "SpeakerRegisters.txt"), quote=FALSE, row.names = FALSE)
