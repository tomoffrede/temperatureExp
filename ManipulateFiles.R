# Tom Offrede
# Create a folder for each dyad
# Rename files into something meaningful

library(tidyverse)
library(tuneR)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/"
dyads <- c("AML", "FWR", "FXO", "HAG", "HBR", "HUJ", "KDA", "KPB", "MJG", "NLO", "OAL", "OXQ", "QRT", "SGB", "SUK", "TTN", "TTY", "VDE", "ZNV")

# # Create folders
# 
# for(d in dyads){
#   dir.create(paste0(folder, d))
# }
# 
# # Copy textgrids into their appropriate folder
# 
# t <- list.files(folder, "TextGrid")
# 
# for(f in t){
#   for(d in dyads){
#     folderCurrentDyad <- paste0(folder, d, "/")
#     if(grepl(d, f)){
#       file <- paste0(folder, f, "/")
#       file.copy(file, folderCurrentDyad)
#     }
#   }
# }

# See which Diapix files are legit and which don't have actual data

diapix <- data.frame(matrix(nrow=0, ncol=4))
names(diapix) <- c("dyad", "file", "lengthL", "lengthR")

for(d in dyads){
  folderC <- paste0(folder, d, "/")
  files <- list.files(folderC, "Diapix")
  for(f in files){
    wav <- readWave(paste0(folderC, f))
    diapix[nrow(diapix)+1,] <- c(d, f,
                                 as.numeric(length(wav@left)/wav@samp.rate),
                                 as.numeric(length(wav@right)/wav@samp.rate))
  }
}
diapix <- diapix %>% 
  mutate_at(c("lengthL", "lengthR"), as.numeric)

# Delete short files
for(i in 1:nrow(diapix)){
  if(diapix$lengthL[i] < 10){ # 10 seconds
    file.remove(paste0(folder, diapix$dyad[i], diapix$file[i]))
  }
  
}


# Rename files

# d="FXO"
for(d in dyads){
  folderC <- paste0(folder, d, "/")
  
  # wav files
  files <- list.files(folderC, "wav")
  for(f in files){
    if(grepl("DiapixA", f)){name <- "D1A"}
    if(grepl("DiapixB", f)){name <- "D1B"}
    if(grepl("DiapixA2", f)){name <- "D2A"}
    if(grepl("DiapixB2", f)){name <- "D2B"}
    if(grepl("List1A", f)){name <- "L1A"}
    if(grepl("List1B", f)){name <- "L1B"}
    if(grepl("List2A", f)){name <- "L2A"}
    if(grepl("List2B", f)){name <- "L2B"}
    if(grepl("List3A", f)){name <- "L3A"}
    if(grepl("List3B", f)){name <- "L3B"}
   file.rename(from = paste0(folderC, f),
               to = paste0(folderC, d, "-", name, ".wav")) 
  }
}















