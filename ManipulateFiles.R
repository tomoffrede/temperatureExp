# Tom Offrede
# Create a folder for each dyad
# Rename files into something meaningful

library(tidyverse)
library(tuneR)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/HAG/compare/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/TestF0Extraction/AllWAV/NewExtraction/FXO/"
folderAllTG <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/AllTextGrids/"
folderAllW <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/AllWav/"
folderAllWF <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/TestF0Extraction/AllWAV/"
dyads <- c("AML", "FWR", "FXO", "HAG", "HBR", "HUJ", "KDA", "KPB", "MJG", "NLO", "OAL", "OXQ", "QRT", "SGB", "SUK", "TTN", "TTY", "VDE", "ZNV")
dyads <- c("FXO", "NLO", "KDA", "QRT")

# Copy all files from one folder to another

og <- list.files(folder, "OverlapSp.TextGrid")
og <- og[!grepl("Overlap", og)]
for(o in og){
  file.copy(from = paste0(folder, o),
            to = paste0(folder2, o))
}

# Copy each TextGrid twice into the same folder, each time taking the name of the corresponding wav file (there are 2 wav files per TextGrid)

# tg <- list.files(folder, "^[A-Z]{3}-(D|L)[0-9]\\.TextGrid$")
tg <- list.files(folder, "f0\\.TextGrid$")
w <- list.files(folder, "wav")
wA <- w[grepl("-A", w)]
wB <- w[grepl("-B", w)]

files0 <- cbind(wA, wB)
files <- data.frame(cbind(files0, tg)) %>%  
  mutate(worked1 = ifelse(substr(wA, 1, 6) == substr(wB, 1, 6), "worked!", "NO!!!"),
         worked2 = ifelse(substr(wA, 1, 6) == substr(tg, 1, 6), "worked!", "NO!!!"))
table(files$worked1)
table(files$worked2)

for(i in 1:nrow(files)){
  tgA <- gsub("wav", "TextGrid", files$wA[i])
  tgB <- gsub("wav", "TextGrid", files$wB[i])
  for(t in c(tgA, tgB)){
    file.copy(from = paste0(folder, files$tg[i]),
              to = paste0(folder, t), overwrite = TRUE)
  }
}

# # Create folders
# 
# for(d in dyads){
#   dir.create(paste0(folder, d))
# }

# # Copy files from a general folder into their appropriate folder

t <- list.files(folder2, "OverlapSp2")

for(f in t){
  for(d in dyads){
    folderCurrentDyad <- paste0(folder2, d, "/")
    if(grepl(d, f)){
      file <- paste0(folder2, f, "/")
      file.copy(file, folderCurrentDyad)
    }
  }
}

# Copy files from their individual folder into a folder with all speakers

for(d in dyads){
  folderC <- paste0(folder2, d, "/")
  files <- list.files(folderC, "OverlapSp.TextGrid")
  for(f in files){
    file <- paste0(folderC, f, "/")
    file.copy(file, folder2)
  }
}

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
  mutate_at(c("lengthL"), as.numeric) %>% 
  select(-lengthR) # all info in left channel
hist(diapix$lengthL)

s <- diapix %>% 
  filter(lengthL < 100)
hist(s$lengthL)

# # Delete short files
# for(i in 1:nrow(diapix)){
#   if(diapix$lengthL[i] < 10){ # 10 seconds
#     file.remove(paste0(folder, diapix$dyad[i], "/", diapix$file[i]))
#   }
# }

# Rename files

# # Option 1
# 
# for(d in dyads){
#   folderC <- paste0(folder, d, "/")
#   
#   # wav files
#   files <- list.files(folderC)
#   for(f in files){
#     if(grepl("Diapix", f)){name <- "D1"}
#     if(grepl("DiapixA2", f)){name <- "D2"}
#     if(grepl("DiapixB2", f)){name <- "D2"}
#     if(grepl("D1", f)){name <- "D1"}
#     if(grepl("D2", f)){name <- "D2"}
#     if(grepl("List1", f)){name <- "L1"}
#     if(grepl("List1", f)){name <- "L1"}
#     if(grepl("List2", f)){name <- "L2"}
#     if(grepl("List2", f)){name <- "L2"}
#     if(grepl("List3", f)){name <- "L3"}
#     if(grepl("List3", f)){name <- "L3"}
#     if(grepl("A", f)){speaker <- "A"}
#     if(grepl("B", f)){speaker <- "B"}
#     
#     if(grepl("wav", f)){
#       file.rename(from = paste0(folderC, f),
#                   to = paste0(folderC, d, "-", name, "-", speaker, ".wav"))
#     }
#     
#     if(grepl("TextGrid", f)){
#       file.rename(from = paste0(folderC, f),
#                   to = paste0(folderC, d, "-", name, ".TextGrid"))
#     }
#   }
# }

# Option 2

files <- list.files(folder, "OverlapSp.TextGrid")
# files <- files[!grepl("OG", files)]

for(f in files){
  newName <- gsub("-OverlapSp", "", f)
  file.rename(from = paste0(folder, f),
              to = paste0(folder, newName))
}
