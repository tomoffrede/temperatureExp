# Tom Offrede
# Create a folder for each dyad
# Rename files into something meaningful

library(tidyverse)
library(tuneR)
library(rPraat)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/"
folder2 <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/AllTextGrids-manualAnnotationOnly/"
folderAllW <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/AllWav/"
dyads <- c("AML", "FWR", "FXO", "HAG", "HBR", "HUJ", "KDA", "KPB", "MJG", "NLO", "OAL", "OXQ", "QRT", "SGB", "SUK", "TTN", "TTY", "VDE", "ZNV")
# dyads <- c(c("HAG", "ZNV", "AML", "HUJ", "KPB"))

# Copy all files from one folder to another

og <- list.files(folder, "OverlapSp.TextGrid")
og <- og[!grepl("Overlap", og)]
for(o in og){
  file.copy(from = paste0(folder, o),
            to = paste0(folder2, o))
}

folderBackUp <- paste0(folder, "backUp-oldPreprocessing-canDeleteLater/")
for(d in dyads){
  currentFolder <- paste0(folder, d, "/")
  files <- list.files(currentFolder)
  files <- files[!grepl("\\.wav", files) & !grepl("Register", files) & !grepl("unused", files)]
  for(f in files){
    file.copy(from = paste0(currentFolder, f),
              to = paste0(folderBackUp, f))
    file.remove(paste0(currentFolder, f))
  }
}

# # Create folders
# 
for(d in dyads){
  dir.create(paste0(folder, d, "/"))
}

# # Copy files from a general folder into their appropriate folder

t <- list.files(folder2)

for(f in t){
  for(d in dyads){
    folderCurrentDyad <- paste0(folder, d, "/")
    if(grepl(d, f)){
      file <- paste0(folder2, f, "/")
      file.copy(file, folderCurrentDyad, overwrite=TRUE)
    }
  }
}

# Copy files from their individual folder into a folder with all speakers

for(d in dyads){
  folderC <- paste0(folder, d, "/")
  # files <- list.files(folderC, "^[A-Z]{3}-(D|L)[0-9]\\.TextGrid$")
  files <- list.files(folderC, "\\.txt$")
  files <- files[!grepl("Register", files)]
  for(f in files){
    file <- paste0(folderC, f, "/")
    file.copy(file, folder2, overwrite = TRUE)
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

# Delete files

for(d in dyads){
  folderC <- paste0(folder, d, "/")
  txtToRemove <- list.files(folderC, "\\.txt")
  txtToRemove <- txtToRemove[!grepl("Register", txtToRemove)]
  tgToRemove <- list.files(folderC, "\\.TextGrid")
  # tgToRemove <- tgToRemove[!grepl("OG", tgToRemove)]
  for(f in c(txtToRemove, tgToRemove)){
    file.remove(paste0(folderC, f))
  }
}

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

# Option 3

for(d in dyads){
  folderC <- paste0(folder, d, "/")
  files <- list.files(folderC, "OG")
  newName <- gsub("-OG", "", f)
  file.rename(from = paste0(folder, f),
              to = paste0(folder, newName))
}

# Option 3

folderOG <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/TempData/wuppertal/oldFileNaming/"
folderNEW <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/TempData/wuppertal/"

filesOG <- list.files(folderOG, "\\.txt", recursive = TRUE)

for(f in filesOG){
  name1 <- str_split(f, fixed("-"))[[1]][[1]]
  name2 <- paste0("-", (as.numeric(gsub(".txt", "", str_split(f, fixed("-"))[[1]][[2]])) + 11), ".txt")
  file.copy(from=paste0(folderOG, f), to=paste0(folderNEW, name1, name2))
}

# Cut out first few lines of txt files (not tables)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/TempData/wuppertal/allInfo/"
folderNew <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/TempData/wuppertal/"
files <- list.files(folder, "\\.txt")

for(f in files){
  t <- readLines(paste0(folder, f))
  t <- t[21:length(t)]
  writeLines(t, paste0(folderNew, f))
}

# Reformat txt files that have a wrong line division
