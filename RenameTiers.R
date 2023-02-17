# Tom Offrede
# Rename tiers from textgrids

library(rPraat)

folderTG <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/AllTextGrids/"

TGv <- list.files(folderTG, "TextGrid") # v for vector

for(t in TGv){
  tg <- tg.read(paste0(folderTG, t), encoding=detectEncoding(paste0(folderTG, t)))
  if(tg.getTierName(tg, 1) == "speaker1"){
    tg <- tg.setTierName(tg, 1, "speakerA")
  }
  if(tg.getTierName(tg, 2) == "speaker2"){
    tg <- tg.setTierName(tg, 2, "speakerB")
  }
  tg.write(tg, paste0(folderTG, t))
}

# See if they all have the right name now

tiers <- data.frame(matrix(nrow=0, ncol=3))
names(tiers) <- c("file", "tier1", "tier2")

for(t in TGv){
  tg <- tg.read(paste0(folderTG, t), encoding=detectEncoding(paste0(folderTG, t)))
  tiers[nrow(tiers)+1,] <- c(t,
                             tg.getTierName(tg, 1),
                             tg.getTierName(tg, 2))
}
