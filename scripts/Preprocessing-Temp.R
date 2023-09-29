# Tom Offrede
# Process temperature data: read txt files, get mean of 10% hottest pixels of each ROI per participant per frame

library(tidyverse)

folderTXT <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/TempData/Extraction-Official/"
`%!in%` <- Negate(`%in%`)

files <- list.files(folderTXT, "\\.txt")

dat <- data.frame(matrix(nrow=0, ncol=4))
names(dat) <- c("speaker", "timeStamp", "ROI", "temperature")


for(f in files){
  lines <- readLines(paste0(folderTXT, f))
  timeStamp <- substr(str_split(lines[6], fixed("\t"))[[1]][[2]], 1, 8) # splits the line that contains the time stamp and gets the 2nd value (i.e. the time stamp), and then save only 1st to 8th characters, i.e. no milisecond info
  
  a <- read.table(paste0(folderTXT, f), sep = "\t", header=TRUE, fileEncoding = readr::guess_encoding(paste0(folderTXT, f))$encoding[1])
  a <- a[,grepl("C", names(a)) & !grepl("px", names(a))]
  names(a) <- gsub("\\.\\.\\.\\.C\\.", "", names(a))
  # names(a) <- gsub("....C.", "", names(a))
  a <- a %>% mutate_at(names(a), as.numeric)
  if(nchar(f) == 9){ # if it's frame [1-9], put a zero before it, so that all files have 10 characters (counting with ".txt")
    f <- gsub("-", "-0", f)
  }
  
  tempA <- data.frame(matrix(nrow=0, ncol=4))
  names(tempA) <- c("speaker", "frame", "ROI", "temperature")
  tempB <- data.frame(matrix(nrow=0, ncol=4))
  names(tempB) <- c("speaker", "frame", "ROI", "temperature")
  # w <- NA
  for(i in 1:ncol(a)){
    # if(substr(names(a[i]), nchar(names(a[i])), nchar(names(a[i]))) %!in% c("A", "B") || substr(names(a[i]), 1, 1) %!in% c("F", "E", "N", "C", "M")){ # some columns were named wrong and contain only numbers. Here I try to correct them.
    #   wrong[nrow(wrong)+1,] <- c(f, names(a[i]))
    # }
    
    frame <- substr(f, 5, 6)
    sp <- paste0(substr(f, 1, 4), substr(names(a[i]), nchar(names(a[i])), nchar(names(a[i]))))
    
    o <- sort(a[,i], decreasing=TRUE)
    o <- o[1:(length(o)*0.1)] # if you want a different amount of pixels (instead of 10%), just change the 0.1 here
    
    if(substr(sp, nchar(sp), nchar(sp)) == "A"){
      tempA[nrow(tempA)+1,] <- c(sp, frame, gsub("\\..*", "", names(a[i])), as.numeric(mean(o)))
    }
    if(substr(sp, nchar(sp), nchar(sp)) == "B"){
      tempB[nrow(tempB)+1,] <- c(sp, frame, gsub("\\..*", "", names(a[i])), as.numeric(mean(o)))
    }
  }
  
  temp <- list(tempA, tempB)
  
  for(t in 1:length(temp)){
    if(nrow(temp[[t]]) > 1){
      temp[[t]]$temperature <- as.numeric(temp[[t]]$temperature)
      temp[[t]][nrow(temp[[t]])+1,] <- c(unique(temp[[t]]$speaker), frame, "Face", mean(temp[[t]]$temperature, na.rm=TRUE))
    }
  }
  
  temp0 <- rbind(temp[[1]], temp[[2]])
  dat <- rbind(dat, temp0)
  
  # f <- paste0(substr(f, 1, 4), as.numeric(frame) + 1, ".txt")
}

dat <- dat %>%
  mutate_at(c("speaker", "ROI"), as.factor) %>%
  mutate_at("frame", as.integer) %>%
  mutate_at(c("temperature"), as.numeric)
  
# There's a very high (.97+) correlation between C1 and C2 and between E1 and E2

c <- dat %>%
  filter(ROI %in% c("C1", "C2", "E1", "E2")) %>% 
  mutate(ROI = ifelse(ROI %in% c("C1", "C2"), "C", "E")) %>% 
  group_by(speaker, frame, ROI) %>% 
  mutate(newTemp = mean(temperature, na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(!duplicated(paste(speaker, frame, ROI))) %>% 
  select(-temperature) %>% 
  rename(temperature=newTemp)

dat <- dat %>% 
  filter(ROI %!in% c("C1", "C2", "E1", "E2"))

dat <- rbind(dat, c)

# Join metadata (which was cleaned in Preprocessing-Speech):

load(paste0(here::here(), "/data/metadata-clean.RData"))

m <- m %>% 
  pivot_longer(realTempPre:realTempPost, names_to = "stage", values_to = "realTemp") %>% 
  mutate(stage = ifelse(stage == "realTempPre", "beginning", ifelse(stage == "realTempDuring", "middle", "ending")))

dat <- dat %>% 
  group_by(speaker) %>% 
  mutate(stageFrame = frame / max(frame),
         stage = ifelse(stageFrame <= 0.33, "beginning", ifelse(stageFrame >= 0.66, "ending", "middle"))) %>% 
  ungroup() %>% 
  select(-stageFrame) %>% 
  mutate(tempDiff = NA)

dat0 <- dat
# dat <- dat0

for(s in unique(dat$speaker)){
  for(f in dat$frame){
    for(r in dat$ROI){
      currentTemp <- as.numeric(dat$temperature[dat$speaker==s &
                                                  dat$frame==f &
                                                  dat$ROI==r])
      otherTemp <- as.numeric(dat$temperature[dat$speaker != s &
                                                substr(dat$speaker, 1, 3) == substr(s, 1, 3) &
                                                dat$frame==f &
                                                dat$ROI==r])
      if(!purrr::is_empty(currentTemp)){
        if(!any(is.na(currentTemp))){
          if(!purrr::is_empty(otherTemp)){
            if(!any(is.na(otherTemp))){
              dat$tempDiff[dat$speaker==s & dat$frame==f & dat$ROI==r] <- as.numeric(abs(currentTemp - otherTemp))
            }
          }
        }
      }
    }
  }
}

dat <- merge(dat, m, by=c("speaker", "stage"))

save(dat, file=paste0(here::here(), "/data/tempData-wuppertal.RData"))
