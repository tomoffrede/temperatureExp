# Tom Offrede
# Process temperature data: read txt files, get mean of 10% hottest pixels of each ROI per participant per frame

library(tidyverse)

folderTXT <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/TempData/Extraction-Official/"
folderNew <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/TempData/Extraction-Official/onlyTable/"
`%!in%` <- Negate(`%in%`)

files <- list.files(folderTXT, "\\.txt")
onlyDiapix <- c("AML", "FWR")

dat <- data.frame(matrix(nrow=0, ncol=5))
names(dat) <- c("speaker", "timeStamp", "ROI", "temperature", "task")

for(f in files){
  lines <- readLines(paste0(folderTXT, f))
  timeStamp <- substr(str_split(lines[6], fixed("\t"))[[1]][[2]], 1, 8) # splits the line that contains the time stamp and gets the 2nd value (i.e. the time stamp), and then save only 1st to 8th characters, i.e. no milisecond info
  if(substr(f, 1, 3) %in% onlyDiapix){
    task <- "Diapix"
  } else{
    task <- ifelse(grepl("1", str_split(lines[3], fixed("\t"))[[1]][[2]]), # get the name of the thermal video file and check if it contains a "1" or not, which indicates if it was recorded during the Lists or Diapix task
                        "Diapix",
                        "Lists")}
  
  writeLines(lines[21:length(lines)], paste0(folderNew, f))
  
  a <- read.table(paste0(folderNew, f), sep = "\t", header=TRUE, fileEncoding = readr::guess_encoding(paste0(folderNew, f))$encoding[1]) |> 
    select(contains("C") & !contains("px")) |> 
    mutate_all(as.numeric)
  names(a) <- gsub("\\.\\.\\.\\.C\\.", "", names(a))
  
  tempA <- data.frame(matrix(nrow=0, ncol=4))
  names(tempA) <- c("speaker", "timeStamp", "ROI", "temperature")
  tempB <- data.frame(matrix(nrow=0, ncol=4))
  names(tempB) <- c("speaker", "timeStamp", "ROI", "temperature")

  for(i in 1:ncol(a)){
    sp <- paste0(substr(f, 1, 4), substr(names(a[i]), nchar(names(a[i])), nchar(names(a[i]))))
    
    o <- sort(a[,i], decreasing=TRUE)
    o <- o[1:(length(o)*0.1)] # if you want a different amount of pixels (instead of 10%), just change the 0.1 here
    
    if(substr(sp, nchar(sp), nchar(sp)) == "A"){
      tempA[nrow(tempA)+1,] <- c(sp, timeStamp, gsub("\\..*", "", names(a[i])), as.numeric(mean(o)))
    }
    if(substr(sp, nchar(sp), nchar(sp)) == "B"){
      tempB[nrow(tempB)+1,] <- c(sp, timeStamp, gsub("\\..*", "", names(a[i])), as.numeric(mean(o)))
    }
  }
  
  temp <- list(tempA, tempB)
  for(t in 1:length(temp)){
    temp[[t]]$temperature <- as.numeric(temp[[t]]$temperature)
    if("E1" %in% temp[[t]]$ROI & "E2" %in% temp[[t]]$ROI){
      temp[[t]][nrow(temp[[t]])+1,] <- c(unique(temp[[t]]$speaker),
                                         timeStamp,
                                         "E",
                                         mean(c(temp[[t]]$temperature[temp[[t]]$ROI=="E1"], temp[[t]]$temperature[temp[[t]]$ROI=="E2"])))
    } else{temp[[t]]$ROI[substr(temp[[t]]$ROI, 1, 1)=="E"] <- "E"}
    temp[[t]]$temperature <- as.numeric(temp[[t]]$temperature)
    if("C1" %in% temp[[t]]$ROI & "C2" %in% temp[[t]]$ROI){
      temp[[t]][nrow(temp[[t]])+1,] <- c(unique(temp[[t]]$speaker),
                                         timeStamp,
                                         "C",
                                         mean(c(temp[[t]]$temperature[temp[[t]]$ROI=="C1"], temp[[t]]$temperature[temp[[t]]$ROI=="C2"])))
    } else{temp[[t]]$ROI[substr(temp[[t]]$ROI, 1, 1)=="C"] <- "C"}
    
    temp[[t]] <- temp[[t]] |> 
      filter(ROI %!in% c("E1", "E2", "C1", "C2"))
  }
  
  temp0 <- rbind(temp[[1]], temp[[2]]) |> 
    mutate(task = task) |> 
    mutate_at(c("speaker", "ROI", "task"), as.factor) |> 
    mutate_at(c("temperature"), as.numeric)
  dat <- rbind(dat, temp0)
}

# datSave <- dat
# dat <- datSave

dat$timeStamp <- as.POSIXct(dat$timeStamp, format="%H:%M:%S")

dat <- dat |>
  filter(ROI != "M") |> # comment this line out if you want to keep the ROI mouth
  mutate(ROI = case_when(
    ROI=="N" ~ "Nose",
    ROI=="E" ~ "Eyes",
    ROI=="M" ~ "Mouth",
    ROI=="C" ~ "Cheeks",
    ROI=="F" ~ "Forehead"
  )) |> 
  mutate_at(c("speaker", "ROI", "task"), as.factor) |>
  mutate_at(c("temperature"), as.numeric) |> 
  group_by(task, speaker, ROI) |> 
  mutate(startTime = min(timeStamp),
         time = as.numeric((timeStamp - startTime)/60)) |> # store passage of time in minutes
  ungroup() |> 
  select(-startTime)
  
# Join metadata (which was cleaned in Preprocessing-Speech):

load(paste0(here::here(), "/data/metadata-clean.RData"))

m <- m |> 
  pivot_longer(realTempPre:realTempPost, names_to = "stage", values_to = "realTemp") |> 
  mutate(stage = ifelse(stage == "realTempPre", "beginning", ifelse(stage == "realTempDuring", "middle", "ending")))

dat <- dat |> 
  group_by(speaker, task) |> 
  mutate(stageTime = time / max(time),
         stage = ifelse(stageTime <= 0.33, "beginning", ifelse(stageTime >= 0.66, "ending", "middle"))) |> 
  ungroup() |> 
  select(-stageTime) |> 
  mutate(tempDiff = NA)

dat0 <- dat
# dat <- dat0

for(s in unique(dat$speaker)){
  for(t in dat$time){
    for(r in dat$ROI){
      currentTemp <- as.numeric(dat$temperature[dat$speaker==s &
                                                  dat$time==t &
                                                  dat$ROI==r])
      otherTemp <- as.numeric(dat$temperature[dat$speaker != s &
                                                substr(dat$speaker, 1, 3) == substr(s, 1, 3) &
                                                dat$time==t &
                                                dat$ROI==r])
      if(!purrr::is_empty(currentTemp)){
        if(!any(is.na(currentTemp))){
          if(!purrr::is_empty(otherTemp)){
            if(!any(is.na(otherTemp))){
              dat$tempDiff[dat$speaker==s & dat$time==t & dat$ROI==r] <- as.numeric(abs(currentTemp - otherTemp))
            }
          }
        }
      }
    }
  }
}

dat <- merge(dat, m, by=c("speaker", "stage"))

save(dat, file=paste0(here::here(), "/data/tempData.RData"))
