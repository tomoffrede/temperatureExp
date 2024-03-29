# Tom Offrede
# Read TXT files with temperature information from face ROIs and plot them
# C: cheek
# E: eye
# F: forehead
# M: mouth
# N: nouse
# 1: left
# 2: right

library(tidyverse)
library(readr)

`%!in%` <- Negate(`%in%`)

# specify the folder where all the files with the temperature data are
# make sure that the name of the folder ends with "/"
folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/"
folderTXT <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/TempData/"

# files <- list.files(folderTXT, "\\.txt")
files <- list.files(folderTXT, "example")
files <- files[files!="KPB-11.txt"]

### see if all the files are named in a good way (that can be parsed easily)

# f <- files
# f <- gsub("\\.txt", "", f)
# table(nchar(f))
# table(substr(f, 1, 3)) # two of them have 24 instead of 25 frames
# table(substr(f, 4, 4))
# table(substr(f, 5, 6)) # there are fewer of frames 1, 2, 3, 4, 5, 6, 7, 8, 9, 25 ??????

###

dat <- data.frame(matrix(nrow=0, ncol=5))
names(dat) <- c("speaker", "frame", "ROI", "temperature", "Tz")

wrong <- data.frame(matrix(ncol=2, nrow=0))
names(wrong) <- c("file", "column")

# files <- files[grepl("SUK", files)]
# f <- files[[1]]

for(f in files){
  a <- read.table(paste0(folderTXT, f), sep = "\t", header=TRUE, fileEncoding = guess_encoding(paste0(folderTXT, f))$encoding[1])
  a <- a[,grepl("C", names(a)) & !grepl("px", names(a))]
  names(a) <- gsub("\\.\\.\\.\\.C\\.", "", names(a))
  # names(a) <- gsub("....C.", "", names(a))
  a <- a %>% mutate_at(names(a), as.numeric)
  if(nchar(f) == 9){ # if it's frame [1-9], put a zero before it, so that all files have 10 characters (counting with ".txt")
    f <- gsub("-", "-0", f)
  }
  
  tempA <- data.frame(matrix(nrow=0, ncol=5))
  names(tempA) <- c("speaker", "frame", "ROI", "temperature", "Tz") # Tz is the temperature in z scores
  tempB <- data.frame(matrix(nrow=0, ncol=5))
  names(tempB) <- c("speaker", "frame", "ROI", "temperature", "Tz") # Tz is the temperature in z scores
  w <- NA
  for(i in 1:ncol(a)){
    if(substr(names(a[i]), nchar(names(a[i])), nchar(names(a[i]))) %!in% c("A", "B") || substr(names(a[i]), 1, 1) %!in% c("F", "E", "N", "C", "M")){ # some columns were named wrong and contain only numbers. Here I try to correct them.
      wrong[nrow(wrong)+1,] <- c(f, names(a[i]))
    }

    frame <- substr(f, 5, 6)
    sp <- paste0(substr(f, 1, 4), substr(names(a[i]), nchar(names(a[i])), nchar(names(a[i]))))
    
    o <- sort(a[,i], decreasing=TRUE)
    o <- o[1:(length(o)*0.1)] # if you want a different amount of pixels (instead of 10%), just change the 0.1 here
    
    if(substr(sp, nchar(sp), nchar(sp)) == "A"){
      tempA[nrow(tempA)+1,] <- c(sp, frame, gsub("\\..*", "", names(a[i])), as.numeric(mean(o)), NA)
    }
    if(substr(sp, nchar(sp), nchar(sp)) == "B"){
      tempB[nrow(tempB)+1,] <- c(sp, frame, gsub("\\..*", "", names(a[i])), as.numeric(mean(o)), NA)
    }
  }
  
  temp <- list(tempA, tempB)
  
  for(t in 1:length(temp)){
    if(nrow(temp[[t]]) > 1){
      temp[[t]]$temperature <- as.numeric(temp[[t]]$temperature)
      temp[[t]] <- temp[[t]] %>%
        mutate(Tz = (temperature - mean(temperature)) / sd(temperature)) %>%
        filter(abs(Tz) < 2.5)
      sp <- unique(temp[[t]]$speaker)
      temp[[t]][nrow(temp[[t]])+1,] <- c(sp, frame, "Face", mean(temp[[t]]$temperature, na.rm=TRUE), NA)
    }
  }
  
  temp0 <- rbind(temp[[1]], temp[[2]])
  dat <- rbind(dat, temp0)
  
  # f <- paste0(substr(f, 1, 4), as.numeric(frame) + 1, ".txt")
}

dat <- dat %>%
  mutate_at(c("speaker", "ROI"), as.factor) %>%
  mutate_at("frame", as.integer) %>%
  mutate_at(c("temperature", "Tz"), as.numeric)

# join with metadata

m0 <- read.csv(paste0(folder, "metadata.csv"))
m <- m0 %>% # delete all the personality questions, we aren't looking at it now
  select(-c(BFI1:BFI44))

m$RCIT <- (m$RCIT7 + m$RCIT8 + m$RCIT9 + m$RCIT10) / 4 # calculate one score for the rating questions of Relationship Closeness Induction Task
m <- m %>% 
  select(-c(RCIT1:RCIT10)) %>%
  rename(speaker = Participant)

dam <- merge(dat, m, by="speaker")

daz <- dam %>% 
  group_by(speaker) %>% 
  mutate(Tz = (temperature - mean(temperature, na.rm=TRUE))/sd(temperature, na.rm=TRUE)) %>% 
  ungroup

# Plots

folder1 <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/Plots/ROIPerSubject-Raw-NoExclusion/"
folder3 <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/Plots/ROIPerSubject-Raw-NoOutliers/"
folder4 <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/Plots/SubjectPerROI-Raw-NoExclusion/"
folder6 <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/Plots/SubjectPerROI-Raw-NoOutliers/"

# subject per ROI, raw temperature, no outliers excluded

dam <- dam0

for(s in unique(dam$speaker)){
  if(unique(dam$Condition[dam$speaker==s]) == "con"){
    con <- "Condition Impersonal"
  } else if(unique(dam$Condition[dam$speaker==s]) == "exp"){
    con <- "Condition Personal"
  }
  rcit <- paste0("Questionnaire: ", unique(dam$RCIT[dam$speaker==s]))
  if(grepl("Impersonal", con)){
    titlecolor <- "red"
  } else {titlecolor <- "blue"}
  ggplot(dam %>% filter(speaker == s), aes(frame, temperature)) +
    geom_point()+
    geom_line()+
    ggtitle(paste(s, con, rcit, sep = ", "))+
    facet_wrap(~ROI)+
    theme(title = element_text(color = titlecolor))
  ggsave(paste0(folder4, s, ".png"))
}

# subject per ROI, raw temperature, outliers excluded

for(s in unique(daz$speaker)){
  if(unique(daz$Condition[daz$speaker==s]) == "con"){
    con <- "Condition Impersonal"
  } else if(unique(daz$Condition[daz$speaker==s]) == "exp"){
    con <- "Condition Personal"
  }
  rcit <- paste0("Questionnaire: ", unique(daz$RCIT[daz$speaker==s]))
  if(grepl("Impersonal", con)){
    titlecolor <- "red"
  } else {titlecolor <- "blue"}
  ggplot(daz %>% filter(speaker == s, abs(Tz) < 2), aes(frame, temperature)) +
    geom_point()+
    geom_line()+
    ggtitle(paste(s, con, rcit, sep = ", "))+
    facet_wrap(~ROI)+
    theme(title = element_text(color = titlecolor))
  ggsave(paste0(folder6, s, ".png"))
}


dam0 <- dam

dam <- dam %>%
  mutate(Condition = ifelse(Condition == "con", "Impersonal", "Personal")) %>% 
  group_by(speaker) %>% 
  mutate(speaker = paste(speaker, unique(RCIT), sep="-")) %>% 
  ungroup %>% 
  filter(ROI != "Face")

# ROI per subject , raw temperature, no outliers excluded

for(r in unique(dam$ROI)){
  ggplot(dam %>% filter(ROI == r), aes(frame, temperature)) +
    geom_point(aes(color=Condition))+
    geom_line(aes(color=Condition))+
    ggtitle(paste(r))+
    facet_wrap(~speaker)
  ggsave(paste0(folder1, r, ".png"))
}

# ROI per subject, raw temperature, outliers excluded

for(r in unique(dam$ROI)){
  ggplot(dam %>% filter(ROI == r, abs(Tz) < 2), aes(frame, temperature)) +
    geom_point(aes(color=Condition))+
    geom_line(aes(color=Condition))+
    ggtitle(paste(r))+
    facet_wrap(~speaker)
  ggsave(paste0(folder3, r, ".png"))
}

dam1 <- dam

dam <- dam1

dam <- dam %>% 
  group_by(speaker) %>% 
  mutate(Tz = (temperature - mean(temperature, na.rm=TRUE))/sd(temperature, na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(abs(Tz) < 2) %>% 
  group_by(ROI, frame) %>% 
  mutate(tempMeanAll = mean(temperature)) %>% 
  ungroup() %>% 
  group_by(ROI, Condition, frame) %>% 
  mutate(tempMeanCond = mean(temperature)) %>% 
  ungroup()
  

# Average of all participants for each ROI
for(r in unique(dam$ROI)){
  ggplot(dam %>% filter(ROI == r), aes(frame, tempMeanAll)) +
    geom_point()+
    geom_line()+
    ggtitle(paste(r))
  ggsave(paste0(folder3, r, "-AllParticipants.png"))
}


# Average of all participants of each condition for each ROI
for(r in unique(dam$ROI)){
  ggplot(dam %>% filter(ROI == r), aes(frame, tempMeanCond)) +
    geom_point(aes(color=Condition))+
    geom_line(aes(color=Condition))+
    ggtitle(paste(r))
  ggsave(paste0(folder3, r, "-AllParticipantsWithCondition.png"))
}

