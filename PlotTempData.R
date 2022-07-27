# Tom Offrede
# Read TXT files with temperature information from face ROIs and plot them
# C: cheek
# E: eye
# F: forehead
# M: mouth
# N: nouse
# 1: left
# 2: right

# code taken from script ExploreData.R

# Bit of code taken from https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/
# to install and load all necessary packages

## First specify the packages of interest
packages = c("tidyverse")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
# end

# Start of my own code

# specify the folder where all the files with the temperature data are
# make sure that the name of the folder ends with "/"
folder2 <- "C:/Users/tomof/Documents/1HU/ExperimentTemperature/Data/FWR/"

files <- list.files(folder2)

dat <- data.frame(matrix(nrow=0, ncol=5))
names(dat) <- c("speaker", "frame", "ROI", "temperature", "Tz")

for(f in files){
  a <- read.table(paste0(folder2, f), sep = "\t", header=TRUE)
  a <- a[,grepl("C", names(a)) & !grepl("px", names(a))]
  names(a) <- gsub("....C.", "", names(a))
  a <- a %>% mutate_at(names(a), as.numeric)
  
  temp <- data.frame(matrix(nrow=0, ncol=5))
  names(temp) <- c("speaker", "frame", "ROI", "temperature", "Tz") # Tz is the temperature in z scores
  
  for(i in 1:ncol(a)){
    frame <- substr(f, 5, 6)
    frame <- gsub("\\.", "", frame)
    sp <- paste0(substr(f, 1, 3), "-", substr(names(a[i]), nchar(names(a[i])), nchar(names(a[i]))))
    
    o <- sort(a[,i], decreasing=TRUE)
    o <- o[1:(length(o)*0.1)] # if you want a different amount of pixels (instead of 10%), just change the 0.1 here
    temp[nrow(temp)+1,] <- c(sp, frame, gsub("\\..*", "", names(a[i])), as.numeric(mean(o)), NA)
    temp$temperature <- as.numeric(temp$temperature)
    if(i < ncol(a)){
      nextsp <- paste0(substr(f, 1, 3), "-", substr(names(a[i+1]), nchar(names(a[i+1])), nchar(names(a[i+1]))))
      if(nextsp != sp){ # if this is the last ROI of the current speaker for this frame, calculate the mean temperature of all ROIs
        temp <- temp %>%
          mutate(Tz = (temperature - mean(temperature)) / sd(temperature)) %>%
          filter(abs(Tz) < 2)
        temp[nrow(temp)+1,] <- c(sp, frame, "Face", mean(temp$temperature[temp$speaker==sp]), NA)
      }
    } else if(i == ncol(a)){
      temp <- temp %>%
        mutate(Tz = (temperature - mean(temperature)) / sd(temperature)) %>%
        filter(abs(Tz) < 2)
      temp[nrow(temp)+1,] <- c(sp, frame, "Face", mean(temp$temperature[temp$speaker==sp]), NA)
    }
  }
  dat <- rbind(dat, temp)
}

dat <- dat %>%
  mutate_at(c("speaker", "ROI"), as.factor) %>%
  mutate_at("frame", as.integer) %>%
  mutate_at(c("temperature", "Tz"), as.numeric)

ggplot(dat, aes(frame, temperature))+
  geom_point()+
  geom_line()+
  facet_grid(speaker~ROI)

## If you want to plot the data of only one speaker, do this:
## Delete the number sign (#) from the beginning of the lines below with code
## 1. Change for the code of the speaker

# dat <- dat %>% filter(speaker=="XXX")

## 2. Run the ggplot() code again

# ggplot(dat, aes(frame, temperature))+
#   geom_point()+
#   geom_line()+
#   facet_grid(speaker~ROI)