# Explore data from thermal camera

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


folder <- "C:/Users/tomof/Documents/1HU/ExperimentTemperature/ExploreData/"


################# Data1
d <- read.table(paste0(folder, "Data1.txt"), sep = "\t", header=TRUE)
names(d) <- c("frame", "upMean", "upSD", "bottomMean", "bottomSD", "trigger", "audioA", "audioB")

par(mfrow=c(2,2))
plot(d$upSD, type="l");plot(d$upMean, type="l")
plot(d$bottomSD, type="l");plot(d$bottomMean, type="l")

plot(d$trigger, type="l")


############### Data2
t <- read.table(paste0(folder, "Data2.txt"), sep = "\t", header=TRUE)
names(t) <- c("frame", "upMean", "upSD", "bottomMean", "bottomSD", "trigger", "audioA", "audioB", "bellyMean", "bellySD", "faceMean", "faceSD")

par(mfrow=c(2,2))
plot(t$upSD, type="l");plot(t$upMean, type="l")
plot(t$bottomSD, type="l");plot(t$bottomMean, type="l")

par(mfrow=c(1,1))
plot(t$trigger, type="l")

### select only periods during trigger

# 1st trigger:
# 8123 > 13228 (diff: 5105)
# 10327 > 5215 (diff: 5112)
# 
# 2nd trigger:
# 8122 > 13234 (diff: 5112)
# 10815 > 5701 (diff: 5114)
# (in the real data the values change a lot more because the sections last longer, so we have to work with the differences, not absolute values)

t$section <- NA
count <- 0
for(i in 2:nrow(t)){
  if(t$trigger[i] - t$trigger[i-1] > 4000){
    count <- count + 1 
    t$section[i] <- paste0("StartTrial", count)
  } else if(t$trigger[i] - t$trigger[i-1] < -4000){
    t$section[i-1] <- paste0("EndTrial", count)
  }
}

t$section[t$section=="StartTrial1"] <- "List1" # example of what a name of a section could look like
t$section[t$section=="EndTrial1"] <- "EndList1"
t$section[t$section=="StartTrial2"] <- "Diapix"
t$section[t$section=="EndTrial2"] <- "EndDiapix"

for(i in 2:nrow(t)){
  if(!is.na(t$section[i-1]) && !(grepl("End", t$section[i])) && !grepl("End", t$section[i-1])){ # I tried directly `if(t$section[i-1]=="List1")`, but it said "Missing value where TRUE/FALSE needed"
    t$section[i] <- "Trial"
  } else if(!is.na(t$section[i-1]) && !(grepl("End", t$section[i])) && !grepl("End", t$section[i-1])){
    t$section[i] <- "Trial"
  }
}
# now rename them:
for(i in 2:nrow(t)){
    t$section[t$section %in% c("Trial", "EndList1") & t$section[i-1]=="List1"] <- "List1"
    t$section[t$section %in% c("Trial", "EndDiapix") & t$section[i-1]=="Diapix"] <- "Diapix"
}

# now you can exclude the data that isn't within the sections

ts <- t[!is.na(t$section),]

par(mfrow=c(4,2))
plot(t$upSD, type="l");plot(t$upMean, type="l")
plot(t$bottomSD, type="l");plot(t$bottomMean, type="l")
plot(t$bellySD, type="l");plot(t$bellyMean, type="l")
plot(t$faceSD, type="l");plot(t$faceMean, type="l")

############ entireTest

s <- read.table(paste0(folder, "entireTest.txt"), sep = "\t", header=TRUE)
names(s) <- c("frame", "bottomMean", "bottomSD", "upMean", "upSD", "trigger", "audioA", "audioB")

# par(mfrow=c(4,2))
# plot(s$upSD, type="l");plot(s$upMean, type="l")
# plot(s$bottomSD, type="l");plot(s$bottomMean, type="l")
# plot(s$audioA, type="l"); plot(s$audioB, type="l")
# plot(s$trigger)

s$section <- NA
count <- 0
for(i in 2:nrow(s)){
  if(s$trigger[i] - s$trigger[i-1] > 4000){
    count <- count + 1 
    s$section[i] <- paste0("StartTrial", count)
  } else if(s$trigger[i] - s$trigger[i-1] < -4000){
    s$section[i-1] <- paste0("EndTrial", count)
  }
}

s$section[s$section=="StartTrial1"] <- "List1" # example of what a name of a section could look like
s$section[s$section=="EndTrial1"] <- "EndList1"
s$section[s$section=="StartTrial2"] <- "List2"
s$section[s$section=="EndTrial2"] <- "EndList2"
s$section[s$section=="StartTrial3"] <- "List3"
s$section[s$section=="EndTrial3"] <- "EndList3"
s$section[s$section=="StartTrial4"] <- "Diapix1"
s$section[s$section=="EndTrial4"] <- "EndDiapix1"
s$section[s$section=="StartTrial5"] <- "Diapix2"
s$section[s$section=="EndTrial5"] <- "EndDiapix2"

for(i in 2:nrow(s)){
  if(!is.na(s$section[i-1]) && !(grepl("End", s$section[i])) && !grepl("End", s$section[i-1])){ # I tried directly `if(t$section[i-1]=="List1")`, but it said "Missing value where TRUE/FALSE needed"
    s$section[i] <- "Trial"
  } else if(!is.na(s$section[i-1]) && !(grepl("End", s$section[i])) && !grepl("End", s$section[i-1])){
    s$section[i] <- "Trial"
  }
}

# before renaming all the frames (in the sections), maybe it'll make it a bit faster to first exclude all the irrelevant frames:

ss <- s[!is.na(s$section),]

# or do I maybe want to keep the entire recording? and then mark them as "conversation" vs "waiting"

# now rename them: ----- not working! almost all being named as List1. why???
for(i in 2:nrow(ss)){
  ss$section[ss$section[i] %in% c("Trial", "EndList1") & ss$section[i-1]=="List1"] <- "List1"
  ss$section[ss$section[i] %in% c("Trial", "EndList2") & ss$section[i-1]=="List2"] <- "List2"
  ss$section[ss$section[i] %in% c("Trial", "EndList3") & ss$section[i-1]=="List3"] <- "List3"
  ss$section[ss$section[i] %in% c("Trial", "EndDiapix1") & ss$section[i-1]=="Diapix1"] <- "Diapix1"
  ss$section[ss$section[i] %in% c("Trial", "EndDiapix2") & ss$section[i-1]=="Diapix2"] <- "Diapix2"
}



#################### test1-20.4.22
s <- read.table(paste0(folder, "test1-20.4.22.txt"), sep = "\t", header=TRUE)
names(s) <- c("frame", "trigger", "Mean1", "SD1", "Mean2", "SD2", "Mean3", "SD3", "audioA", "audioB")

par(mfrow=c(5,2))
plot(s$SD1, type="l");plot(s$Mean1, type="l")
plot(s$SD2, type="l");plot(s$Mean2, type="l")
plot(s$SD3, type="l");plot(s$Mean3, type="l")
plot(s$audioA, type="l"); plot(s$audioB, type="l")
plot(s$trigger)

###############################

#### data from an entire area (pixel by pixel)

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