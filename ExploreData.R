# Explore data from thermal camera

folder <- "C:/Users/tomof/Documents/1HU/ExperimentTemperature/ExploreData/"

d <- read.table(paste0(folder, "Data1.txt"), sep = "\t", header=TRUE)
names(d) <- c("frame", "upMean", "upSD", "bottomMean", "bottomSD", "trigger", "audioA", "audioB")

par(mfrow=c(2,2))
plot(d$upSD, type="l");plot(d$upMean, type="l")
plot(d$bottomSD, type="l");plot(d$bottomMean, type="l")

plot(d$trigger, type="l")


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