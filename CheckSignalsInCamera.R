# see if recording made on 5.4.22 saved the microphones' and trigger data well

folder <- "C:/Users/tomof/Documents/1HU/ExperimentTemperature/"

d <- read.table(paste0(folder, "test1422.txt"), header=TRUE, sep='\t')
names(d) <- c("frame", "trigger", "mic1", "mic2")

plot(d$trigger, type="l")

# seems that it was saved wrong? investigate.