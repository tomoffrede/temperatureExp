# Tom Offrede
# check the correspondence between DL and C values extracted with Altair
library(tidyverse)
folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/"
files <- list.files(folder, "\\.txt")
files <- files[grepl("C1|C2|DL1|DL2", files)]
f <- files[[1]]

dat <- data.frame(matrix(ncol=0, nrow=(4386)))
dat$index <- 1:nrow(dat)

for(f in files){
  t <- read.table(paste0(folder, f), sep = "\t", header=TRUE, fileEncoding = readr::guess_encoding(paste0(folder, f))$encoding[1])
  if(grepl("C", f)){
    t <- t[,grepl("C", names(t)) & !grepl("px", names(t))]
    names(t) <- gsub("\\.\\.\\.\\.C\\.", "_C", names(t))
  }
  if(grepl("DL", f)){
    t <- t[,grepl("DL", names(t)) & !grepl("px", names(t))]
    names(t) <- gsub("\\.\\.\\.DL\\.", "_DL", names(t))
  }
  t <- t %>% mutate_at(names(t), as.numeric)
  if(nrow(t) < 1000){
    add <- data.frame(matrix(nrow=(4386-928), ncol=ncol(t)))
    names(add) <- names(t)
    t <- rbind(t, add)
  }
  t$index <- 1:nrow(t)
  dat <- merge(dat, t, by="index")
}

dat <- dat %>% 
  select(-index) %>% 
  mutate(diff1 = X1_C - X1_DL,
         diff2 = X2_C - X2_DL,
         diff3 = X3_C - X3_DL,
         diff4 = X4_C - X4_DL,
         diff5 = X5_C - X5_DL,
         diff6 = X6_C - X6_DL,
         diff7 = X7_C - X7_DL,
         diff8 = X8_C - X8_DL,
         diff9 = X9_C - X9_DL,
         diff10 = X10_C - X10_DL,
         diff11 = X11_C - X11_DL,
         diff12 = X12_C - X12_DL,
         diff13 = X13_C - X13_DL,
         diff14 = X14_C - X14_DL,
         diff15 = X15_C - X15_DL,
         diff16 = X16_C - X16_DL,
         diff17 = X17_C - X17_DL,
         prod1 = X1_C - X1_DL,
         prod2 = X2_C - X2_DL,
         prod3 = X3_C - X3_DL,
         prod4 = X4_C - X4_DL,
         prod5 = X5_C - X5_DL,
         prod6 = X6_C - X6_DL,
         prod7 = X7_C - X7_DL,
         prod8 = X8_C - X8_DL,
         prod9 = X9_C - X9_DL,
         prod10 = X10_C - X10_DL,
         prod11 = X11_C - X11_DL,
         prod12 = X12_C - X12_DL,
         prod13 = X13_C - X13_DL,
         prod14 = X14_C - X14_DL,
         prod15 = X15_C - X15_DL,
         prod16 = X16_C - X16_DL,
         prod17 = X17_C - X17_DL)

