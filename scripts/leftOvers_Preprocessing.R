# Tom Offrede

# Leftovers from Preprocessing
# (I don't necessarily know what it all does)

f <- files[[1]]
what <- data.frame(matrix(nrow=0, ncol=2))
names(what) <- c("file", "ok")
for(f in files){
  a <- read.table(paste0(folderTXT, f), sep = "\t", header=TRUE, fileEncoding = readr::guess_encoding(paste0(folderTXT, f))$encoding[1])
  what[nrow(what)+1,] <- c(f, ifelse(nrow(a)>0, "ok!", "NO!!"))
}
g <- readLines(paste0(folderTXT,gf))
b <- readLines(paste0(folderTXT,bf))

####

# check if "time" is always saved in the same line in all txt files

check <- data.frame(matrix(nrow=0, ncol=2))
names(check) <- c("file", "ok")

for(f in files){
  lines <- readLines(paste0(folderTXT, f))
  if(grepl("time", lines[6])){
    check[nrow(check)+1,] <- c(f, "ok")
  } else{check[nrow(check)+1,] <- c(f, "no!")}
}

# check if the format of all time stamps is the same

for(f in files){
  lines <- readLines(paste0(folderTXT, f))
  check[nrow(check)+1,] <- c(f, str_split(lines[6], fixed("\t"))[[1]][[2]])
}

####

# check all ROIs from all files and see if they're all named properly

roi <- data.frame(matrix(nrow=0, ncol=2))
names(roi) <- c("file", "ROI")
for(f in files){
  lines <- readLines(paste0(folderTXT, f))
  writeLines(lines[21:length(lines)], paste0(folderNew, f))
  
  a <- read.table(paste0(folderNew, f), sep = "\t", header=TRUE, fileEncoding = readr::guess_encoding(paste0(folderNew, f))$encoding[1]) |> 
    select(contains("C") & !contains("px")) |> 
    mutate_all(as.numeric)
  names(a) <- gsub("\\.\\.\\.\\.C\\.", "", names(a))
  for(col in names(a)){
    roi[nrow(roi)+1,] <- c(f, col)
  }
}

####

# check if the temperature unit is celsius in all files

celsius <- data.frame(matrix(nrow=0, ncol=2))
names(celsius) <- c("file", "ok")
for(f in files){
  lines <- readLines(paste0(folderTXT, f))
  if(grepl("°C", str_split(lines[13], fixed("\t"))[[1]][[1]])){
    celsius[nrow(celsius)+1,] <- c(f, "ok")
  } else{celsius[nrow(celsius)+1,] <- c(f, "no!!!")}
}

####

