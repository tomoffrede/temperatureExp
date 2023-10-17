# Tom Offrede
# Get raw metadata in csv file and clean it
# this code was originally written in `Preprocessing-Speech.R` and now I'm just transferring it here
# if some of it doesn't work, it could be because it was dependent on other code inside `Preprocessing-Speech.R`
# some of it probably doesn't work with newer versions of dplyr

library(tidyverse)

folder <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/"
folderMetadata <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/Data/SpeechData/AllForPreprocessing/"
folderData <- "C:/Users/offredet/Documents/1HU/ExperimentTemperature/temperatureExp/data/"

m <- read.csv(paste0(folderMetadata, "metadata.csv"))
names(m) <- tolower(names(m))
m <- m |> 
  rename(speaker = participant,
         L1 = l1,
         preAcquaintance = preacquaintance,
         comfortPre = comfortpre,
         comfortPost = comfortpost,
         tempPre = temppre,
         tempPost = temppost,
         realTempPre = realtemppre,
         realTempDuring = realtempduring,
         realTempPost = realtemppost) |> 
  mutate(condition = case_when(
    condition == "exp" ~ "close",
    condition == "con" ~ "impersonal",
  ))

f <- read.csv(paste0(folder, "bfi-factors.csv"), sep=";", na.strings = "") |> 
  filter(!is.na(factor)) |> 
  mutate(item = paste0("bfi", item))
i <- m |> 
  select_if(grepl("bfi|speaker", names(.))) |> 
  select_if(!grepl("5|32|37", names(.))) |> # in the paper of the Italian BFI (Fossati et al 2011) they left out those items from the PCA table without mentioning anything, which I find shady
  pivot_longer(cols=c("bfi1":"bfi44"),
               names_to="item",
               values_to="rating") |> 
  mutate(rating = ifelse(rating==7, 5, rating))
bfi <- merge(i, f, by="item") |> 
  mutate(rating = case_when(reversed == "R" ~ 6 - rating,
                            is.na(reversed) ~ rating)) |> 
  group_by(speaker, factor) |> 
  mutate(score = mean(rating, na.rm=TRUE)) |> 
  ungroup() |> 
  mutate(factor = case_when(
    factor == "A" ~ "agreeableness",
    factor == "C" ~ "conscientiousness",
    factor == "E" ~ "extraversion",
    factor == "N" ~ "neuroticism",
    factor == "O" ~ "openness",
  )) |> 
  select(-c(item, rating, reversed)) |> 
  distinct() |>
  pivot_wider(names_from = factor,
              values_from = score)

m <- merge(m, bfi, by="speaker") |> 
  select_if(!grepl("bfi", names(.))) |> 
  rename(privacy = rcit1, # Pensi di aver avuto un'adeguata privacy nella tua conversazione?
         atEase = rcit2, # Ti sei sentito relativamente a tuo agio in questo ambiente di conversazione?
         convMeet = rcit3, # Consideri la conversazione un buon modo per conoscere qualcuno?
         convHabit = rcit4, # Ti impegni spesso in conversazioni simili a quella in cui ti sei appena impegnato?
         friendsQuestions = rcit5, # Pensi che la maggior parte dei tuoi amici farebbe domande simili a quelle poste in questa conversazione?
         convFriends = rcit6, # Pensi che i tuoi amici considerino la conversazione il modo più importante per conoscere qualcuno?
         closeness = rcit7, # Quanto ti senti legato al partecipante con cui stai lavorando a questo studio?
         similarity = rcit8, # Quanto ti senti simile al partecipante con cui stai lavorando a questo studio?
         likeability = rcit9, # Quanto ti piace il partecipante con cui stai lavorando a questo studio?
         becomeFriends = rcit10) # In futuro, in che misura ritieni di poter essere amico del partecipante con cui stai lavorando a questo studio?

save(m, file=paste0(folderData, "metadata-clean.RData"))