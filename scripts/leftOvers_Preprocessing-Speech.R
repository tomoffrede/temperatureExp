# Tom Offrede
# leftover code from Preprocessing-Speech.R


#####

# original for loop that saves the f0 of all previous IPUs of the previous turn
# (so one row will contain, e.g., for a given turn: f0 of 1st IPU of current turn and f0 of 1st IPU of previous turn; f0 of 2nd IPU of turn and f0 of 2nd IPU of previous turn etc)

for(i in 1:nrow(dat)){
  prevf0mean <- dat$f0mean[dat$file == dat$file[i] &
                             dat$speaker != dat$speaker[i] &
                             dat$turn == dat$prevTurn[i] &
                             dat$IPU == dat$IPU[i]]
  prevf0med <- dat$f0med[dat$file == dat$file[i] &
                           dat$speaker != dat$speaker[i] &
                           dat$turn == dat$prevTurn[i] &
                           dat$IPU == dat$IPU[i]]
  prevf0sd <- dat$f0sd[dat$file == dat$file[i] &
                         dat$speaker != dat$speaker[i] &
                         dat$turn == dat$prevTurn[i] &
                         dat$IPU == dat$IPU[i]]
  prevf0max <- dat$f0max[dat$file == dat$file[i] &
                           dat$speaker != dat$speaker[i] &
                           dat$turn == dat$prevTurn[i] &
                           dat$IPU == dat$IPU[i]]
  prevTurnf0mean <- unique(dat$turnf0mean[dat$file == dat$file[i] &
                                            dat$speaker != dat$speaker[i] &
                                            dat$turn == dat$prevTurn[i]])
  prevTurnf0med <- unique(dat$turnf0med[dat$file == dat$file[i] &
                                          dat$speaker != dat$speaker[i] &
                                          dat$turn == dat$prevTurn[i]])
  prevTurnf0sd <- unique(dat$turnf0sd[dat$file == dat$file[i] &
                                        dat$speaker != dat$speaker[i] &
                                        dat$turn == dat$prevTurn[i]])
  prevTurnf0max <- unique(dat$turnf0max[dat$file == dat$file[i] &
                                          dat$speaker != dat$speaker[i] &
                                          dat$turn == dat$prevTurn[i]])
  if(!purrr::is_empty(prevf0mean)){
    if(!any(is.na(prevf0mean))){
      dat$prevf0mean[i] <- prevf0mean
    }
  }
  if(!purrr::is_empty(prevf0med)){
    if(!any(is.na(prevf0med))){
      dat$prevf0med[i] <- prevf0med
    }
  }
  if(!purrr::is_empty(prevf0sd)){
    if(!any(is.na(prevf0sd))){
      dat$prevf0sd[i] <- prevf0sd
    }
  }
  if(!purrr::is_empty(prevf0max)){
    if(!any(is.na(prevf0max))){
      dat$prevf0max[i] <- prevf0max
    }
  }
  if(!purrr::is_empty(prevTurnf0mean)){
    if(!any(is.na(prevTurnf0mean))){
      dat$prevTurnf0mean[i] <- prevTurnf0mean
    }
  }
  if(!purrr::is_empty(prevTurnf0med)){
    if(!any(is.na(prevTurnf0med))){
      dat$prevTurnf0med[i] <- prevTurnf0med
    }
  }
  if(!purrr::is_empty(prevTurnf0sd)){
    if(!any(is.na(prevTurnf0sd))){
      dat$prevTurnf0sd[i] <- prevTurnf0sd
    }
  }
  if(!purrr::is_empty(prevTurnf0max)){
    if(!any(is.na(prevTurnf0max))){
      dat$prevTurnf0max[i] <- prevTurnf0max
    }
  }
}

#####
# attempting to save an `overallTurn`, i.e. turn from 1 to n() for each speaker, across all tasks (Lists and Diapix)
# This was back when I was dividing tasks as L1-L3, D1, D2 (now just: Lists and Diapix)

dat0 <- data.frame(matrix(nrow=0, ncol=5))
names(dat0) <- c("speaker", "task", "turn", "IPU", "overallTurn")

for(s in unique(dat$speaker)){ # the warnings received (no non-missing arguments to max) are because FWR's D1 files and MJG's L3 files are missing
  l1 <- max(dat$turn[dat$speaker==s & dat$task=="L1"])
  l2 <- max(dat$turn[dat$speaker==s & dat$task=="L2"] + l1)
  l3 <- max(dat$turn[dat$speaker==s & dat$task=="L3"] + l2)
  d1 <- max(dat$turn[dat$speaker==s & dat$task=="D1"] + l3)
  current <- dat %>%
    filter(speaker==s) %>%
    select(speaker, task, turn, IPU) %>%
    mutate(overallTurn = case_when(
      task=="L1" ~ turn,
      task=="L2" ~ turn + l1,
      task=="L3" ~ turn + l2,
      task=="D1" ~ turn + l3,
      task=="D2" ~ turn + d1
    ))
  dat0 <- rbind(dat0, current)
}

dat <- merge(dat, dat0, by=c("speaker", "task", "turn", "IPU"))

#####

