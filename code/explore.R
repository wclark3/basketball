# next...
# relevant game info is collected, now build sparse matrices for players and team/seasons

# A statistic in basketball defined as the time a team gains 
# offensive possession of the ball until it scores, loses the ball, 
# or commits a violation or foul

rm(list = ls())

require(dplyr)

ProcessGame <- function(fname, season) {
  
  dat <- tbl_df(read.csv(fname))
  
  # home/away team inferred from name of data file
  fname.split <- strsplit(fname, "-")[[1]]
  team.names <- fname.split[length(fname.split)]
  team.names <- strsplit(team.names, "@")[[1]]
  team.away <- team.names[1]
  team.home <- team.names[2]
  team.home <- gsub(".csv", "", team.home)
  
  # home/away team denotes seasons as well
  team.away <- paste(team.away, season, sep = ".")
  team.home <- paste(team.home, season, sep = ".")
  
  dat$team.away <- rep(team.away, nrow(dat))
  dat$team.home <- rep(team.home, nrow(dat))
  dat$home.pm <- dat$home_score - dat$away_score
  
  goals <- list()
  players <- list()
  
  line.start <- 1
  poss <- 1
  for(r in 2:nrow(dat)) {
    
    line1 <- select(dat[r-1,], a1, a2, a3, a4, a5, h1, h2, h3, h4, h5)
    line2 <- select(dat[r,], a1, a2, a3, a4, a5, h1, h2, h3, h4, h5)
    
    if(dat$event_type[r] == "turnover" | 
       dat$event_type[r] == "shot" |
       (dat$event_type[r] == "free throw" & dat$num[r] == dat$outof[r]) |
       dat$event_type[r] == "foul" | dat$event_type[r] == "violation" |
       dat$event_type[r] == "end of period") {
      poss <- poss + 1
    }
    
    if((sum(line1 != line2) > 0) | (r == nrow(dat))) { # true if there has been a line change
      goals[[length(goals)+1]] <- cbind(dat$home.pm[r-1] - dat$home.pm[line.start],
                                        poss, select(dat[r-1,], team.away, team.home, period))
      players[[length(players)+1]] <- line1
      line.start <- r
      poss <- 1
    }
    
  }
  
  # convert list to data.frame
  goals <- do.call(rbind, goals)
  colnames(goals)[1] <- "home.pm"
  players <- do.call(rbind, players)
  
  return(list(goals, players))
}

season.dir <- "../data/2016-17"
game.files <- list.files(season.dir)

goals <- list()
players <- list()

for(i in 2:4) {
  season <- gsub("-", ".", strsplit(season.dir, "/")[[1]][3])
  fname <- paste(season.dir, game.files[i], sep = "/")
  game.info <- ProcessGame(fname, season)
  print(fname)
  
  goals[length(goals)+1] <- game.info[1]
  players[length(players)+1] <- game.info[2]
}

goals <- do.call(rbind, goals)
players <- do.call(rbind, players)


