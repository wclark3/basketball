rm(list = ls())

require(dplyr)
require(gamlr)

dat <- tbl_df(read.csv("../data/sample-combined-pbp-logs.csv"))

# construct game data ----
# rows = line changes, columns = home plus minus | home players | away players

game_dat <- data.frame(home_pm = integer(), n_poss = integer(),
                       h1 = factor(), h2 = factor(),
                       h3 = factor(), h4 = factor(),
                       h5 = factor(), a1 = factor(),
                       a2 = factor(), a3 = factor(),
                       a4 = factor(), a5 = factor())

line_start <- 1
for(r in 2:nrow(dat)) {
  
  line_1 <- select(dat[r-1,], h1, h2, h3, h4, h5, a1, a2, a3, a4, a5)
  line_2 <- select(dat[r,], h1, h2, h3, h4, h5, a1, a2, a3, a4, a5)
  
  if(sum(line_1!=line_2) > 0) { # i.e. there is a line change
    home_pm <- (dat$home_score[(r-1)] - dat$away_score[(r-1)]) - 
      (dat$home_score[line_start] - dat$away_score[line_start])
    
    n_poss <- (r - line_start)
    
    game_row <- cbind(home_pm, n_poss,
                      select(dat[r-1,], h1, h2, h3, h4, h5, a1, a2, a3, a4, a5))
    game_dat <- rbind(game_dat, game_row)
    line_start <- r
  }
  
}

# construct player matrix ----

n_poss <- nrow(game_dat)

players <- unique(unlist(select(game_dat, a1, a2, a3, a4, a5, h1, h2, h3, h4, h5)))
n_players <- length(players)

player_mat <- matrix(data = 0, nrow = n_poss, ncol = n_players)
colnames(player_mat) <- players

for(r in 1:nrow(game_dat)) {
  
  home_players <- select(game_dat[r,], h1, h2, h3, h4, h5)
  away_players <- select(game_dat[r,], a1, a2, a3, a4, a5)
  
  player_mat[r,colnames(player_mat) %in% unlist(home_players)] <- 1
  player_mat[r,colnames(player_mat) %in% unlist(away_players)] <- (-1)
  
}

# run regression ----
# need more data from gamlr I believe

fit <- gamlr(x = player_mat, y = game_dat$home_pm, family = "binomial",
             weights = game_dat$n_poss)
