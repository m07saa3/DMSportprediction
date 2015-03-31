# stats from historical meetings beetween two teams
# NB! use rollapply for windows-stat. its much faster than loop

# this function return subset from df which include matches beetween team1 and team2
head2head.matches <- function(team1, team2, df, ...) { 
df[ (df$HomeTeam == team1 & df$AwayTeam == team2) |
    (df$HomeTeam == team2 & df$AwayTeam == team1), ]
}

which.home <- function(team, df) {
  x <- rep(NA, nrow(df))
  x[df$HomeTeam == team]  <- TRUE
  x[df$AwayTeam == team]  <- FALSE
}