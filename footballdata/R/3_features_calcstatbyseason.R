# get current number of wins, draws and losses
# for all, home and away season's matches
#
# Input: birfucated dataframe (see example below)
#
# raw.seasons <- downloadRangeSeason(league = "E", division = "0", range.start.years = 2011:2014)
# dataset <- getForkedDataset(raw.seasons)
# season.match.results <- getSeasonMatchResults(dataset)
getSeasonMatchResults <- function(df) {
  
  season.match.results <- as.data.frame(matrix(0, nrow = nrow(df), ncol = 12))
  colnames(season.match.results) <- c("current_wins_all", "current_draws_all", "current_loses_all", "n_games_all",
                                      "current_wins_home", "current_draws_home", "current_loses_home", "n_games_home",
                                      "current_wins_away", "current_draws_away", "current_loses_away", "n_games_away")
  
  seasons <- unique(df$Season)
  for (season in seasons) {
    teams <- unique(df$Team[df$Season == season])
    for (team in teams) {
      
      #############################
      # For all matches in season #
      #############################
      
      id.matches.all <- which(df$Season == season & df$Team == team)
      res.matches.all <- df$FTR[id.matches.all]
      res.matches.all <- as.numeric(mapvalues(res.matches.all, c("D", "W", "L"), c(1, 3, 0)))
      
      total.wins.all <- c(0, cumsum(res.matches.all == 3)[-length(res.matches.all)])
      total.draws.all <- c(0, cumsum(res.matches.all == 1)[-length(res.matches.all)])
      total.losses.all <- c(0, cumsum(res.matches.all == 0)[-length(res.matches.all)])
      
      season.match.results$current_wins_all[id.matches.all] <- total.wins.all
      season.match.results$current_draws_all[id.matches.all] <- total.draws.all
      season.match.results$current_loses_all[id.matches.all] <- total.losses.all
      season.match.results$n_games_all[id.matches.all] <- seq_along(id.matches.all) - 1
      
      
      ##################################
      # For all home matches in season #
      ##################################
          
      total.wins.home <- c(0, cumsum(res.matches.all == 3 & df$home_away[id.matches.all] == "Home")[-length(res.matches.all)])
      total.draws.home <- c(0, cumsum(res.matches.all == 1 & df$home_away[id.matches.all] == "Home")[-length(res.matches.all)])
      total.losses.home <- c(0, cumsum(res.matches.all == 0 & df$home_away[id.matches.all] == "Home")[-length(res.matches.all)])
      total.games.home <- c(0, cumsum(df$home_away[id.matches.all] == "Home")[-length(res.matches.all)])
      
      # write to base (HOME)
      season.match.results$current_wins_home[id.matches.all] <- total.wins.home
      season.match.results$current_draws_home[id.matches.all] <- total.draws.home
      season.match.results$current_loses_home[id.matches.all] <- total.losses.home
      season.match.results$n_games_home[id.matches.all] <- total.games.home
      
      
      ##################################
      # For all away matches in season #
      ##################################
      
      total.wins.away <- c(0, cumsum(res.matches.all == 3 & df$home_away[id.matches.all] == "Away")[-length(res.matches.all)])
      total.draws.away <- c(0, cumsum(res.matches.all == 1 & df$home_away[id.matches.all] == "Away")[-length(res.matches.all)])
      total.losses.away <- c(0, cumsum(res.matches.all == 0 & df$home_away[id.matches.all] == "Away")[-length(res.matches.all)])
      total.games.away <- c(0, cumsum(df$home_away[id.matches.all] == "Away")[-length(res.matches.all)])
      
      # write to base (AWAY)
      season.match.results$current_wins_away[id.matches.all] <- total.wins.away
      season.match.results$current_draws_away[id.matches.all] <- total.draws.away
      season.match.results$current_loses_away[id.matches.all] <- total.losses.away
      season.match.results$n_games_away[id.matches.all] <- total.games.away
      
    }
    
  }
  season.match.results
}
