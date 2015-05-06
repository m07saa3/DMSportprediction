# get current number of wins, draws and losses
# for all, home and away season's matches
#
# Input: birfucated dataframe (see example below)
#
#

getSeasonMatchResults <- function(df) {
  
  season.match.results <- as.data.frame(matrix(0, nrow = nrow(df), ncol = 13))
  colnames(season.match.results) <- c("current_wins_all", "current_draws_all", "current_loses_all", "n_games_all",
                                      "current_wins_home", "current_draws_home", "current_loses_home", "n_games_home",
                                      "current_wins_away", "current_draws_away", "current_loses_away", "n_games_away",
                                      "current_score")
  
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
      
      season.match.results$current_score[id.matches.all] <- 3 * total.wins.all + total.draws.all
      
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

getSeasonMatchStat <- function(df, par, name) {
  
  stat.results <- as.data.frame(matrix(0, nrow = nrow(df), ncol = 3))
  colnames(stat.results) <- c(paste0(name, "_all"), paste0(name, "_home"), paste0(name, "_away"))
  
  seasons <- unique(df$Season)
  for (season in seasons) {
    teams <- unique(df$Team[df$Season == season])
    for (team in teams) {
      
      #############################
      # For all matches in season #
      #############################
      
      id.matches.all <- which(df$Season == season & df$Team == team)
      stat.matches.all <- df[id.matches.all, par]
      
      stat.all <- c(0, cumsum(stat.matches.all)[-length(stat.matches.all)])
      
      stat.results[id.matches.all, 1] <- stat.all      
      
      
      ##################################
      # For all home matches in season #
      ##################################
      
      stat.home <- stat.matches.all
      stat.home[df$home_away[id.matches.all] == "Home"] <- 0
      
      stat.home <- c(0, cumsum(stat.home)[-length(stat.matches.all)])
      
      # write to base (HOME)
      stat.results[id.matches.all, 2] <- stat.home
      
      
      ##################################
      # For all away matches in season #
      ##################################
      
      stat.away <- stat.matches.all
      stat.away[df$home_away[id.matches.all] == "Away"] <- 0
      
      stat.away <- c(0, cumsum(stat.away)[-length(stat.matches.all)])
      
      # write to base (HOME)
      stat.results[id.matches.all, 3] <- stat.away
      
    }
    
  }
  stat.results
}

mergeSeasonMatchFeatures <- function(df) {
  
  # Let's merge features from season stat:
  # There're goals in N matches, attempts, shots, etc. per current season
  
  # Define features and their output names in feature_df
  if (T) {
    pars <- rbind(
      c("FTG", "current_ft_goals_scored"), 
      c("FTG_vs", "current_ft_goals_conceded"),
      c("HTG", "current_ht_goals_scored"),  
      c("HTG_vs", "current_ht_goals_conceded"),
      c("S", "current_team_shots"),
      c("S_vs", "current_team_vs_shots"),
      c("ST", "current_team_shots_target"),
      c("ST_vs", "current_team_vs_shots_target"),
      c("F", "current_team_fouls"),
      c("F_vs", "current_team_vs_fouls"),
      c("C", "current_team_corners"),
      c("C_vs", "current_team_vs_corners"),
      c("Y", "current_team_yellow_cards"),
      c("Y_vs", "current_team_vs_yellow_cards")
    )
  }
  
  pars <- as.data.frame(pars)
  pars[[1]] <- as.character(pars[[1]])
  pars[[2]] <- as.character(pars[[2]])
  colnames(pars) <- c("feature", "output_name")
  
  # Let's add each group of features in list, 
  # and cbind all !!!!!
  
  features.list <- lapply(seq(nrow(pars)), function(i) 
    getSeasonMatchStat(df, par = pars$feature[i], name = pars$output_name[i]))
  
  
  # final base (without -1 - empty dfs)
  season.features.base <- do.call(what = cbind, args = features.list[!identical(features.list, -1)])
  
  season.features.base 
  
  
}

getSeasonStandings <- function(df) {
  
  rank.teams <- rep(0, nrow(df))
  
  seasons <- unique(df$Season)
  
  for (season in seasons) {
    id.season <- which(df$Season == season)
    season.dataset <- df[id.season, ]
    
    teams <- unique(season.dataset$Team)
    dates <- unique(season.dataset$Date)
    
    simple.standings <- data.frame(team = teams, score = rep(0, length(teams)), 
                                   n.games = rep(0, length(teams)), goals.diffs = rep(0, length(teams)))
    
    # for each date grab info (standings pars) about team
    list.standings <- list()
    
    list.standings[[1]] <- simple.standings
    
    for (i in 2:length(dates)) {
      last.standings <- list.standings[[i-1]]
      
      current.date <- dates[i]
      
      id <- which(season.dataset$Date == current.date)
      
      teams.played <- unique(season.dataset$Team[id])
      
      # if the team doesn't play today, 
      # we get info from last standing
      
      current.standings <- last.standings
      
      # but if team does play, 
      # we get new info :)
      
      for (j in seq_along(id)) {
        current.id <- id[j]
        
        # team
        team <- season.dataset$Team[current.id]
        
        # stat
        current.score <- season.dataset$current_score[current.id]
        current.n.games <- season.dataset$n_games_all[current.id]
        current.diff.goals <- season.dataset$current_ft_goals_scored_all[current.id] - 
          season.dataset$current_ft_goals_conceded_all[current.id]
        
        # id in current.standing
        id.team <- which(current.standings$team == team)
        
        current.standings$score[id.team] <- current.score
        current.standings$n.games[id.team] <- current.n.games
        current.standings$goals.diffs[id.team] <- current.diff.goals
        
      }
      
      list.standings[[i]] <- current.standings
      
    }
    
    # get rank
    
    for (k in id.season) {
      team <- df$Team[k]
      team.vs <- df$Team_vs[k]
      
      standings <- list.standings[[which(dates == df$Date[k])]]
      standings <- standings[order(standings$score, -standings$n.games, standings$goals.diffs), ]
      
      rank.team <- which(standings$team == team) / nrow(standings)
      
      rank.teams[k] <- rank.team
    }
  }
  
  result <- data.frame(rank = rank.teams)
  result
}

# raw.seasons <- downloadRangeSeason(league = "E", division = "0", range.start.years = 2012:2014)
# dataset <- getForkedDataset(raw.seasons)
#
# season.match.results <- getSeasonMatchResults(dataset)
# season.match.stats <- mergeSeasonMatchFeatures(dataset)
# season.all.stat <- cbind(dataset, season.match.results, season.match.stats)
# ranks <- getSeasonStandings(season.all.stat)
# season.all.stat <- cbind(season.all.stat, ranks)
