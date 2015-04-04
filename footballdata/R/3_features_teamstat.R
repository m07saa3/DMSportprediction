# These function get info about some stat per last matches
#
# Input: birfucated dataframe (see example below)
#
# There are two columns in output dataframe:
# - general info - info about <window_n> last matches
# - home_away info - info about <window_m> last <home or away> matches
#
#
# Simple example:
# raw.seasons <- downloadRangeSeason(league = "E", division = "0", range.start.years = 2011:2014)
# dataset <- getForkedDataset(raw.seasons)
#
# goals per last <window_n> matches and 
# goals per last <window_m> home or away matches 
#
# general.features <- mergeGeneralMatchFeatures(dataset, 10, 5)
# outcome.match.features <- mergeOutcomeMatchFeatures(dataset, 10, 5)



# Number of goals, shots, corners, etc.

getGeneralStat <- function(df, window_n, window_m, stat, name) {
  
  # Stat (FTR or HTR)
  #
  # Number of goals, shots, corners..
  #
  # There are two columns in output dataframe:
  # - general info - info about <window_n> last matches
  # - home_away info - info about <window_m> last <home or away> matches
  
  # Output df or -1 (if par is absent)
  
  if(!stat %in% colnames(df)) {
    warning(paste0("par ", stat, " is absent.."))
    return(-1)
  }
  
  teams <- unique(df$Team)
  
  general.stat <- as.data.frame(matrix(NA, nrow = nrow(df), ncol = 2))
  colnames(general.stat) <- c(paste0(name, "_all"), paste0(name, "_home_away"))
  
  for (team in teams) {    
    
    # As you can see below, there are added NA
    # at the begin of stat - vector - the info about <window_n or _m> matches is absent, and
    # removed last item - otherwise we look in the future :)
    
    # get id of all <window_n> team's matches 
    team.id_all <- which(df$Team == team)
    if (length(team.id_all) > window_n) {
      stat.all <- c(rep(NA, window_n), 
                    rollapply(df[team.id_all, stat], window_n, mean))
      general.stat[team.id_all, 1] <- stat.all[-length(stat.all)]
    }
    
    # get id of all <window_m> home team's matches 
    team.id_home <- which(df$Team == team & df$home_away == "Home")
    if (length(team.id_home) > window_m) {
      stat.home <- c(rep(NA, window_m), 
                     rollapply(df[team.id_home, stat], window_m, mean))
      
      general.stat[team.id_home, 2] <- stat.home[-length(stat.home)]
    }
    
    # get id of all <window_m> away team's matches 
    team.id_away <- which(df$Team == team & df$home_away == "Away")
    if (length(team.id_home) > window_m) {
      stat.away <- c(rep(NA, window_m), 
                     rollapply(df[team.id_away, stat], window_m, mean))
      general.stat[team.id_away, 2] <- stat.away[-length(stat.away)]
    }
  }
  
  general.stat
}

mergeGeneralMatchFeatures <- function(df, window_n, window_m) {
  
  # Let's merge features from general stat:
  # There're goals in N matches, attempts, shots, etc. per last matches
  
  # Define features and their output names in feature_df
  if (T) {
    pars <- rbind(
      c("FTG", "ft_goals_scored"), 
      c("FTG_vs", "ft_goals_conceded"),
      c("HTG", "ht_goals_scored"),  
      c("HTG_vs", "ht_goals_conceded"),
      c("S", "team_shots"),
      c("S_vs", "team_vs_shots"),
      c("ST", "team_shots_target"),
      c("ST_vs", "team_vs_shots_target"),
      c("F", "team_fouls"),
      c("F_vs", "team_vs_fouls"),
      c("C", "team_corners"),
      c("C_vs", "team_vs_corners"),
      c("Y", "team_yellow_cards"),
      c("Y_vs", "team_vs_yellow_cards")
    )
  }
  
  pars <- as.data.frame(pars)
  pars[[1]] <- as.character(pars[[1]])
  pars[[2]] <- as.character(pars[[2]])
  colnames(pars) <- c("feature", "output_name")
  
  # Let's add each group of features in list, 
  # and cbind all !!!!!
  
  features.list <- lapply(seq(nrow(pars)), function(i) 
    getGeneralStat(df, window_n = window_n, window_m = window_m, stat = pars$feature[i], name = pars$output_name[i]))
  
  
  # final base (without -1 - empty dfs)
  general.features.base <- do.call(what = cbind, args = features.list[!identical(features.list, -1)])
  
  general.features.base 
}


# Number of wins, losses, draws per last matches

getOutcomeMatchResult <- function(df, window_n, window_m, stat, name) {
  
  # Stat (FTR or HTR)
  #
  # Number of wins, losses, draws per last matches
  #
  # There are two columns in output dataframe:
  # - general info - info about <window_n> last matches
  # - home_away info - info about <window_m> last <home or away> matches
  
  # Output df or -1 (if par is absent)
  
  if(!stat %in% colnames(df)) {
    warning(paste0("par ", stat, " is absent.."))
    return(-1)
  }
  
  teams <- unique(df$Team)
  
  match.stat <- as.data.frame(matrix(NA, nrow = nrow(df), ncol = 6))
  colnames(match.stat) <- c(paste0(name, "_wins_all"), paste0(name, "_losses_all"), paste0(name, "_draws_all"),
                            paste0(name, "_wins_home_away"), paste0(name, "_losses_home_away"), paste0(name, "_draws_home_away"))
  
  
  for (team in teams) {
    
    # As you can see below, there are added NA
    # at the begin of stat - vector - the info about <window_n or _m> matches is absent, and
    # removed last item - otherwise we look in the future :)
    
    # get id of all <window_n> team's matches 
    team.id_all <- which(df$Team == team)
    
    if (length(team.id_all) > window_n) {
      stat.wins.all <- c(rep(NA, window_n), 
                         rollapply(df[team.id_all, stat] == "W", window_n, sum))
      stat.losses.all <- c(rep(NA, window_n), 
                           rollapply(df[team.id_all, stat] == "L", window_n, sum))
      stat.draws.all <- c(rep(NA, window_n), 
                          rollapply(df[team.id_all, stat] == "D", window_n, sum))
      
      match.stat[team.id_all, 1] <- stat.wins.all[-length(stat.wins.all)]
      match.stat[team.id_all, 2] <- stat.losses.all[-length(stat.losses.all)]
      match.stat[team.id_all, 3] <- stat.draws.all[-length(stat.draws.all)]
    }
    
    # get id of all <window_m> home team's matches 
    team.id_home <- which(df$Team == team & df$home_away == "Home")
    if (length(team.id_home) > window_m) {
      
      stat.wins.home <- c(rep(NA, window_m), 
                          rollapply(df[team.id_home, stat] == "W", window_m, sum))
      stat.losses.home <- c(rep(NA, window_m), 
                            rollapply(df[team.id_home, stat] == "L", window_m, sum))
      stat.draws.home <- c(rep(NA, window_m), 
                           rollapply(df[team.id_home, stat] == "D", window_m, sum))
      
      match.stat[team.id_home, 4] <- stat.wins.home[-length(stat.wins.home)]
      match.stat[team.id_home, 5] <- stat.losses.home[-length(stat.losses.home)]
      match.stat[team.id_home, 6] <- stat.draws.home[-length(stat.draws.home)]
      
    }
    
    # get id of all <window_m> away team's matches 
    team.id_away <- which(df$Team == team & df$home_away == "Away")
    if (length(team.id_away) > window_m) {
      stat.wins.away <- c(rep(NA, window_m), 
                          rollapply(df[team.id_away, stat] == "W", window_m, sum))
      stat.losses.away <- c(rep(NA, window_m), 
                            rollapply(df[team.id_away, stat] == "L", window_m, sum))
      stat.draws.away <- c(rep(NA, window_m), 
                           rollapply(df[team.id_away, stat] == "D", window_m, sum))
      
      match.stat[team.id_away, 4] <- stat.wins.away[-length(stat.wins.away)]
      match.stat[team.id_away, 5] <- stat.losses.away[-length(stat.losses.away)]
      match.stat[team.id_away, 6] <- stat.draws.away[-length(stat.draws.away)]
    }
  }
  
  match.stat
}

mergeOutcomeMatchFeatures <- function(df, window_n, window_m) {
  
  # Let's merge features from outcome match stat:
  # There're number of wins, losses, draws per last matches
  
  pars <- rbind(
    c("FTR", "full_time"), 
    c("HTR", "half_time")
  )
  
  pars <- as.data.frame(pars)
  pars[[1]] <- as.character(pars[[1]])
  pars[[2]] <- as.character(pars[[2]])
  colnames(pars) <- c("feature", "output_name")
  
  features.list <- lapply(seq(nrow(pars)), function(i) 
    getOutcomeResult(df, window_n = window_n, window_m = window_m, stat = pars$feature[i], name = pars$output_name[i]))
  
  
  # final base (without -1 - empty dfs)
  general.features.base <- do.call(what = cbind, args = features.list[!identical(features.list, -1)])
  
  general.features.base 
  
}
