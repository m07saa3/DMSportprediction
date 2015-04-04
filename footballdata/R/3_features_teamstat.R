#
# getGeneralStat - get info about some stat (goals, shots, etc..) per last matches
# merge.general.features - merge this stat
#
# Input: birfucated dataframe (see example below), 
# window_n - look stat for n last matches (all)
# widnow_m - look stat for m last matches (home_away)
#
# Output: df - features
#
# Simple example:
# raw.seasons <- downloadRangeSeason(league = "E", division = "0", range.start.years = 2011:2014)
# dataset <- getForkedDataset(raw.seasons)
#
# general.features <- merge.general.features(dataset, 10, 5);dim(general.features)


getGeneralStat <- function(df, window_n, window_m, stat, name) {
  
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

merge.general.features <- function(df, window_n, window_m) {
  
  # Let's merge features from general stat:
  # Features are:
  # goals in N matches, attempts, shots, etc.
  
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
