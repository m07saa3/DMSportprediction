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
# goal.features <- getGeneralStat(dataset, 4, 2, "FTG")


getGeneralStat <- function(df, window_n, window_m, stat) {
  
  teams <- unique(df$Team)
  
  general.stat <- as.data.frame(matrix(NA, nrow = nrow(df), ncol = 2))
  colnames(general.stat) <- c(paste0(stat, "_all"), paste0(stat, "_home_away"))
  
  for (team in teams) {
  
    # As you can see below, there are added NA
    # at the begin of stat - vector - the info about <window_n or _m> matches is absent, and
    # removed last item - otherwise we look in the future :)
    
    
    # get id of all <window_n> team's matches 
    
    team.id_all <- which(df$Team == team)
    if (length(team.id_all) > window_n)
      general.stat[team.id_all, 1] <- c(rep(NA, window_n), 
                                        rollapply(df[team.id_all, stat], window_n, mean))[-length(team.id_all)]

    # get id of all <window_m> home team's matches 
    team.id_home <- which(df$Team == team & df$home_away == "Home")
    if (length(team.id_home) > window_m)
      general.stat[team.id_home, 2] <- c(rep(NA, window_m), 
                                         rollapply(df[team.id_home, stat], window_m, mean))[-length(team.id_home)]
  
    # get id of all <window_m> away team's matches 
    team.id_away <- which(df$Team == team & df$home_away == "Away")
    if (length(team.id_home) > window_m)
      general.stat[team.id_away, 2] <- c(rep(NA, window_m), 
                                         rollapply(df[team.id_away, stat], window_m, mean))[-length(team.id_away)]
    
  }
  
  general.stat
}


