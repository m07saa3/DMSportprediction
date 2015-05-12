# Merge all features!

getFeatures <- function(raw.seasons) {
  
  # get data
  dataset <- getForkedDataset(raw.seasons)
  
  # GENERAL AND OUTCOME FEATURES
  general.features <- mergeGeneralMatchFeatures(dataset, 10, 5)
  outcome.match.features <- mergeOutcomeMatchFeatures(dataset, 10, 5)
  
  # SEASON FEATURES
  season.match.results <- getSeasonMatchResults(dataset)
  season.match.stats <- mergeSeasonMatchFeatures(dataset)
  season.all.stat <- cbind(dataset, season.match.results, season.match.stats)
  ranks <- getSeasonStandings(season.all.stat)
  
  season.features <- cbind(season.match.results, season.match.stats, ranks)
  
  all.features <- cbind(general.features, outcome.match.features, season.features)
  
  all.features <- cbind(dataset[ , c("Season", "Start.year", "Div", "Date", "Team", "Team_vs", "home_away")], all.features)
 
  ################################################
  # now add features from another team (vs team) #
  ################################################
  
  all.features.current <- all.features[seq(1, nrow(all.features) - 1, 2), -7]
  all.features.vs <- all.features[seq(2, nrow(all.features), 2), -1:-7]
  
  colnames(all.features.vs) <- sapply(seq(ncol(all.features.vs)), 
                                      function(i) paste0("VS_", colnames(all.features.vs)[i]))
 
  result <- cbind(all.features.current, all.features.vs)
}


# names vs in feature's name, means not stat vs team!
# It means stat from all teams, which was rival team!
# F.e. vs for goals - means how much goals conceded team

# raw <- downloadRangeSeason(league = "E", division = "0", range.start.years = 2011:2014)
# features <- getFeatures(raw)
