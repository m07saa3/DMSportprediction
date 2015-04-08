# Merge all features!

getFeatures <- function(league, division, range.start.years) {
  
  # get data
  raw.seasons <- downloadRangeSeason(league = league, division = division, range.start.years = range.start.years)
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
  
  all.features
}

# features <- getFeatures(league = "E", division = "0", range.start.years = 2011:2014)