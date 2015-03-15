setwd("DM Sport")

GetMatchStat <- function(stat, col.id, right.order = T) {
  id <- which(dataset[, col.id] == stat)
  
  result <- rep(0, 2)
  
  if (right.order) {
    home.id <- 1
    away.id <- 2
  } else {
    home.id <- 2
    away.id <- 1
  }
  
    result[home.id] <-  dataset[id, col.id - 1]
    result[away.id] <- dataset[id, col.id + 1]
  
  names(result) <- c(paste0(stat, "_home"), paste0(stat, "_away"))
  
  result
}

GetStat <- function(team.home, col.id) {
  result <- list()
  
  # Does away_team follows(T/F) by home_team ?
  good.order = F
  
  teams <- GetMatchStat("Type", 3)
  
  if (teams[1] == team.home) {
    good.order = T
  }
  
  result$goals <- GetMatchStat("Goal", col.id, good.order)
  result$penalties <- GetMatchStat("Penalty", col.id, good.order)
  result$red.cards <- GetMatchStat("Red Card", col.id, good.order)
  result$yellow.cards <- GetMatchStat("Yellow Card", col.id, good.order)
  result$corners <- GetMatchStat("Corner", col.id, good.order)
  result$substitutions <- GetMatchStat("Substitution", col.id, good.order)
  result$shots.on.target <- GetMatchStat("Shot on Target", col.id, good.order)
  result$shots.off.target <- GetMatchStat("Shot off Target", col.id, good.order)
  result$dangers <- GetMatchStat("Danger", col.id, good.order)
  result$attacks <- GetMatchStat("Attack", col.id, good.order)
  
  result
}

GetGeneralStat <- function() {
  match.id <- dataset$ID[1]
  
  country <- dataset$Country[1]
  
  league <- dataset$League[1]
  
  kickoff <- dataset$Kickoff[1]
  
  home.team <- dataset$Home.team[1]
  
  away.team <- dataset$Away.team[1]
  
  general.stat <- c(match.id, country, league, kickoff, home.team, away.team)
  
  general.stat
}



library(XLConnect)

################
# Get features #
################

# files from rts

#########################
# XLSX files must be    #
# in <XXX> folder       #
#########################

files <- list.files("<XXX>")

names.main.stat <- c("Path_file", "Match_id", "Country", " League", "Kickoff", "Team_home", "Team_away", 
                     "Goals_home", "Goals_away", "Penalties_home", "Penalties_away",
                     "Red_cards_home", "Red_cards_away", "Yellow_cards_home", "Yellow_cards_away",
                     "Corners_home", "Corners_away", "Substitutions_home", "Substitutions_away", 
                     "Shots_on_target_home", "Shots_on_target_away",   "Shots_off_target_home", "Shots_off_target_away",
                     "Dangers_home", "Dangers_away",  "Attacks_home", "Attacks_away")

features <- as.data.frame(matrix(0, nrow = length(files), ncol = length(names.main.stat)))
colnames(features) <- names.main.stat


for (i in seq_along(files)) {
  
  path.file <- files[i]
  
  dataset <- readWorksheet(loadWorkbook(path.file), sheet = 1)
  
  #################
  # General stats #
  #################
  general.stat <- unlist(GetGeneralStat())
  
  ####################
  # Match statistics #
  # (goals, cards..) #
  ####################
  match.stat <- as.numeric(unlist(GetStat(team.home = general.stat[5], col.id = 3)))
  
  
  features[i, 1] <- path.file
  features[i, 2:7] <- general.stat   
  features[i, 8:ncol(features)] <- match.stat
}

dir.create("Prepared")
write.csv(features, file = "Prepared/features.csv", row.names = F, quote = F)
