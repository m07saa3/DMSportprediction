

#######################################################################
#                                                                     #
# There is start code (very raw and slow :D) for predicting           #
# results of football matches                                         #
#                                                                     #
# Code has been tested only on leagues: B1, E1, I1, F1                #
#                                                                     #
# TV - diffs of goals_by_hometeam and goals_by_awayteam               #
#                                                                     #
# Features - general data (stat from hometeam and awayteam matches)   #
# and personal meeting (the same statistics)                          #
#                                                                     #
#######################################################################




GetStat <- function(comand, matches.id, stat.for.home, stat.for.away) {
  
  stat <- c()
  
  for (i in seq_along(matches.id)) {
    match.id <- matches.id[i]
    
    if (dataset$HomeTeam[match.id] == comand) {
      stat <- append(stat, dataset[match.id, stat.for.home])
    } else {
      stat <- append(stat, dataset[match.id, stat.for.away])
    }
    
  }
  
  stat
}


# download from github repo
path.file = "/home/stason/Projects/DMSportPrediction/data_raw/football_data_co_uk/B1.csv"

dataset <- read.csv(path.file, stringsAsFactors = FALSE)



# Pars, which need for data 

names.pars <- c("ft_goal_scored", "ft_goal_conceded", "ht_goal_scored", "ht_goal_conceded",
                "shots", "shots_on_target", 
                "corners", "fouls_committed", "yellow_cards", "red_cards")

if (T) {
  statistics.pars <- c(
    "FTHG" , # Full Time Home Team Goals (home team goal scored)
    "FTAG" , # Full Time Away Team Goals
    
    "FTAG" , # Full Time Home Team Goals (home team goal conceded)
    "FTHG" , # Full Time Away Team Goals
    
    "HTHG" , # Half Time Home Team Goals (away team goal scored)
    "HTAG" , # Half Time Away Team Goals
    
    "HTAG" , # Half Time Home Team Goals (away team goal conceded)
    "HTHG" , # Half Time Away Team Goals
    
    "HS" , # Home Team Shots
    "AS" , # Away Team Shots
    
    "HST" , # Home Team Shots on Target
    "AST" , # Away Team Shots on Target
    
    "HC" , # Home Team Corners
    "AC" , # Away Team Corners
    
    "HF" , # Home Team Fouls Committed
    "AF" , # Away Team Fouls Committed
    
    "HY" , # Home Team Yellow Cards
    "AY" , # Away Team Yellow Cards
    
    "HR" , # Home Team Red Cards
    "AR"  # Away Team Red Cards
    
  )
}



################
# General data #
################

# how many last matches we analyze?  

window <- 9

stat.list.general <- list()
for (i in seq(nrow(dataset))) {
  
  # home. - home team
  # away. - away team
  
  # get team names
  home.team <- dataset$HomeTeam[i]
  away.team <- dataset$AwayTeam[i]
  
  # get id of last matches
  matches.id.home <- which(dataset$HomeTeam[1: (i-1)] == home.team | dataset$AwayTeam[1:(i-1)] == home.team)
  matches.id.away <- which(dataset$HomeTeam[1: (i-1)] == away.team | dataset$AwayTeam[1:(i-1)] == away.team)
  
  if (min(length(matches.id.home), length(matches.id.away)) >= window) {
    
    # id of last <window> matches
    part.last.matches.home <- matches.id.home[(length(matches.id.home) - window + 1):length(matches.id.home)]
    part.last.matches.away <- matches.id.away[(length(matches.id.away) - window + 1):length(matches.id.away)]
    
    all.statistics.home.team <- rep(0, length(statistics.pars) / 2)
    all.statistics.away.team <- rep(0, length(statistics.pars) / 2)
    
    
    for (j in seq(2, length(statistics.pars), 2)) {
      par.home <- statistics.pars[j-1]
      par.away <- statistics.pars[j]
      
      stat.home <- GetStat(home.team, part.last.matches.home, 
                           stat.for.home = par.home, stat.for.away = par.away)
      stat.away <- GetStat(away.team, part.last.matches.away, 
                           stat.for.home = par.home, stat.for.away = par.away)
      
      all.statistics.home.team[j / 2] <- mean(stat.home)
      all.statistics.away.team[j / 2] <- mean(stat.away)
    }
    
    names(all.statistics.home.team) <- sapply(seq(length(statistics.pars)/2 ), function(k) paste0("home_", names.pars[k]))
    names(all.statistics.away.team) <- sapply(seq(length(statistics.pars)/2), function(k) paste0("away_", names.pars[k]))
    
    
    all.statistics <- c(all.statistics.home.team, all.statistics.away.team)
    
    stat.list.general[[i]] <- all.statistics
    
    
  } else {
    stat.list.general[[i]]  <- NA
  }
  
  print(i)
}



###############################
# Data from personal meetings #
###############################

window <- 5

stat.list.personal <- list()
for (i in seq(nrow(dataset))) {
  
  # home. - home team
  # away. - away team
  
  # get team names
  home.team <- dataset$HomeTeam[i]
  away.team <- dataset$AwayTeam[i]
  
  # get id of last matches
  matches.id.home <- which(dataset$HomeTeam[1: (i-1)] == home.team & dataset$AwayTeam[1:(i-1)] == away.team)
  matches.id.away <- which(dataset$HomeTeam[1: (i-1)] == away.team & dataset$AwayTeam[1:(i-1)] == home.team)
  
  matches.id.personal <- sort(unique(c(matches.id.home, matches.id.away)))
  
  if (length(matches.id.personal) >= window) {
    
    # id of last <window> matches
    part.last.matches <- matches.id.personal[(length(matches.id.personal) - window + 1):length(matches.id.personal)]
    
    all.statistics.home.team <- rep(0, length(statistics.pars) / 2)
    all.statistics.away.team <- rep(0, length(statistics.pars) / 2)
    
    
    for (j in seq(2, length(statistics.pars), 2)) {
      par.home <- statistics.pars[j-1]
      par.away <- statistics.pars[j]
      
      stat.home <- GetStat(home.team, part.last.matches, 
                           stat.for.home = par.home, stat.for.away = par.away)
      
      all.statistics.home.team[j / 2] <- mean(stat.home)
    }
    
    names(all.statistics.home.team) <- sapply(seq(length(statistics.pars)/2 ), function(k) paste0("personal_home_", names.pars[k]))
    
    
    all.statistics <- all.statistics.home.team
    
    stat.list.personal[[i]] <- all.statistics
    
    
  } else {
    stat.list.personal[[i]]  <- NA
  }
  
  print(i)
}



########################
# Collect features and #
# remove NA            #
########################

features.general <- as.data.frame(do.call(rbind, stat.list.general))
features.personal <- as.data.frame(do.call(rbind, stat.list.personal))

# all features: general data + personal_meetings
features <- cbind(features.general, features.personal)

labels <- dataset$FTHG - dataset$FTAG 

# removing NA
id_na_cols <- which(apply(is.na(features), 2, all))

if(length(id_na_cols) > 0)
  features <- features[, -id_na_cols]

id_na_rows <- which(apply(is.na(features), 1, any))

features.clear <- as.data.frame(features[-id_na_rows, ])
labels.clear <- labels[-id_na_rows]

sort(cor(cbind(labels.clear, features.clear))[, 1])


############### 
# Build model #
###############

dim(features.clear)

# simple linear regression
# without regularization

model <- lm(labels.clear ~., features.clear)
summary.model <- summary(model)
result <- summary.model$adj.r.squared
result
