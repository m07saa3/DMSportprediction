

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



sort(cor(clean.df(cbind(labels.clear, features.clear)))[, 1])


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
