# Get dataset, which dimension is 2N x M
# each row (match) splited on two rows: for home and away teams
# 
# Also added special column: Home_away
#
# Simple using:
# raw.seasons <- downloadRangeSeason(league = "E", division = "0", range.start.years = 2011:2014)
# dataset <- getForkedDataset(raw.seasons)


getForkedDataset <- function(df) {
  
  # This trick is used for recovering 
  # initial order
  
  id <- 1:nrow(df)
  
  id.home <- 2 * id - 1
  id.away <- 2 * id
  
  
  # So, let's define pars for home team, then for away team
  # and, finally, new pars, f.e:
  # FTHG and FTAG -> FTG and FTG_vs
  
  pars.home <- c("Season", "Start.year", "Div", "Date", "HomeTeam", "AwayTeam"
                 , "FTHG", "FTAG", "FTR"
                 , "HTHG", "HTAG", "HTR", "Referee", "HS", "AS"
                 , "HST", "AST", "HF", "AF", "HC", "AC"
                 , "HY", "AY", "HR")
  
  pars.away <- c("Season", "Start.year", "Div", "Date", "AwayTeam", "HomeTeam"
                 , "FTAG", "FTHG", "FTR"
                 , "HTAG", "HTHG", "HTR", "Referee", "AS", "HS"
                 , "AST", "HST", "AF", "HF", "AC", "HC"
                 , "AY", "HY", "HR")
  
  
  pars.new <- c("Season", "Start.year", "Div", "Date", "Team", "Team_vs"
                , "FTG", "FTG_vs", "FTR"
                , "HTG", "HTG_vs", "HTR", "Referee", "S", "S_vs"
                , "ST", "ST_vs", "F", "F_vs", "C", "C_vs"
                , "Y", "Y_vs", "HR")
  
  # Get subsets for home and away teams
  home.df <- df[, pars.home]
  away.df <- df[, pars.away]
  
  colnames(home.df) <- pars.new
  colnames(away.df) <- pars.new
  
  home.df$home_away <- "Home"
  away.df$home_away <- "Away"
  
  home.df$id <- id.home
  away.df$id <- id.away
  
  # Merging data
  new.df <- rbind(home.df, away.df)
  new.df <- new.df[order(new.df$id, decreasing = F), ]
  
  # Remove unnecessary
  new.df$id <- NULL
  rownames(new.df) <- NULL
  
  new.df
}