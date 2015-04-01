# stats from historical meetings beetween two teams

# this function return subset from df which include matches beetween team1 and team2
# use same.field = T if you want only matches with this field (home for team1)
get.personal.matches <- function(team1, team2, df, same.field = F) { 
  mask <- df$HomeTeam == team1 & df$AwayTeam == team2
  if(same.field)
    return(df[mask, ])
  df[ mask | (df$HomeTeam == team2 & df$AwayTeam == team1), ]
}

# lets drop all information about Home-Away
# use get.personal.matches, same.fild = F for prepare df
drop.ha.stats <- function(df, team) {
  # well it looks like GOVNOCODE© but in fact function returns
  # same df but with team=team in df$HomeTeam (and ofcource correct others columns)
  # it have to make my life easier tommorow...
  data <- data.frame(
    X=df$X,
    Season=df$Season,
    Start.year=df$Start.year,
    Div=df$Div,
    Date=df$Date,
    HomeTeam=df$HomeTeam,
    AwayTeam=df$AwayTeam,
    FTHG=df$FTHG,
    FTAG=df$FTAG,
    HTHG=df$HTHG,
    HTAG=df$HTAG)
    if(!is.null(df$HS)) {
      data$HS=df$HS
      data$AS=df$AS
    }
    if(!is.null(df$HST)) {
      data$HST=df$HST
      data$AST=df$AST
    }
    if(!is.null(df$HHW)) {
      data$HHW=df$HHW
      data$AHW=df$AHW
    }
    if(!is.null(df$HC)) {
    data$HC=df$HC
    data$AC=df$AC
    }
    if(!is.null(df$HF)) {
    data$HF=df$HF
    data$AF=df$AF
    }
    if(!is.null(df$HO)) {
    data$HO=df$HO
    data$AO=df$AO
    }
    if(!is.null(df$HY)) {
      data$HY=df$HY
      data$AY=df$AY
    }
    if(!is.null(df$HR)) {
      data$HR=df$HR
      data$AR=df$AR
    }
  # holly shit! fuckin drugs...
  mask <- data$AwayTeam == team
  data.ok <- data[!mask,]
  data.mask <- data[mask,]
  n <- ncol(data.mask)
  vec <- 6:n
  add <- vec + rep(c(1,-1), length.out = length(vec))
  data.mask <- data.mask[,c(1:5,add)]
  colnames(data.mask) <- colnames(data.ok)
  d <- rbind(data.mask,data.ok)
  #TO DO
  #d <- d[order(d$Time),]
}


#main function
get.personal.stat <- function(df) {
  teams <- unique(df$HomeTeam)
  pairs <- t(combn(teams, 2))
  unique.pairs <- rbind(pairs, pairs[ ,2:1])
  hash <- paste(unique.pairs)
  all.meetings <- lapply(1:nrow(unique.pairs), FUN = function(i){
    d <- get.personal.matches(unique.pairs[i, 1], unique.pairs[i, 2], df)
    d <- drop.ha.stat(d)
  })
  all.meetings.at.same.field <- lapply(1:nrow(unique.pairs), FUN = function(i){
    get.personal.matches(unique.pairs[i, 1], unique.pairs[i, 2], df, same.field = T)
  })
  # ok, now we have lots of small df
  
  
  
  
  
  
  
  
}