require(RCurl)
require(plyr)

downloadSeason <- function (league, division, season.start.year, add.season = TRUE, na.rm = TRUE) {

  start.year <- season.start.year
  end.year <- season.start.year + 1

  URL <- sprintf("http://www.football-data.co.uk/mmz4281/%02d%02d/%s%s.csv",
                 start.year %% 100, end.year %% 100, league, division)
  x <- getURL(URL)
  out <- read.csv(textConnection(x))

  if (na.rm) {
    out[out == ""] <- NA
    out <- out[,colSums(is.na(out)) < nrow(out)]
    out <- na.omit(out)
  }

  if (add.season) {
    season <- sprintf("%d/%d", start.year, end.year)
    out <- cbind(Season = season, Start.year = start.year, out)
  }

  return (out)
}
downloadRangeSeason <- function (league, division, range.start.years, add.season = TRUE, combine = TRUE) {

  seasons.list <- sapply(range.start.years, function(x)
    downloadSeason(league, division, season.start.year = x, add.season))

  if (!combine)
    return (seasons.list)


  seasons <- rbind.fill(seasons.list)
  return (seasons)
}

# example:
#season  <- downloadSeason(league = "E", division = "0", season.start.year = 1999, add.season = T, na.rm = T)
#seasons <- downloadRangeSeason(league = "E", division = "0", range.start.years = 1999:2002)

# this function load all data to csv files:

load.data <- function() {
# england:
E0 <- downloadRangeSeason(league = "E", division = "0", range.start.years = 1994:2014)
E1 <- downloadRangeSeason(league = "E", division = "1", range.start.years = 1994:2014)
E2 <- downloadRangeSeason(league = "E", division = "2", range.start.years = 1994:2014)
E3 <- downloadRangeSeason(league = "E", division = "3", range.start.years = 1994:2014)
EC <- downloadRangeSeason(league = "E", division = "3", range.start.years = 2005:2014)
write.csv(E0,file="E0.csv")
write.csv(E1,file="E1.csv")
write.csv(E2,file="E2.csv")
write.csv(E3,file="E3.csv")
write.csv(EC,file="EC.csv")
# scotland:
SC0 <- downloadRangeSeason(league = "SC", division = "0", range.start.years = 1994:2014)
SC1 <- downloadRangeSeason(league = "SC", division = "1", range.start.years = 1994:2014)
SC2 <- downloadRangeSeason(league = "SC", division = "2", range.start.years = 2000:2014)
SC3 <- downloadRangeSeason(league = "SC", division = "3", range.start.years = 2000:2014)
write.csv(SC0,file="SC0.csv")
write.csv(SC1,file="SC1.csv")
write.csv(SC2,file="SC2.csv")
write.csv(SC3,file="SC3.csv")
# germany:
D1 <- downloadRangeSeason(league = "D", division = "1", range.start.years = 1994:2014)
D2 <- downloadRangeSeason(league = "D", division = "2", range.start.years = 1994:2014)
write.csv(D1,file="D1.csv")
write.csv(D2,file="D2.csv")
# italy:
I1 <- downloadRangeSeason(league = "I", division = "1", range.start.years = 2000:2014)
I2 <- downloadRangeSeason(league = "I", division = "2", range.start.years = 2000:2014)
write.csv(I1,file="I1.csv")
write.csv(I2,file="I2.csv")
# spain:
SP1 <- downloadRangeSeason(league = "SP", division = "1", range.start.years = 1994:2014)
SP2 <- downloadRangeSeason(league = "SP", division = "2", range.start.years = 1996:2014)
write.csv(SP1,file="SP1.csv")
write.csv(SP2,file="SP2.csv")
# france:
F1 <- downloadRangeSeason(league = "F", division = "1", range.start.years = 1994:2014)
F2 <- downloadRangeSeason(league = "F", division = "2", range.start.years = 1996:2014)
write.csv(F1,file="F1.csv")
write.csv(F2,file="F2.csv")
# netherlands:
N1 <- downloadRangeSeason(league = "N", division = "1", range.start.years = 1994:2014)
write.csv(N1,file="N1.csv")
# belgium:
B1 <- downloadRangeSeason(league = "B", division = "1", range.start.years = 1996:2014)
write.csv(B1,file="B1.csv")
# portugal:
P1 <- downloadRangeSeason(league = "P", division = "1", range.start.years = 1994:2014)
write.csv(P1,file="P1.csv")
# turkey:
T1 <- downloadRangeSeason(league = "T", division = "1", range.start.years = 1994:2014)
write.csv(T1,file="T1.csv")
# greece:
G1 <- downloadRangeSeason(league = "G", division = "1", range.start.years = 1996:2014)
write.csv(G1,file="G1.csv")
}
