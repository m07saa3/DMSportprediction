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
    out <- cbind(Season = season, out)
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


#season  <- downloadSeason(league = "E", division = "0", season.start.year = 1999, add.season = T, na.rm = T)
#seasons <- downloadRangeSeason(league = "E", division = "0", range.start.years = 1999:2002)

# АПЛ - все сезоны с 93/94 по 13/14
english.premier.league <- downloadRangeSeason(league = "E", division = "0",
                                              range.start.years = 1993:2013)
