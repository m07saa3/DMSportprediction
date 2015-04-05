#list of leagues
list.leagues <- function() {
  path.file = file.path("..", "data_raw", "football_data_co_uk")
  l = list.files(path.file, pattern = ".csv")
  ll = unlist(strsplit(l, "[.]"))
  ll[ll != "csv"]
}

# download from github repo
get.data <- function(league = "B1") {
  path.file = file.path("..", "data_raw", "football_data_co_uk", paste0(league, ".csv"))
  dataset <- read.csv(path.file, stringsAsFactors = FALSE)
  dataset$Date <- dmy(dataset$Date)
  tail <- year(dataset$Date)%% 2000 #ke-ke-ke :)
  mask <- tail < 50
  year(dataset$Date)[mask] <- 2000 + tail[mask]
  year(dataset$Date)[!mask] <- 1900 + tail[!mask]
  dataset
}
