#list of leagues
list.leagues <- function() {
  path.file = file.path("..", "data_raw", "football_data_co_uk")
  list.files(path.file, pattern = ".csv")
}

# download from github repo
get.data <- function(league = "B1") {
  path.file = file.path("..", "data_raw", "football_data_co_uk", paste0(league, ".csv"))
  dataset <- read.csv(path.file, stringsAsFactors = FALSE)
  dataset$Date <- dmy(dataset$Date)
  mask <- year(dataset$Date) > 2030 #ke-ke-ke :)
  dataset$Date[mask] <- dataset$Date[mask] - years(100)
  dataset
}
