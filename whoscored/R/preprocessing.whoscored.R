# load data.frame from raw-file
get.data.whoscored <- function() {
whoscored.path <- file.path("..", "data_raw", "whoscored", "who.csv")
data <- read.csv(whoscored.path, sep = ";", as.is = TRUE)
data$Time <- dmy_hms(data$Time, tz = "UTC")
years <- simplify2array(strsplit(data$Season, split = "/"))
data$Year.start <- as.numeric(years[1,])
data$Year.end <- as.numeric(years[2,])
print(table(data$ScanStatus))
names <- colnames(data)
names[names == "Legua"] <- "League"
colnames(data) <- names
data$League <- paste(data$Country, data$League)
data <- data[order(data$Time),]
}

# print some info about data
df.stats <- function(df) {
  print(sort(unique(df$Year.start)))
  print(sort(unique(df$League)))
}


# substract data [use df.stats() for list of available leagues]
filter.data <- function(df, years = 2009:2014, leagues = "") {
  df[df$Year.start %in% years & (leagues == "" | df$League %in% leagues), ]
}


# create df with 1 row = 1 team info *
collect.team.data <- function(df) {
  
}
