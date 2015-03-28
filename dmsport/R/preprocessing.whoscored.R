# load data.frame from raw-file
get.data.whoscored <- function() {
whoscored.path <- file.path("..", "..", "data_raw", "whoscored", "who.csv")
data <- read.csv(whoscored.path, sep = ";", as.is = TRUE)
data$Time <- dmy_hms(data$Time, tz = "UTC")
years <- simplify2array(strsplit(data$Season, split = "/"))
data$Year.start <- as.numeric(y[1,])
data$Year.end <- as.numeric(y[2,])
print(table(data$ScanStatus))
names <- colnames(data)
names[names == "Legua"] <- "League"
colnames(data) <- names
data$League <- paste(data$Country, data$League)
data
}

# print some info about data
df.stats <- function(df) {
  print(sort(unique(df$Year.start)))
  print(sort(unique(df$League)))
}


# substract data
filter.data <- function(df, years = 2009:2014, leagues = "") {
  df[df$Year.start %in% years & (leagues == "" | df$League %in% leagues), ]
}
