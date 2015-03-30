

#################################
#                               #      
# Start code for whoscored data #
#                               #
#################################


path.file = "/home/stason/Projects/DMSportPrediction/data_raw/who.csv"


dataset <- read.csv(path.file, sep = ";", stringsAsFactors = F)
league <- dataset[dataset$Country == "England", ]

# sort by time
league <- league[order(league$Time),]

time.seq <- gsub("[.]","-", league$Time)
league <- league[order(strptime(time.seq, format = "%d-%m-%Y %H:%M:%S")), ]

data.with.preview <- league[league$ExistPreview == "True", ]

# extract features and target variable
features <- data.with.preview[, c(17:25, 57:65)]
TV <- data.with.preview$Score1 - data.with.preview$Score2

# watch correlation
sort(cor(cbind(TV, features))[, 1])

# create model
model <- lm(TV~., data = features)
result <- summary(model)
result$adj.r.squared
