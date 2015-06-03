# There is very-very simple glmnet.
# Using is simple too:
#
# get.model.results: 
# 1) get features 
# 2) remove na rows
# 3) split on train and test sets
# 4) predicting on test set

build.glmnet.model <- function(trainSet, train.TV) {
  
  print("Building model..finding optimal lambda..")
  
  set.seed(1)
  
  model <- cv.glmnet(as.matrix(trainSet), train.TV , nfolds = 5)

  model
}

get.model.results <- function(raw.seasons) {
  
  sq.R <- function(pred, real) {
    1- (var(pred-real)/var(real))^2
  }
  
  # get labels and features
  labels <- create.y(raw.seasons)
  TV <- labels$full.diff
  
  features <- getFeatures(raw.seasons)
  
  # remove NA rows
  not.na.rows <- which(apply(!is.na(features), 1, all))
  features <- features[not.na.rows, -1:-6]
  TV <- TV[not.na.rows]
  
  raw <- raw.seasons[not.na.rows, ]
  
  highlyCorrelated <- findCorrelation(cor(features), cutoff = 0.8)
  features <- features[, -highlyCorrelated]
  
  # split on train and test sets
  trainSet <- features[1:(nrow(features) * 0.8), ]
  trainRaw <- raw[1:(nrow(features) * 0.8), ]
  train.TV <- TV[1:(nrow(features) * 0.8)]
  
  testSet <- features[(nrow(trainSet) + 1):nrow(features), ]
  testRaw <- raw[(nrow(trainSet) + 1):nrow(features), ]
  test.TV <- TV[(nrow(trainSet) + 1):nrow(features)] 
  
  # Code: build model
  model <- build.glmnet.model(trainSet, train.TV)
  
  # predict to test set
  predictions <- as.numeric(round(predict(model, as.matrix(testSet), s = "lambda.min")))
  hist(predictions - test.TV, breaks = 30)
  
  pred.base <- data.frame(real = test.TV,
                          predictions = predictions,
                          residuals = predictions - test.TV
                          )
  
  results <- list()
  
  results$model <- model
  results$pred.base <- pred.base
  results$squaredR <- sq.R(predictions, test.TV)
  
  results$test.data <- testRaw
  results$train.data <- trainRaw
  
  results
}

# It works for E0, because there a lot of pars.
# raw.seasons <- downloadRangeSeason(league = "E", division = "0", range.start.years = 2011:2014)
# results <- get.model.results(raw.seasons)
#
#
# So, there are real subset: test.set,
# and predictions for him: predictions$predictions
#
# predictions <- results$pred.base
# test.set <- results$test.data 
