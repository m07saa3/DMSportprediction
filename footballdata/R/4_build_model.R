# There is very-very simple glmnet.
# Using is simple too:
#
# get.model.results: 
# 1) get features 
# 2) remove na rows
# 3) split on train and test sets
# 4) predicting on test set

build.glmnet.model <- function(trainSet, train.TV) {
  
  par.alpha = seq(0, 1, 0.02)
  res <- c()
  
  print("Building model..finding optimal alpha and lambda..")
  for (a in par.alpha) {
    set.seed(1)
    model <- cv.glmnet(as.matrix(trainSet), train.TV , nfolds = 5, alpha = a)
    res <- append(res, min(model$cvm))
    print(a)
  }
  
  best.alpha <- par.alpha[which.min(res)]
  set.seed(1)
  model <- cv.glmnet(as.matrix(trainSet), train.TV , nfolds = 5, alpha = best.alpha)
  
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
  
  # split on train and test sets
  trainSet <- features[1:(nrow(features) * 0.8), ]
  train.TV <- TV[1:(nrow(features) * 0.8)]
  
  testSet <- features[(nrow(trainSet) + 1):nrow(features), ]
  test.TV <- TV[(nrow(trainSet) + 1):nrow(features)] 
  
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
  
  results
}

# raw.seasons <- downloadRangeSeason(league = "E", division = "0", range.start.years = 2011:2014)
# results <- get.model.results(raw.seasons)
# results