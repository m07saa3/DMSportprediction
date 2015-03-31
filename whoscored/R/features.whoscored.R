

# lets calculate some Y for fun --------------------------
#TO DO change code when add EXTRA TIME + PENALTY TIME

Y.features <- function(df) {
 Y.df <- data.frame( 
  Y.full.dif = df$Score1 - df$Score2,
  Y.half.dif = df$HalfScore1 - df$HalfScore2,
  Y.full.total = df$Score1 + df$Score2,
  Y.half.total = df$HalfScore1 + df$HalfScore2,
  prediction.dif = df$PredictionScore1 - df$PredictionScore2,
  prediction.total = df$PredictionScore1 + df$PredictionScore2
  )
}


