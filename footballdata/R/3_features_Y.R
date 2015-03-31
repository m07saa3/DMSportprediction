create.y <- function(df) {
  Y <- data.frame(
    full.diff = df$FTHG - df$FTAG,
    full.total = df$FTHG + df$FTAG,
    half.diff = df$HTHG - df$HTAG,
    half.total = df$HTHG + df$HTAG,
    result = df$FTR,
    home.win = df$FTR == "H"
    )
}