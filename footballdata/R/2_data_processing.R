#TO DO convert Date as Posix



#TO DO create function which make df for team-stat-analysis



# removes NA from df
clean.df <- function(df) {
  na.mask <- is.na(df)
  na.cols <- apply(na.mask, 2, sum) == nrow(df)
  na.rows <- apply(na.mask, 1, sum) == ncol(df)
  df[!na.rows,!na.cols]
}


