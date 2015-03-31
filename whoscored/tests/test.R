# set league
league <- "England Premier League"
# load all data
data <- get.data.whoscored()
# grep by league and date
df <- filter.data(data, 2014, league)
# add some Y
y.df <- Y.features(df)


baseline.total = mse(y.df$Y.full.total, y.df$prediction.total)
baseline.dif = mse(y.df$Y.full.dif, y.df$prediction.dif)