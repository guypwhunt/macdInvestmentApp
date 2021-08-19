source("functions.R")

#df <- read.csv("indexFund.csv")
#df <- read.csv("industryEtfs.csv", fileEncoding="UTF-8-BOM")
#df <- read.csv("geographicalEtfs.csv")
df <- read.csv("ftse100Companies.csv", fileEncoding="UTF-8-BOM")

finalDf <- df
#rbind(rbind(df["ID"], df1["ID"]), df2["ID"])

timeSpan <- list("monthly", "weekly", "daily")

for (time in timeSpan) {
  symbols <- c()
  for (i in 1:nrow(finalDf)) {
    try({
      symbol <- finalDf["ID"][i,]
      timeSeries <- getTimeSeries(symbol, "yahoo", time)
      timeSeries <- formatTimeSeries(timeSeries)
      macd <- calculateMacd(timeSeries)
      attributes(macd)
      if (tail(macd["signal"], 1) < 0 | tail(macd["macd"], 1) < 0) {
        symbols <- append(symbols, symbol)
      }
    }, silent = TRUE)
  }
  print("")
  print(time)
  subDf <- df[df$ID %in% symbols,]
  print(subDf[, c("ID", "Name")])
}
