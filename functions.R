library(quantmod)
library(dplyr)
library(lubridate)
library(plotly)

getTimeSeries <- function(symbol) {
  library(quantmod)
  timeSeries <- getSymbols(symbol, src = "yahoo", from = (Sys.Date() - 180),
                             to = Sys.Date(),
                             auto.assign = FALSE,
                             #periodicity = "monthly"
                           )
  colnames(timeSeries) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  return(timeSeries)
}

formatTimeSeries <- function(timeSeries) {
timeSeries <- as.data.frame(timeSeries)
timeSeries$Date <- rownames(timeSeries)
timeSeries <- na.omit(timeSeries)
return(timeSeries)}

calculateMacd <- function(timeSeries) {
  macd <- MACD(timeSeries$Close)
  macd <- as.data.frame(macd)
  macd$date <- timeSeries$Date
  macd <- na.omit(macd)
  return(macd)
}

plotMacd <- function(macd) {
  fig <- plot_ly(macd, x = ~date, y = ~macd,
                 name = "MACD",
                 type = "scatter",
                 mode = "line"
  )
  fig <- fig %>% add_trace(y = ~signal, name = 'Signal',
                           mode = "line")

  fig
}

symbolText <- function() {
  symbols <- HTML("<b>Asia Pacific Ex Japan</b>
  <p>Fidelity Index Pacific ex Japan (Class P): 0P00011UPP.L</p>
  <p>HSBC Pacific Index (Class C): 0P0000WN7M.L</p>
  <p>Vanguard Pacific ex-Japan Stock Index: 0P0000KM1Z.L</p>")

  return(symbols)
}
