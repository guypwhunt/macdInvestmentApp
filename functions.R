library(quantmod)
library(dplyr)
library(lubridate)
library(plotly)

getTimeSeries <- function(symbol, dataSource, timePeriod) {
  library(quantmod)
  if (dataSource == "yahoo"){
  timeSeries <- getSymbols(symbol, src = dataSource, from = (Sys.Date() - 500),
                             to = Sys.Date(),
                             auto.assign = FALSE,
                             periodicity = timePeriod
                           )} else {
                             timeSeries <- getSymbols(symbol, src = dataSource, from = (Sys.Date() - 500),
                                                      to = Sys.Date(),
                                                      auto.assign = FALSE
                             )}
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

plotMacd <- function(macd, symbol = "") {
  fig <- plot_ly(macd, x = ~date, y = ~macd,
                 name = "MACD",
                 type = "scatter",
                 mode = "line"
  )
  fig <- fig %>% add_trace(y = ~signal, name = 'Signal',
                           mode = "line")
  fig <- fig %>% layout(title = paste(symbol, "MACD Analysis"))

  fig
}

symbolText <- function() {
  symbols <- HTML("<b>Asia Pacific Ex Japan</b>
  <p>Fidelity Index Pacific ex Japan (Class P): 0P00011UPP.L</p>
  <p>HSBC Pacific Index (Class C): 0P0000WN7M.L</p>
  <p>Vanguard Pacific ex-Japan Stock Index: 0P0000KM1Z.L</p>
  <b>Europe Excluding UK</b>
  <p>Fidelity Index Europe ex UK: 0P00013O92.L</p>
  <p>HSBC European Index (Class C): 0P0000WN7I.L</p>
  <p>Vanguard FTSE Developed Europe ex-UK Equity Index: VERE.DE</p>
  <b>Europe Including UK</b>
  <p>Vanguard SRI European Stock: 0P0000UGLE.L</p>
  <b>Global</b>
  <p>Aviva Inv International Index Tracking: 0P0000A8XL.L</p>
  <p>Fidelity Index World: 0P0000XNUY.L</p>
  <p>HSBC FTSE All-World Index (Class C): 0P00013P6I.L</p>
  <p>Vanguard ESG Developed World All Cap Equity: 0P0000UGLG.L</p>
  <p>Vanguard FTSE Developed World ex-UK Equity Index: 0P0000KSP7.L</p>
  <p>Vanguard FTSE Global All Cap Index: 0P00018XAR.L</p>
  <p>Vanguard Global Small-Cap Index: 0P000147M1.L</p>
  <p>Vanguard LifeStrategy 100% Equity: 0P0000TKZO.L</p>
  <b>Global Emerging Markets</b>
  <p>Fidelity Index Emerging Markets: 0P00011YDA.L</p>
  <p>Henderson Rowe FTSE RAFI: 0P0000UHUM.L</p>
  <p>Vanguard Emerging Markets Stock Index: 0P000147LN.L</p>
  <b>Japan</b>
  <p>Fidelity Index Japan (Class P): 0P00013O93.L</p>
  <p>HSBC Japan Index (Class C): 0P0000WN7K.L</p>
  <p>Royal London Japan Tracker: 0P0000NAG3.L</p>
  <p>Vanguard Japan Stock Index: 0P000147M4.L</p>
  <b>North America</b>
  <p>Fidelity Index US (Class P): 0P0001FIOP.L</p>
  <p>HSBC American Index (Class C): 0P0000WN7H.L</p>
  <p>UBS S&P 500 Index: 0P000147T9.L</p>
  <b>UK</b>
  <p>ASI UK All Share Tracker (Class B): 0P0000VGXY.L</p>
  <p>Aviva Inv UK Index Tracking (Class 2): 0P0000XQBH.L</p>
  <p>Fidelity Index UK (Class P): 0P000125KT.L</p>
  <p>HSBC FTSE 100 Index (Class C): 0P0000WN79.L</p>
  <p>HSBC FTSE 250 Index (Class S): 0P000159K7.L</p>
  <p>HSBC FTSE All-Share Index (Class C): 0P0000WN7A.L</p>
  <p>Royal London FTSE 350 Tracker: 0P0000NAG0.L</p>
  <p>Royal London FTSE 350 Tracker: 0P0000NAG0.L</p>
                  ")
  # https://www.hl.co.uk/funds/index-tracker-funds/view-index-tracker-funds?investment=&x=47&y=12&companyid=&sectorid=121

  return(symbols)
}

indexFunds <- function() {
  #fundCategory <- c("Asia Pacific Ex Japan", "Asia Pacific Ex Japan", "Asia Pacific Ex Japan", "Europe Excluding UK", "Europe Excluding UK", "Europe Excluding UK", "Europe Including UK", "Global", "Global", "Global", "Global", "Global", "Global", "Global", "Global", "Global Emerging Markets", "Global Emerging Markets", "Global Emerging Markets", "Japan", "Japan", "Japan", "Japan", "North America", "North America", "North America", "UK", "UK", "UK", "UK", "UK", "UK", "UK")
  #fundNames <- c("Fidelity Index Pacific ex Japan (Class P)", "HSBC Pacific Index (Class C)", "Vanguard Pacific ex-Japan Stock Index", "Fidelity Index Europe ex UK", "HSBC European Index (Class C)", "Vanguard FTSE Developed Europe ex-UK Equity Index", "Vanguard SRI European Stock", "Aviva Inv International Index Tracking", "Fidelity Index World", "HSBC FTSE All-World Index (Class C)", "Vanguard ESG Developed World All Cap Equity", "Vanguard FTSE Developed World ex-UK Equity Index", "Vanguard FTSE Global All Cap Index", "Vanguard Global Small-Cap Index", "Vanguard LifeStrategy 100% Equity", "Fidelity Index Emerging Markets", "Henderson Rowe FTSE RAFI", "Vanguard Emerging Markets Stock Index", "Fidelity Index Japan (Class P)", "HSBC Japan Index (Class C)", "Royal London Japan Tracker", "Vanguard Japan Stock Index", "Fidelity Index US (Class P)", "HSBC American Index (Class C)", "UBS S&P 500 Index", "ASI UK All Share Tracker (Class B)", "Aviva Inv UK Index Tracking (Class 2)", "Fidelity Index UK (Class P)", "HSBC FTSE 100 Index (Class C)", "HSBC FTSE 250 Index (Class S)", "HSBC FTSE All-Share Index (Class C)", "Royal London FTSE 350 Tracker")
  #fundSymbol <- c("0P00011UPP.L", "0P0000WN7M.L", "0P0000KM1Z.L", "0P00013O92.L", "0P0000WN7I.L", "VERE.DE", "0P0000UGLE.L", "0P0000A8XL.L", "0P0000XNUY.L", "0P00013P6I.L", "0P0000UGLG.L", "0P0000KSP7.L", "0P00018XAR.L", "0P000147M1.L", "0P0000TKZO.L", "0P00011YDA.L", "0P0000UHUM.L", "0P000147LN.L", "0P00013O93.L", "0P0000WN7K.L", "0P0000NAG3.L", "0P000147M4.L", "0P0001FIOP.L", "0P0000WN7H.L", "0P000147T9.L", "0P0000VGXY.L", "0P0000XQBH.L", "0P000125KT.L", "0P0000WN79.L", "0P000159K7.L", "0P0000WN7A.L", "0P0000NAG0.L")

  #dF <- data.frame(fundCategory, fundNames, fundSymbol)
  dF <- read.csv("indexFund.csv")
  return(dF)
}

industryEtfs <- function() {
  dF <- read.csv("industryEtfs.csv")
  return(dF)
}

geographicalEtfs <- function() {
  dF <- read.csv("geographicalEtfs.csv")
  return(dF)
}
