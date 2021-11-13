# This is an R script written to conduct technical analysis on Cryptocurrency Coins
# Include all necessary libraries
# Data Processing Libraries
library(tidyr)
library(dplyr)
library(skimr)
library(stringr)
library(stringi)

# Plotting Libraries
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(forecast)
library(pso)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(prophet)

# API libraries
library(jsonlite)
library(geckor)

# Get Market Data
getCurrentMarketData <- function() {
  url <- "https://api.coingecko.com/api/v3/coins/markets?vs_currency=usd"
  data <- fromJSON(url)
  market_data <- as.data.frame(data)
  market_data <- data %>% select(symbol, id, current_price, market_cap, total_volume, high_24h, low_24h, price_change_24h, price_change_percentage_24h)
  return(market_data)
}

# Filter Market Date
filterCurrentMarketDate <- function(listOfCurrencies, market_data) {
  filtered <- market_data %>% filter(id %in% listOfCurrencies) %>% select(id, current_price, price_change_percentage_24h)
  return(filtered)
}

# Get Historical Market Data for list of coins
getHistoricalMarketData <- function(listCoins) {
  historical_market_df = data.frame()
  for (coin in listCoins) {
    historical_market <- coin_history(coin,
                                      vs_currency = "usd",
                                      180,
                                      interval = "daily",
                                      max_attempts = 3
    )
    historical_market_df <- rbind(historical_market_df, historical_market) 
  }
  return(historical_market_df)
}

# Convert coin name to coinsymbol
convertCoinNameToSymbol <- function(coinname, market_data) {
  # As the coin is passed in via its name (i.e. ethereum), we need to first obtain its symbol, in order to grab its OHLV data from online
  return(market_data[market_data$id == coinname,'symbol'])
}

# Plot Chartseries for in-depth technical analysis
chartSeriesFunc <- function(coin_id, theme, months, typechart) {
  x <- getSymbols(paste0(coin_id, '-USD'), auto.assign=FALSE) %>% na.omit() %>% na.approx()
  chartSeries(x, type=typechart, subset = paste0("last ", as.character(months), " months"), theme = chartTheme(theme), log.scale = FALSE, TA=c(addVo(), addSMA(n=20, col='blue'), addSMA(n=30, col="red"), addBBands(n=20, sd=2, maType="SMA"), addRSI(n=14, maType="EMA", wilder=TRUE),addSMI(n=13,slow=25,fast=2,signal=9,ma.type="EMA")))
}

# Plot treemap for current market data
plotTreeMap <- function(marketdf) {
  df1 <- na.omit(marketdf[,c('id','market_cap')])
  df1$market_cap <- as.numeric(df1$market_cap)
  df1$formatted_market_cap <-  paste0(df1$id,'\n','$',format(df1$market_cap,big.mark = ',',scientific = F, trim = T))
  treemap(df1, index = 'formatted_market_cap', vSize = 'market_cap', title = 'Cryptocurrency Market Cap', fontsize.labels=c(20, 10), palette='RdYlGn', algorithm = "squarified", aspRatio = 2)
}

# Plot line graphs for historical market data
plotHistoricalMarketCapLine <- function(historicalmarketdf) {
  options(scipen = 999)
  cbbPalette <- c("#CC79A7", "#0072B2", "#D55E00")
  plot <- historicalmarketdf %>% mutate(date = as.Date(date(timestamp)), coin_id = factor(coin_id), market_cap = market_cap / 1000000) 
  plot %>% ggplot(aes(x=date, y=log10(market_cap), colour=coin_id)) + geom_line() + scale_colour_manual(values=cbbPalette) +
    scale_x_date(date_labels="%b %y",date_breaks  ="1 month") +
    theme(text = element_text(size=13)) +
    labs(title="Market Capitalisation Over Time",
         x ="Date", y = "Market Capitalisation (Log 10 USD millions)")
}

# Prediction of prices using Prophet
predictPrice <- function(coinid) {
  # Get coin history first
  coin_history_df <- coin_history(coin_id = coinid, vs_currency = "usd", days = "max") %>% mutate(timestamp = as.Date(timestamp)) %>% drop_na()
  
  # Since it will difficult to visualise the changes in prices when they are very high / very low, it is viable to perform a log-transformation
  ds <- coin_history_df$timestamp
  y <- log(coin_history_df$price)
  df <- data.frame(ds, y)
  qplot(ds, y, data=df)
  m <- prophet(df)
  return(m)
}

plotProphet <- function(m) {
  future <- make_future_dataframe(m, periods = 365)
  tail(future)
  forecast <- predict(m, future)
  dyplot.prophet(m, forecast)
}

plotForecast <- function(m) {
  future <- make_future_dataframe(m, periods = 365)
  forecast <- predict(m, future)
  prophet_plot_components(m, forecast)
}

# Get optimized portfolio weights
optimizePortfolio <- function(tickers) {
  portfolioPrices <- NULL
  for(ticker in tickers) {
    print(ticker)
    portfolioPrices <- cbind(portfolioPrices,
                             getSymbols.yahoo(paste0(ticker, '-USD'), auto.assign=FALSE)[,4])
  }
  
  portfolioReturns <- na.omit(ROC(portfolioPrices))
  
  portf <- portfolio.spec(colnames(portfolioReturns))
  
  portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
  portf <- add.constraint(portf, type="box", min=.10, max=.40)
  portf <- add.objective(portf, type="return", name="mean")
  portf <- add.objective(portf, type="risk", name="StdDev")
  
  optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "pso", trace=TRUE)
  return(optPort)
}

# Plot efficient frontier
plotEfficientFrontier <- function(optPort) {
  ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 25,
                                 risk_aversion = NULL)
  chart.EfficientFrontier(ef,
                          match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                          cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                          RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                          chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                          cex.assets = 0.8)
}
