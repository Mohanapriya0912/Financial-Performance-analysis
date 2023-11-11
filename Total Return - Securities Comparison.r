#Module_5 Total Return Analysis - NVIDIA vs Meta vs Adobe vs Using Cumulative Returns
#Loading Libraries
rm(list = ls())
library(quantmod)
library(ggplot2)
library(broom)

#Loading stock data from yahoo_finance for last five years
start = as.Date("2017-12-31") 
end = as.Date("2023-10-01")
getSymbols(c("NVDA","META","ADBE","GOOG","MSFT"), src = "yahoo", from = start, to = end)


#Data set of Gross Total Returns
nvda_ruturns <- 1 + Delt(NVDA$NVDA.Adjusted)
meta_returns <- 1 + Delt(META$META.Adjusted)
adobe_returns <- 1 + Delt(ADBE$ADBE.Adjusted) 
google_returns <- 1 + Delt(GOOG$GOOG.Adjusted)
msft_returns <- 1 + Delt(MSFT$MSFT.Adjusted)

#Combine gross total returns for each security in single data set
all_returns <- cbind(nvda_ruturns, meta_returns, adobe_returns, google_returns,msft_returns)
names(all_returns) <- c("NVDA", "META", "ADBE", "GOOG", "MSFT")

#Replace initial investment as $1 ->  Oct 1st, 2018
all_returns[1, ] <- c(1, 1, 1, 1,1)

#Calculate Cumulative product for the security Gross Total Returns
cummulate_returns <- cumprod(all_returns)
cummulate_returns <- tidy(cummulate_returns)

#Plot the Cumulative Returns using ggplots 

ggplot(cummulate_returns, aes(index,value,color=series)) +
  labs(title = "Top US Tech : Total Returns Analysis",
       subtitle = "Using Adjusted Prices",
       caption = " Source: Yahoo Finance") +
       xlab("Date") + ylab("Value of Investment") +
  scale_color_manual(values = c("green", "black", "orange", "blue", "firebrick")) +
  geom_line()

 
