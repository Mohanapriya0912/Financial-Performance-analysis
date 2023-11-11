#Module_5 Total Return Analysis - NVIDIA vs Meta vs Adobe vs Using Cumulative Returns
#Loading Libraries
rm(list = ls())
library("tidyverse")
library("tidyquant")

#Loading stock data from yahoo_finance for last five years
start = as.Date("2017-12-31") 
end = as.Date("2023-10-01")
tickers = c("NVDA","META","ADBE","GOOG","MSFT") 
stocks <- tq_get(tickers, from = start, to = end)



#Calculate returns for each security
returns_all <- stocks %>%   
  group_by(symbol) %>%   
  tq_transmute(select= adjusted,                
               mutate_fun = periodReturn,                
               period = "daily",                
               col_rename = "returns")

#Calculate Cumulative product for the security Gross Total Returns
returns_all <- returns_all %>% mutate(cum_returns =cumprod(1+returns))

#Plot the Cumulative Returns using ggplots 
returns_all %>% ggplot(aes(date, cum_returns, color =symbol)) +
labs(title = "Top US Tech : Total Returns Analysis",
     subtitle = "Using Adjusted Prices",
     caption = " Source: Yahoo Finance") +
     xlab("Date") + ylab("Value of Investment") +
  geom_line()