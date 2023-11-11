#loading the libraries
library("tidyverse")

library("tidyquant")

#Step 1: Download prices and calculate returns(daily)
port_dailyret <- tq_get(c("DIS", "JNJ", "KO", "UAL", "TSLA", "SPY", "BND"), from ='2018-12-31') %>% 
  
  group_by(symbol) %>% 
  
  tq_transmute(select =adjusted,
               
               mutate_fun = periodReturn,
               
               period = "daily",
               
               col_rename = "ret")



#Step 2: Constructing the main portfolio for daily returns
port_retwd <- port_dailyret %>% 
  
  tq_portfolio(assets_col = symbol,
               
               returns_col = ret,
               
               weights = c(0.20, 0.20, 0.20, 0.20, 0.20, 0.00, 0.00),
               
               col_rename = "CumulativeValue",
               
               wealth.index =TRUE) %>% 
  
  add_column(symbol = "MyPort", .before =1)


#Step 3: SPY portfolio
port_retwspy <- port_dailyret %>%
  
  tq_portfolio(assets_col = symbol,
               
               returns_col = ret,
               
               weights = c(0.00, 0.00, 0.00, 0.000, 0.00, 1.00, 0.00),
               
               col_rename = "CumulativeValue",
               
               wealth.index =TRUE) %>% 
  
  add_column(symbol = "SPY", .before =1)

#Step 4: BND portfolio
port_retwbnd <- port_dailyret %>%
  
  tq_portfolio(assets_col = symbol,
               
               returns_col = ret,
               
               weights = c(0.00, 0.00, 0.00, 0.000, 0.00, 0.00, 1.00),
               
               col_rename = "CumulativeValue",
               
               wealth.index =TRUE) %>% 
  
  add_column(symbol = "BND", .before =1)

#Step 5: Combining all the portfolios together
all_port <- rbind(port_retwd, port_retwspy, port_retwbnd)  

#Step 6: Plotting the portfolios
  all_port %>%  ggplot(aes(date, CumulativeValue, color =symbol)) +   
       geom_line()+ ylab("Growth of $1 invested 5 years ago")