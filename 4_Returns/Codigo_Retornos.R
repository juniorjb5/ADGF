


install.packages("quantmod")
install.packages("jsonlite")


library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)



symbols <- c("SPY","EFA", "IJS", "EEM","AGG")


prices <-
  getSymbols(symbols,
             src = 'yahoo',
             from = "2012-12-31",
             to = "2017-12-31",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.)))



prices <-
  getSymbols(symbols,
             src = 'yahoo',
             from = "2012-12-31",
             to = "2017-12-31",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)





prices_monthly <- to.monthly(prices,
                             indexAt = "lastof",
                             OHLC = FALSE)

head(prices_monthly, 7)




asset_returns_xts <-
  Return.calculate(prices_monthly,
                   method = "log") %>%
  na.omit()

head(round(asset_returns_xts,3), 7)








asset_returns_dplyr_byhand <- prices %>%
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  # convert the index to a date
  data.frame(date = index(.)) %>%
  # now remove the index because it got converted to row names
  remove_rownames() %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  mutate(returns = (log(prices) - log(lag(prices)))) %>%
  select(-prices) %>%
  spread(asset, returns) %>%
  select(date, symbols)





head(asset_returns_dplyr_byhand, 10)


asset_returns_dplyr_byhand <-
  asset_returns_dplyr_byhand %>%
  na.omit()






asset_returns_tq_1 <-
  prices %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  gather(asset, prices, -date) %>%
  filter(asset == "SPY") %>% 
  tq_transmute(select = prices, mutate_fun = periodReturn,
               period = "monthly",
               type = "log",
               col_rename = "SPY")

asset_returns_tq_2 <-
  prices %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  gather(asset, prices, -date) %>%
  filter(asset == "EFA") %>% 
  tq_transmute(select = prices, mutate_fun = periodReturn,
               period = "monthly",
               type = "log",
               col_rename = "EFA")  

asset_returns_tq_3 <-
  prices %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  gather(asset, prices, -date) %>%
  filter(asset == "IJS") %>% 
  tq_transmute(select = prices, mutate_fun = periodReturn,
               period = "monthly",
               type = "log",
               col_rename = "IJS")  


asset_returns_tq_4 <-
  prices %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  gather(asset, prices, -date) %>%
  filter(asset == "EEM") %>% 
  tq_transmute(select = prices, mutate_fun = periodReturn,
               period = "monthly",
               type = "log",
               col_rename = "EEM")  


asset_returns_tq_5 <-
  prices %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  gather(asset, prices, -date) %>%
  filter(asset == "AGG") %>% 
  tq_transmute(select = prices, mutate_fun = periodReturn,
               period = "monthly",
               type = "log",
               col_rename = "AGG")  

asset_returns_tq_builtin <-
  left_join(asset_returns_tq_1,asset_returns_tq_2, by = "date") %>% 
  left_join(. , asset_returns_tq_3, by = "date") %>% 
  left_join(. , asset_returns_tq_4, by = "date") %>%   
  left_join(. , asset_returns_tq_5, by = "date") %>% 
  slice(-1)

head(asset_returns_tq_builtin, 10)





head(asset_returns_xts, 3)

head(asset_returns_dplyr_byhand, 3)

head(asset_returns_tq_builtin, 3)




library(highcharter)


highchart(type = "stock") %>%
  hc_title(text = "Monthly Log Returns") %>%
  hc_add_series(asset_returns_xts[, symbols[1]],
                name = symbols[1]) %>%
  hc_add_series(asset_returns_xts[, symbols[2]],
                name = symbols[2]) %>%
  hc_add_series(asset_returns_xts[, symbols[3]],
                name = symbols[3]) %>%
  hc_add_series(asset_returns_xts[, symbols[4]],
                name = symbols[4]) %>%
  hc_add_series(asset_returns_xts[, symbols[5]],
                name = symbols[5]) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)








library(highcharter)

hc_hist <- hist(asset_returns_xts[, symbols[1]],breaks = 50,plot = FALSE)


hchart(hc_hist, color = "cornflowerblue") %>%
  hc_title(text = paste(symbols[1], "Log Returns Distribution",
                        sep = " ")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE)  








autoplot(asset_returns_xts,facets = TRUE)





asset_returns_long <-
  asset_returns_dplyr_byhand %>%
  gather(asset, returns, -date) %>%
  group_by(asset)


head(asset_returns_long, 10)








asset_returns_long %>%
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.45, binwidth = .005) +
  ggtitle("Monthly Returns Since 2013")







library(plotly)

g1<-asset_returns_long %>%
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.45, binwidth = .005) +
  ggtitle("Monthly Returns Since 2013")


ggplotly(g1)






asset_returns_long %>%
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.45, binwidth = .005) +
  ggtitle("Monthly Returns Since 2013")





asset_returns_long %>%
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.45, binwidth = .01) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2013") +
  theme_update(plot.title = element_text(hjust = 0.5))



asset_returns_long %>%
  ggplot(aes(x = returns, colour = asset)) +
  geom_density(alpha = 1) +
  ggtitle("Monthly Returns Density Since 2013") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))



asset_returns_long %>%
  ggplot(aes(x = returns)) +
  geom_density(aes(color = asset), alpha = 1) +
  geom_histogram(aes(fill = asset), alpha = 0.45, binwidth = .01) +
  guides(fill = FALSE) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2013") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))




asset_returns_long %>%
  ggplot(aes(x = date, y = returns, color=asset)) +
  geom_line()+
  ggtitle("Monthly Returns Since 2013") +
  xlab("Date") +
  ylab("Returns") +
  theme_update(plot.title = element_text(hjust = 0.5))




w <- c(0.25, 0.25, 0.20, 0.20, 0.10)

tibble(w, symbols)

# Corroborar la suma de las proporciones
tibble(w, symbols) %>%
  summarise(total_weight = sum(w))







w_1 <- w[1]
w_2 <- w[2]
w_3 <- w[3]
w_4 <- w[4]
w_5 <- w[5]



asset1 <- asset_returns_xts[,1]
asset2 <- asset_returns_xts[,2]
asset3 <- asset_returns_xts[,3]
asset4 <- asset_returns_xts[,4]
asset5 <- asset_returns_xts[,5]


portfolio_returns_byhand <-
  (w_1 * asset1) +
  (w_2 * asset2) +
  (w_3 * asset3) +
  (w_4 * asset4) +
  (w_5 * asset5)

names(portfolio_returns_byhand) <- "returns"







portfolio_returns_xts_rebalanced_monthly <-
  Return.portfolio(asset_returns_xts,
                   weights = w,
                   rebalance_on = "months") %>%
  `colnames<-`("returns")

head(portfolio_returns_xts_rebalanced_monthly, 10)







highchart(type = "stock") %>%
  hc_title(text = "Portfolio Monthly Returns") %>%
  hc_add_series(portfolio_returns_xts_rebalanced_monthly$returns,
                name = "Rebalanced Monthly",
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)






hc_portfolio <-
  hist(portfolio_returns_xts_rebalanced_monthly$returns,
       breaks = 50,
       plot = FALSE)
hchart(hc_portfolio,
       color = "cornflowerblue",
       name = "Portfolio") %>%
  hc_title(text = "Portfolio Returns Distribution") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE)







asset_returns_long %>%
  group_by(asset) %>%
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5])) %>%
  
  
  head(20)







portfolio_returns_dplyr_byhand <-
  asset_returns_long %>%
  group_by(asset) %>%
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5]),
         weighted_returns = returns * weights) %>%
  group_by(date) %>%
  summarise(returns = sum(weighted_returns))


head(portfolio_returns_dplyr_byhand, 10)





portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")

head(portfolio_returns_tq_rebalanced_monthly, 10)











portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = date, y = returns)) +
  geom_point(colour = "cornflowerblue")+
  xlab("date") +
  ylab("monthly return") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Portfolio Returns Scatter") +
  scale_x_date(breaks = pretty_breaks(n=6))







portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = .005,
                 fill = "cornflowerblue",
                 color = "cornflowerblue") +
  ggtitle("Portfolio Returns Distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))











portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")

portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = .005,
                 fill = "cornflowerblue",
                 color = "cornflowerblue") +
  ggtitle("Portfolio Returns Distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))










asset_returns_long %>%
  ggplot(aes(x = returns,
             fill = asset)) +
  geom_histogram(alpha = 0.15,
                 binwidth = .01) +
  geom_histogram(data = portfolio_returns_tq_rebalanced_monthly,
                 fill = "cornflowerblue",
                 binwidth = .01) +
  ggtitle("Portfolio and Asset Monthly Returns") +
  theme_update(plot.title = element_text(hjust = 0.5))







portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = .01,
                 colour = "cornflowerblue",
                 fill = "cornflowerblue") +
  geom_density(alpha = 1, color = "red") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Portfolio Histogram and Density")


















