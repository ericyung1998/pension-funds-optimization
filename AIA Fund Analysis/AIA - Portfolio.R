rm(list = ls())
par(mfrow=c(1,1))
setwd("")  # set directory

library(tidyquant)
library(tidyverse)
library(dplyr)
library(tibble)
library(plotly)
library(timetk)

#########################################################################################################################

  # DATA PREPARATION

NAEF <- read.table("AIA - North American Equity Fund.csv",header = T, sep=",")
GCEF <- read.table("AIA - Greater China Equity Fund.csv",header = T, sep=",")
GBF <- read.table("AIA - Global Bond Fund.csv",header = T, sep=",")

attach(NAEF)
attach(GCEF)
attach(GBF)

NAEF$Date <- as.Date(NAEF$Date,format="%m/%d/%Y")
NAEF$Price <- as.numeric(NAEF$Price)

GCEF$Date <- as.Date(GCEF$Date,format="%m/%d/%Y")
GCEF$Price <- as.numeric(GCEF$Price)

GBF$Date <- as.Date(GBF$Date,format="%m/%d/%Y")
GBF$Price <- as.numeric(GBF$Price)

#########################################################################################################################

  # FUND COMPARISON

plot(NAEF$Date,NAEF$Price,type="l",xlab="Date",main="Funds")
lines(GCEF$Date, GCEF$Price, type="l", col="red")
lines(GBF$Date, GBF$Price, type="l", col="blue")
legend("topleft",c("NAEF","GCEF", "GBF"), lty=c(1,1,1), col=c('black','red','blue'))

#########################################################################################################################

  # LOG RETURNS

par(mfrow=c(3,2))

StartYear <- 2007

    # North America Equity Fund    

NAEF_Time <- seq(from=StartYear,to=2021,length.out=length(NAEF$Price))
NAEF_LogReturn <- diff(log(NAEF$Price))

plot(NAEF_Time,NAEF$Price,type="l",xlab="Date",main="NA Equity Fund Price \n2007 - 2021")
plot(NAEF_Time[2:length(NAEF$Price)], NAEF_LogReturn,type="l",xlab="Date",main="NA Equity Fund Log Returns \n2007 - 2021")

    # Greater China Equity Fund

GCEF_Time <- seq(from=StartYear,to=2021,length.out=length(GCEF$Price))
GCEF_LogReturn <- diff(log(GCEF$Price))

plot(GCEF_Time,GCEF$Price,type="l",col='red',xlab="Date",main="GC Equity Fund Price \n2007 - 2021")
plot(GCEF_Time[2:length(GCEF$Price)], GCEF_LogReturn,type="l",col='red',xlab="Date",main="GC Equity Fund Log Returns \n2007 - 2021")

    # Global Bond Fund

GBF_Time <- seq(from=StartYear,to=2021,length.out=length(GBF$Price))
GBF_LogReturn <- diff(log(GBF$Price))

plot(GBF_Time,GBF$Price,type="l",xlab="Date",col='blue',main="Global Bond Fund Price \n2007 - 2021")
plot(GBF_Time[2:length(GBF$Price)], GBF_LogReturn,type="l",col='blue',xlab="Date",main="Global Bond Fund Log Returns \n2007 - 2021")

#########################################################################################################################

  # BENCHMARK TRACKING

normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

par(mfrow=c(3,1))

    # North America Equity Fund 

SPY <- 'SPY' %>% tq_get(from = '2002-01-01',to = '2021-11-19', get = 'stock.prices')

NAEF_Price_Norm <- normalize(x = NAEF$Price)
SPY_Price_Norm <- normalize(x = SPY$close)

plot(NAEF$Date,NAEF_Price_Norm,type="l",xlab="Date",main="AIA vs Market NA Equity \n2002 - 2021")
lines(SPY$date, SPY_Price_Norm, type='l', lty=3, col='red')
legend("topleft",c("AIA","Market"), lty=c(1,3), col=c('black','red'))

    # Greater China Equity Fund

GCEF_Price_Norm <- normalize(x = GCEF$Price)
GCEF_Time <- seq(from=2004,to=2021,length.out=length(GCEF$Price))

SSE <- '000001.SS' %>% tq_get(from = '2004-12-01',to = '2021-11-19', get = 'stock.prices')
SSE_Price_Norm <- normalize(x = SSE$close)
HSI <- '^HSI' %>% tq_get(from = '2004-12-01',to = '2021-11-19', get = 'stock.prices')
HSI_Price_Norm <- normalize(x = HSI$close)
TWII <- '^TWII' %>% tq_get(from = '2004-12-01',to = '2021-11-19', get = 'stock.prices')
TWII_Price_Norm <- normalize(x = TWII$close)

plot(GCEF$Date,GCEF_Price_Norm,type="l",xlab="Date",main="AIA vs Market GC Equity \n2004 - 2021")
lines(SSE$date, SSE_Price_Norm, type='l', lty=3, col='red')
lines(HSI$date, HSI_Price_Norm, type='l', lty=3, col='blue')
lines(TWII$date, TWII_Price_Norm, type='l', lty=3, col='green')
legend("topleft",c("AIA","SSE", "HSI", "TWII"), lty=c(1,3,3,3), col=c('black','red', 'blue', 'green'))

    # Global Bond Fund
GBF_Price_Norm <- normalize(x = GBF$Price)
GBF_Time <- seq(from=2004,to=2021,length.out=length(GBF$Price))

BND <- 'BND' %>% tq_get(from = '2007-12-01',to = '2021-11-19', get = 'stock.prices')
BND_Price_Norm <- normalize(x = BND$close)

plot(GBF$Date,GBF_Price_Norm,type="l",xlab="Date",main="AIA vs Market Global Bond \n2007 - 2021")
lines(BND$date, BND_Price_Norm, type='l', lty=3, col='red')
legend("topleft",c("AIA","BND"), lty=c(1,3), col=c('black','red'))

#########################################################################################################################

  # OPTIMIZE PORTFOLIO

StartDate <- as.Date("2011-01-01")
EndDate <- as.Date("2021-11-20")
tick <- c("NA Equity Fund", "GC Equity Fund", "Global Bond Fund")
#tick <- c("NA Equity Fund", "GC Equity Fund")

NAEF_trimmed <- NAEF[NAEF$Date >= StartDate & NAEF$Date < EndDate, ]
GCEF_trimmed <- GCEF[GCEF$Date >= StartDate & GCEF$Date < EndDate, ]
GBF_trimmed <- GBF[GBF$Date >= StartDate & GBF$Date < EndDate, ]

fund_price <- bind_rows(NAEF_trimmed,GCEF_trimmed,GBF_trimmed)
#fund_price <- bind_rows(NAEF_trimmed,GCEF_trimmed)
fund_price <- na.omit(fund_price)

log_ret_xts <- fund_price %>%
               group_by(Type) %>%
               tq_transmute(select = Price,
                             mutate_fun = periodReturn,
                             period = 'daily',
                             col_rename = 'ret',
                             type = 'log') %>%
               spread(key = Type, value = ret) %>%
               tk_xts()

log_ret_xts <- log_ret_xts[complete.cases(log_ret_xts), ]

head(log_ret_xts)

  # mean
mean_ret <- colMeans(log_ret_xts)
print(round(mean_ret, 5))

  # covariance matrix
cov_mat <- cov(log_ret_xts) * 252
print(round(cov_mat,4))

#####################

  # portfolio optimization (multiple)
num_port <- 10000

  # creating a matrix to store the weights
all_wts <- matrix(nrow = num_port,
                  ncol = length(tick))

  # creating an empty vector to store - Portfolio returns
port_returns <- vector('numeric', length = num_port)

  # creating an empty vector to store - Portfolio Standard deviation
port_risk <- vector('numeric', length = num_port)

  # creating an empty vector to store - Portfolio Sharpe Ratio
sharpe_ratio <- vector('numeric', length = num_port)

  # loop portfolio
for (i in seq_along(port_returns)) {
  
  wts <- runif(length(tick))
  wts <- wts/sum(wts)
  
  # weight matrix
  all_wts[i,] <- wts
  
  # portfolio returns
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  port_returns[i] <- port_ret
  
  # portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # portfolio Sharpe Ratio(risk free rate = 0%)
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}

#####################

  # storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)

  # converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret_xts)

  # combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

  # portfolios
min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

#####################

  # minimum variance
min_var_plot <- min_var %>%
  gather(tick[sort.list(tick)][1]:tick[sort.list(tick)][length(tick)], key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Min Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

  # highest sharpe ratio
max_sr_plot <- max_sr %>%
  gather(tick[sort.list(tick)][1]:tick[sort.list(tick)][length(tick)], key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Max Sharpe Ratio Portfolio Weights") +
  scale_y_continuous(labels = scales::percent)

  # efficient frontier
frontier_plot <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red')

#####################

  # RESULTS

#ggplotly(min_var_plot)
#ggplotly(max_sr_plot)
#ggplotly(frontier_plot)

min_var <- min_var %>% 
  add_column(Type = "Minimum Variance", .before = names(min_var)[1])
  
max_sr <- max_sr %>% 
  add_column(Type = "Maximum Sharpe Ratio", .before = names(max_sr)[1])

df <- union_all(min_var,max_sr)

fig <- subplot(min_var_plot, max_sr_plot, frontier_plot) %>%
  layout(title = 'Min Var + Max Sharpe Ratio Portfolio + Efficient Frontier')

fig
df

#########################################################################################################################

  # SINGLE PORTFOLIO

wts_new <- c(0.1,0.1,0.9)
  
  # calculate the portfolio returns
port_returns <- (sum(wts_new * mean_ret) + 1)^252 - 1

  # calculate the portfolio risk
port_risk <- sqrt(t(wts_new) %*% (cov_mat %*% wts_new))

  # calculate the Sharpe Ratio
sharpe_ratio <- port_returns/port_risk

  # summary
c(port_returns, port_risk, sharpe_ratio)
