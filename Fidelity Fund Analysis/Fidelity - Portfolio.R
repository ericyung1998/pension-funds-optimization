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

GEF <- read.table("Fidelity - Global Equity Fund.csv",header = T, sep=",", fileEncoding="UTF-8-BOM")
APEF <- read.table("Fidelity - Asia Pacific Equity Fund.csv",header = T, sep=",", fileEncoding="UTF-8-BOM")
GF <- read.table("Fidelity - Growth Fund.csv",header = T, sep=",", fileEncoding="UTF-8-BOM")
WBF <- read.table("Fidelity - World Bond Fund.csv",header = T, sep=",", fileEncoding="UTF-8-BOM")

attach(GEF)
attach(APEF)
attach(GF)
attach(WBF)

GEF$Date <- as.Date(GEF$Date,format="%m/%d/%Y")
GEF$Price <- as.numeric(GEF$Price)

APEF$Date <- as.Date(APEF$Date,format="%m/%d/%Y")
APEF$Price <- as.numeric(APEF$Price)

GF$Date <- as.Date(GF$Date,format="%m/%d/%Y")
GF$Price <- as.numeric(GF$Price)

WBF$Date <- as.Date(WBF$Date,format="%m/%d/%Y")
WBF$Price <- as.numeric(WBF$Price)

#########################################################################################################################

# FUND COMPARISON

plot(GEF$Date,GEF$Price,type="l",xlab="Date",main="Funds")
lines(APEF$Date, APEF$Price, type="l", col="red")
lines(GF$Date, GF$Price, type="l", col="blue")
lines(WBF$Date, WBF$Price, type="l", col="green")
legend("topleft",c("GEF","APEF", "GF", "WBF"), lty=c(1,1,1,1), col=c('black','red','blue','green'))

#########################################################################################################################

# OPTIMIZE PORTFOLIO

StartDate <- as.Date("2011-01-01")
EndDate <- as.Date("2021-11-20")

#tick <- c("Global Equity Fund", "Asia Pacific Equity Fund", "Growth Fund")
#tick <- c("Global Equity Fund", "Asia Pacific Equity Fund")
tick <- c("Global Equity Fund", "World Bond Fund")

GEF_trimmed <- GEF[GEF$Date >= StartDate & GEF$Date < EndDate, ]
APEF_trimmed <- APEF[APEF$Date >= StartDate & APEF$Date < EndDate, ]
GF_trimmed <- GF[GF$Date >= StartDate & GF$Date < EndDate, ]
GB_trimmed <- GF[GF$Date >= StartDate & GF$Date < EndDate, ]
WBF_trimmed <- WBF[WBF$Date >= StartDate & WBF$Date < EndDate, ]

#fund_price <- bind_rows(GEF_trimmed,APEF_trimmed,GF_trimmed)
#fund_price <- bind_rows(GEF_trimmed,APEF_trimmed)
fund_price <- bind_rows(GEF_trimmed,WBF_trimmed)
fund_price <- na.omit(fund_price)

head(fund_price)

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

wts_new <- c(0.9,0.1)

# calculate the portfolio returns
port_returns <- (sum(wts_new * mean_ret) + 1)^252 - 1

# calculate the portfolio risk
port_risk <- sqrt(t(wts_new) %*% (cov_mat %*% wts_new))

# calculate the Sharpe Ratio
sharpe_ratio <- port_returns/port_risk

# summary
c(port_returns, port_risk, sharpe_ratio)















