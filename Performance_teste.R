library(quantmod)
library(PerformanceAnalytics)

getSymbols(c("RAIL3.SA","ELET6.SA","ITUB4.SA", "LAME4.SA", "AZUL4.SA" ), src = "yahoo",  from = "2017-04-11", to = "2019-04-26")

PORT_1 <- RAIL3.SA$RAIL3.SA.Adjusted 
PORT_1$ELET6.SA <- ELET6.SA$ELET6.SA.Adjusted
PORT_1$ITUB4.SA <- ITUB4.SA$ITUB4.SA.Adjusted
PORT_1$LAME4.SA <- LAME4.SA$LAME4.SA.Adjusted
PORT_1$AZUL4.SA <- AZUL4.SA$AZUL4.SA.Adjusted
head(PORT_1)

por_weights <- c(0.2, 0.20, 0.20, 0.20, 0.20)

###Sum of Portfolio
port_value = PORT_1*por_weights
head(port_value)
port_tot <- xts()
port_total <- (port_value$RAIL3.SA.Adjusted+port_value$ELET6.SA+port_value$ITUB4.SA+port_value$LAME4.SA+port_value$AZUL4.SA)
head(port_total)

###Return Portfolio
ret_port1 <- Return.calculate(PORT_1)
head(ret_port1)
ret_port1 <- ret_port1[-1,]
ret_port1 <- na.fill(ret_port1, 0 )

rebal_port1 <- Return.portfolio(R = ret_port1, weights = por_weights, rebalance_on = "weeks")

charts.PerformanceSummary(rebal_port1, main = " Portfolio: RAIL3.SA,ELET6.SA,ITUB4.SA, LAME4.SA, AZUL4.SA", col=dark8equal )

rebalance <- Return.portfolio(R = ret_port1, weights = por_weights, rebalance_on = "weeks", verbose = TRUE)
head(rebalance$EOP.Weight)
head(rebalance$BOP.Weight)
head(rebalance$contribution)
head(rebalance$returns)


###Table of Annualized Return, Annualized Std Dev, and Annualized Sharpe###
table.AnnualizedReturns(rebal_port1, scale = 12, Rf= 0.0238/12)

### Anualized comands and charts
an_ret <- rebalance$returns
head(an_ret)
plot(cumsum(an_ret))
# Calculate the mean, volatility, and Sharpe ratio of returns
returns_ann <- Return.annualized(an_ret)
returns_ann
sd_ann <- StdDev.annualized(an_ret)
sd_ann
sharpe_ann <- SharpeRatio.annualized(an_ret, Rf=.035/30)
sharpe_ann

# Plotting the 12-month rolling annualized mean
chart.RollingPerformance(R = an_ret, width = 30, FUN = "Return.annualized")

# Plotting the 12-month rolling annualized standard deviation
chart.RollingPerformance(R = an_ret, width = 30, FUN = "StdDev.annualized")

# Plotting the 12-month rolling annualized Sharpe ratio
chart.RollingPerformance(R = an_ret, width = 30, FUN = "SharpeRatio.annualized", Rf=.035/30)


# Fill in window for 2008
port2016 <- window(an_ret, start = "2016-01-01", end = "2016-12-31")

# Create window for 2014
port2018 <- window(an_ret, start = "2018-01-01", end = "2018-12-31")

# Plotting settings
par(mfrow = c(1, 2) , mar=c(3, 2, 2, 2))
names(port2016) <- "port2016"
names(port2018) <- "port2018"

# Plot histogram of 2008
chart.Histogram(port2016, methods = c("add.density", "add.normal"))

# Plot histogram of 2014
chart.Histogram(port2018, methods = c("add.density", "add.normal"))

#  Compute the skewness 
skewness(an_ret)  
  

# Compute the excess kurtosis 
kurtosis(an_ret)

# Calculate the SemiDeviation
SemiDeviation(an_ret)

# Calculate the value at risk
VaR(an_ret,p = 0.05)
VaR(an_ret,p = 0.025)

# Calculate the expected shortfall
ES(an_ret,p = 0.05)
ES(an_ret,p = 0.025)

# Plot histogram of returns
chart.Histogram(an_ret, methods = c("add.density", "add.normal"))


# Table of drawdowns
table.Drawdowns(an_ret)

# Plot of drawdowns
chart.Drawdown(an_ret)
