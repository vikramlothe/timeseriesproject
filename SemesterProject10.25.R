#clear global environment
rm(list=ls())

#call packages
library(lubridate)
library(quantmod)
library(readxl)
library(forecast)
library(urca)
library(xts)
library(vars)

#set a working directory
setwd("/Users/vikramlothe/Documents/ECON 483/Semester Project")

#Download data from FRED
durspend = getSymbols("PCEDG", src = "FRED")
ndurspend = getSymbols("PCEND", src = "FRED")
servspend = getSymbols("PCES", src = "FRED")
govben = getSymbols("B087RC1Q027SBEA", src = "FRED")
unem = getSymbols("UNRATE", src = "FRED")
rec = getSymbols("JHDUSRGDPBR", src = "FRED")
gdpdef = getSymbols("GDPDEF", src="FRED")

#Clean Data: Make all data quarterly, get common 50 year period
durspend = window(PCEDG, start = as.Date("1974-01-01"), end = as.Date("2023-12-31"))
ndurspend = window(PCEND, start = as.Date("1974-01-01"), end = as.Date("2023-12-31"))
servspend = window(PCES, start = as.Date("1974-01-01"), end = as.Date("2023-12-31"))
govben = window(B087RC1Q027SBEA, start = as.Date("1974-01-01"), end = as.Date("2023-12-31"))
unem = window(UNRATE, start = as.Date("1974-01-01"), end = as.Date("2023-12-31"))
rec = window(JHDUSRGDPBR, start = as.Date("1974-01-01"), end = as.Date("2023-12-31"))
gdpdef = window(GDPDEF, start = as.Date("1974-01-01"), end = as.Date("2023-12-31"))

durspend = apply.quarterly(durspend,sum)
index(durspend) <- index(durspend) %m-% months(2)
ndurspend = apply.quarterly(ndurspend,sum)
index(ndurspend) <- index(ndurspend) %m-% months(2)
servspend = apply.quarterly(servspend,sum)
index(servspend) <- index(servspend) %m-% months(2)

#take the first value of every three months for unemployment
unem = apply.quarterly(unem, function(x) x[1])
index(unem) <- index(unem) %m-% months(2)
combined_xts <- merge(durspend, ndurspend, servspend, govben, unem, rec, gdpdef)
colnames(combined_xts) <- c("durspend", "ndurspend", "servspend", "govben", "unem", "rec", "gdpdef")
mydat <- data.frame(
  date = index(combined_xts),
  coredata(combined_xts)
)

head(mydat)

#put variables in real terms using gdp deflator
#consider non-durable spending as non-durable and service spending together
rdurspend = mydat$durspend/mydat$gdpdef
rnondurspend = (mydat$ndurspend+mydat$servspend)/mydat$gdpdef
rgovben = mydat$govben/mydat$gdpdef

#calculate inflation using gdp deflator
#log-difference transformations creates inflation from GDP Deflator, spending 
#variables into percentage change form
gdpdef = ts(mydat$gdpdef, start=c(1974,1), end=c(2023,4), frequency=4)
inflation = diff(log(gdpdef))

#declare variables as time series, calculate consumption and gov. benefits
#using gdp deflator
unem = ts(mydat$unem, start=c(1974,1), end=c(2023,4), frequency=4)
rgovben = ts(rgovben, start=c(1974,1), end=c(2023,4), frequency=4)
rgovch = diff(log(rgovben))
rnondurspend = ts(rnondurspend, start=c(1974,1), end=c(2023,4), frequency=4)
rndch = diff(log(rnondurspend))
rdurspend = ts(rdurspend, start=c(1974,1), end=c(2023,4), frequency=4)
rdch = diff(log(rdurspend))

## Create plots
ts.plot(cbind(rnondurspend, rdurspend), col=c("black","red"),
        lty=c("solid", "solid"), main="Annual real consumption, classified
by type", ylab = "")
legend("topleft", bty = "n", legend=c("Non-durable spending",
                                      "Durable Spending"),
       fill=c("black", "red"))

ts.plot(cbind(rndch,rdch, rgovch),
        col=c("black", "red", "blue"),
        lty=c("solid", "dashed", "dotted"),
        main="Annualized growth of
consumption and government benefits",
        ylab= "% growth, annualized")
legend("topleft",
       bty = "n",
       legend=c("Non-durable consumpton (including services)",
                "Durable consumption",
                "Real government benefits"),
       fill=c("black", "red", "blue"))

ts.plot(cbind(inflation, unem), col=c("black", "red"),
        lty=c("solid", "solid"), main="US National inflation and unemployment,
1974-2023", ylab="Percent")
legend("topleft", bty = "n", legend=c("inflation (GDP deflator)",
                                      "unemployment rate"),
       fill=c("black", "red"))

#stationarity tests
### unem - test for stationarity with trend and without trend, but w/o drift
#not stationary in levels
summary(ur.df(unem,type="trend", lags=10, selectlags="BIC"))
summary(ur.df(unem,type="none", lags=10, selectlags="BIC"))

#stationary in differences
summary(ur.df(na.omit(diff(unem)),type="trend", lags=10, selectlags="BIC"))
summary(ur.df(na.omit(diff(unem)),type="none", lags=10, selectlags="BIC"))

###rdch - test for stationarity with a drift
#stationary
summary(ur.df((rdch),type="drift", lags=10, selectlags="BIC"))

###rndch - test for stationarity with a drift
#stationary
summary(ur.df((rndch),type="drift", lags=10, selectlags="BIC"))

###inflation test for stationarity with both trend and drift
### not stationary
summary(ur.df(inflation,type="trend", lags=10, selectlags="BIC"))
summary(ur.df(inflation,type="drift", lags=10, selectlags="BIC"))

#stationary in first differences
summary(ur.df(na.omit(diff(inflation), type="trend", lags=10, selectlags="BIC")))
summary(ur.df(na.omit(diff(inflation), type="drift", lags=10, selectlags="BIC")))

###rgovch - test for stationarity with a drift
##stationary
summary(ur.df((rgovch),type="drift", lags=10, selectlags="BIC"))

### VAR Model for COVID-19 analysis
d2_unem <- diff(diff(na.omit(unem)))
d_inf <- diff(na.omit(inflation))
d_rgovch <- diff(na.omit(rgovch))
d_rndch <- diff(na.omit(rndch))
d_rdch <- diff(na.omit(rdch))

yvector1 <- ts.union(d_rgovch, d_rndch, d_rdch, d2_unem, d_inf)

VARselect(yvector1, lag.max = 8, type = "const")

# Estimate VAR with exogenous recession interactions
varmodel <- VAR(yvector1, p = 3, type = "const")

# Turn off scientific notation
options(scipen = 999)

# Impulse Response Functions 
impulse_d_rndch <- irf(varmodel, impulse = "d_rgovch", response = "d_rndch",
                       n.ahead = 8, ortho = TRUE)
impulse_d_rdch <- irf(varmodel, impulse = "d_rgovch", response = "d_rdch",
                      n.ahead = 8, ortho = TRUE)
impulse_d2_unem1 <- irf(varmodel, impulse = "d_rndch", response = "d2_unem",
                        n.ahead = 8, ortho = TRUE)
impulse_d2_unem2 <- irf(varmodel, impulse = "d_rdch", response = "d2_unem",
                        n.ahead = 8, ortho = TRUE)
impulse_d_inf1 <- irf(varmodel, impulse = "d_rndch", response = "d_inf",
                      n.ahead = 8, ortho = TRUE)
impulse_d_inf2 <- irf(varmodel, impulse = "d_rdch", response = "d_inf",
                      n.ahead = 8, ortho = TRUE)

plot(impulse_d_rndch)
plot(impulse_d_rdch)
plot(impulse_d2_unem1)
plot(impulse_d2_unem2)
plot(impulse_d_inf1)
plot(impulse_d_inf2)

# Result: 1-standard deviation positive shocks to non-durable consumption have 
# a much stronger temporary effect on lowering the unemployment rate than 
# 1-standard deviation positive shocks to durable consumption
