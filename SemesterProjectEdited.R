#clear global environment
rm(list=ls())

#call packages
library(quantmod)
library(readxl)
library(forecast)
library(urca)
library(xts)
library(stargazer)
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

#Clean Data: Make all data quarterly, get common 31 year period
durspend = window(PCEDG, start = as.Date("1993-01-01"), end = as.Date("2023-12-31"))
ndurspend = window(PCEND, start = as.Date("1993-01-01"), end = as.Date("2023-12-31"))
servspend = window(PCES, start = as.Date("1993-01-01"), end = as.Date("2023-12-31"))
govben = window(B087RC1Q027SBEA, start = as.Date("1993-01-01"), end = as.Date("2023-12-31"))
unem = window(UNRATE, start = as.Date("1993-01-01"), end = as.Date("2023-12-31"))
rec = window(JHDUSRGDPBR, start = as.Date("1993-01-01"), end = as.Date("2023-12-31"))
gdpdef = window(GDPDEF, start = as.Date("1993-01-01"), end = as.Date("2023-12-31"))

durspend = to.quarterly(durspend, OHLC=FALSE)
ndurspend = to.quarterly(ndurspend, OHLC=FALSE)
servspend = to.quarterly(servspend, OHLC=FALSE)
govben = to.quarterly(govben, OHLC=FALSE)
unem = to.quarterly(unem, OHLC=FALSE)
rec = to.quarterly(rec, OHLC=FALSE)
gdpdef = to.quarterly(gdpdef, OHLC=FALSE)

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
#log-difference transformations put inflation, spending variables into 
#percentage change form
#Change: Inflation and spending variables NOT multiplied by 400 to prevent
#kurtosis
gdpdef = ts(mydat$gdpdef, start=c(1993,1), end=c(2023,4), frequency=4)
inflation = diff(log(gdpdef))

#declare variables as time series, calculate consumption and gov. benefits
#using gdp deflator
unem = ts(mydat$unem, start=c(1993,1), end=c(2023,4), frequency=4)

rgovben = ts(rgovben, start=c(1993,1), end=c(2023,4), frequency=4)
rgovch = diff(log(rgovben))

rnondurspend = ts(rnondurspend, start=c(1993,1), end=c(2023,4), frequency=4)
rndch = diff(log(rnondurspend))

rdurspend = ts(rdurspend, start=c(1993,1), end=c(2023,4), frequency=4)
rdch = diff(log(rdurspend))

## Create plots
ts.plot(cbind(rnondurspend, rdurspend), col=c("black","red"),
        lty=c("solid", "solid"), main="Annual real consumption, classified
        by type", ylab = "")
legend("topleft", bty = "n", legend=c("Non-durable spending", 
                                      "Durable Spending"),
       fill=c("black", "red"))

#test for stationarity in levels of percent changes, with drift only
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
        1993-2023", ylab="Percent")
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
summary(ur.df(na.omit(diff(rdch)),type="drift", lags=10, selectlags="BIC"))

###rndch - test for stationarity with a drift
#stationary
summary(ur.df(na.omit(diff(rndch)),type="drift", lags=10, selectlags="BIC"))

###inflation test for stationarity with both trend and drift
### significant at 5%, but not 1%
### I will assume stationarity because it makes more sense
summary(ur.df(inflation,type="trend", lags=10, selectlags="BIC"))
summary(ur.df(inflation,type="drift", lags=10, selectlags="BIC"))

###rgovch - test for stationarity with a drift
##stationary
summary(ur.df((rgovch),type="drift", lags=10, selectlags="BIC"))

###VAR Model for COVID-19 analysis
d_unem = diff(na.omit(unem))
yvector1 = ts.union(rgovch, rndch, rdch, d_unem, inflation)

## select optimal lag:
VARselect(yvector1,lag.max=8,type=c("const"))

varmodel=VAR(yvector1,p=3, type=c("const"))

#turn off scientific notation for interpretable graphs
options(scipen=999)

#non-cumulative impulse response functions
impulse_rndch=irf(varmodel,impulse="rgovch",ortho = TRUE, 
                  cumuluative = FALSE,n.ahead=24, response="rndch")
impulse_rdch=irf(varmodel,impulse="rgovch",ortho = TRUE,
                 cumulative = FALSE, n.ahead=24, response="rdch")
impulse_d_unem1=irf(varmodel, impulse="rndch", ortho = TRUE, n.ahead=24, response="d_unem")
impulse_d_unem2=irf(varmodel, impulse="rdch", ortho = TRUE, n.ahead=24, response="d_unem")
impulse_inflation1=irf(varmodel, impulse="rndch", ortho = TRUE,
                       cumulative = FALSE, n.ahead=24, response="inflation")
impulse_inflation2=irf(varmodel, impulse="rdch", ortho = TRUE, 
                       cumulative= FALSE, n.ahead=24, response="inflation")

plot(impulse_rndch)
plot(impulse_rdch)
plot(impulse_d_unem1)
plot(impulse_d_unem2)
plot(impulse_inflation1)
plot(impulse_inflation2)

#cumulative impulse response functions
#check last four irfs as these were the only interesting ones
cimpulse_d_unem1=irf(varmodel, impulse="rndch", ortho = TRUE,
                     cumulative = TRUE, n.ahead=24, response="d_unem")
cimpulse_d_unem2=irf(varmodel, impulse="rdch", ortho = TRUE,
                     cumulative = TRUE, n.ahead=24, response="d_unem")
cimpulse_inflation1=irf(varmodel, impulse="rndch", ortho = TRUE,
                       cumulative = TRUE, n.ahead=24, response="inflation")
cimpulse_inflation2=irf(varmodel, impulse="rdch", ortho = TRUE, 
                       cumulative= TRUE, n.ahead=24, response="inflation")

plot(cimpulse_d_unem1)
plot(cimpulse_d_unem2)
plot(cimpulse_inflation1)
plot(cimpulse_inflation2)


                   





















