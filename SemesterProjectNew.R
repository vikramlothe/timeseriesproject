#clear global environment
rm(list=ls())

#call packages
library(readxl)
library(forecast)
library(urca)
library(xts)
library(stargazer)
library(vars)

#set a working directory
setwd("/Users/vikramlothe/Documents/ECON 483/Semester Project")

#read in data from excel
durspend = read_excel("/Users/vikramlothe/Documents/ECON 483/Semester Project/Data/PCEDG.xlsx")
nondurspend = read_excel("/Users/vikramlothe/Documents/ECON 483/Semester Project/Data/PCEND.xlsx")
servspend = read_excel("/Users/vikramlothe/Documents/ECON 483/Semester Project/Data/PCES.xlsx")
govben = read_excel("/Users/vikramlothe/Documents/ECON 483/Semester Project/Data/GSBpers.xlsx")
unem = read_excel("/Users/vikramlothe/Documents/ECON 483/Semester Project/Data/UNEM.xlsx")
recdummy = read_excel("/Users/vikramlothe/Documents/ECON 483/Semester Project/Data/Recession.xlsx")
gdpdef = read_excel("/Users/vikramlothe/Documents/ECON 483/Semester Project/Data/GDPDEF.xlsx")

#create a data frame with one date column and all relevant variables
mydat = data.frame(durspend[,1:2], nondurspend[,2], servspend[,2], govben[,2],
                   unem[,2], gdpdef[,2], recdummy[,2:3])

#put variables in real terms using gdp deflator
rdurspend = mydat$PCEDG/mydat$GDPDEF
rnondurspend = (mydat$PCEND+mydat$PCES)/mydat$GDPDEF
rgovben = mydat$GSBPERS/mydat$GDPDEF

#calculate inflation using gdp deflator
gdpdef = ts(mydat$GDPDEF, start=c(1993,1), end=c(2023,4), frequency=4)
inflation = 400*diff(log(gdpdef))

#declare variables as time series, calculate consumption and gov. benefits
#using gdp deflator
unem = ts(mydat$unem, start=c(1993,1), end=c(2023,4), frequency=4)

rgovben = ts(rgovben, start=c(1993,1), end=c(2023,4), frequency=4)
rgovch = 400*diff(log(rgovben))

rnondurspend = ts(rnondurspend, start=c(1993,1), end=c(2023,4), frequency=4)
rndch = 400*diff(log(rnondurspend))

rdurspend = ts(rdurspend, start=c(1993,1), end=c(2023,4), frequency=4)
rdch = 400*diff(log(rdurspend))

## Put in Paper
ts.plot(cbind(rnondurspend, rdurspend), col=c("black","red"),
        lty=c("solid", "solid"), main="Annual non-nominal spending, classified
        by type", ylab = "")
legend("topleft", bty = "n", legend=c("Non-durable spending", 
                                      "Durable Spending"),
       fill=c("black", "red"))

#test for stationarity in levels of percent changes, with drift only
ts.plot(cbind(rndch,rdch, rgovch), col=c("black", "red", "blue"), 
        lty=c("solid", "dashed", "dotted"), main="Annualized growth of 
        consumption and government benefits", ylab= "% growth, annualized")
legend("topleft", bty = "n", legend=c("Non-durable spending (including services)", 
                                       "Durable Spending", 
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
### tau2: significant 5%, tau3: significant at 10%
### I will assume stationarity because it makes more sense
summary(ur.df(inflation,type="trend", lags=10, selectlags="BIC"))
summary(ur.df(inflation,type="drift", lags=10, selectlags="BIC"))

###rgovch - test for stationarity with a drift
##stationary
summary(ur.df((rgovch),type="drift", lags=10, selectlags="BIC"))

###VAR Model for COVID-19 analysis
d_unem = diff(na.omit(unem))

recdummy[is.na(recdummy)] = 0
reccovid = ts(recdummy[,3], start=c(1993, 2), end=c(2023,4), frequency= 4)

z1=rndch*recdummy$Recgr[2:124]
z2=rdch*recdummy$Recgr[2:124]

yvector1 = ts.union(rgovch, rndch, rdch, d_unem, inflation)

## select optimal lag:
VARselect(yvector1,lag.max=8,type=c("const"))

varmodel=VAR(yvector1,p=3, type=c("const"))

impulse_rndch=irf(varmodel,impulse="rgovch",ortho = TRUE, 
                  cumuluative = FALSE,n.ahead=24, response="rndch")
impulse_rdch=irf(varmodel,impulse="rgovch",ortho = TRUE,
                 cumulative = FALSE, n.ahead=24, response="rdch")
impulse_d_unem1=irf(varmodel, impulse="rndch", ortho = TRUE, n.ahead=24, response="d_unem")
impulse_d_unem2=irf(varmodel, impulse="rdch", ortho = TRUE, n.ahead=24, response="d_unem")
impulse_inflation1=irf(varmodel, impulse="rndch", ortho = TRUE,
                       cumulative = TRUE, n.ahead=24, response="inflation")
impulse_inflation2=irf(varmodel, impulse="rdch", ortho = TRUE, 
                       cumulative= TRUE, n.ahead=24, response="inflation")
durratio=rdurspend/(rdurspend+rnondurspend)

plot(impulse_rndch)
plot(impulse_rdch)
plot(impulse_d_unem1)
plot(impulse_d_unem2)
plot(impulse_inflation1)
plot(impulse_inflation2)

plot(durratio, main="Ratio of durable consumption to total consumption")


                   





















