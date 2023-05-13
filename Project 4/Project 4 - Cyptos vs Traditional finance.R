require (xts) # attache the package to convert our dataframes into an xts (time-series) object for simplicity in calculations
require(zoo)

library(tidyquant)

# import a time series of different frequency
# Stock 1 - Apple
AAPL = read.csv("AAPL.csv")
AAPL$Date = as.Date(AAPL$Date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
AAPL.xts = xts(AAPL['Close'], order.by=AAPL$Date)  # converting a data frame into xts onject (time-series object)
head(AAPL.xts)

aapl_return = diff (log(AAPL.xts)) # Log return calculation
aapl_return = aapl_return [-1] # removing the first empty observation, received after return calculation
head(aapl_return)
# Stock 2 - Amazon
AMZN = read.csv("AMZN.csv")
AMZN$Date = as.Date(AMZN$Date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
AMZN.xts = xts(AMZN['Close'], order.by=AMZN$Date)  # converting a data frame into xts onject (time-series object)
head(AMZN.xts)

amzn_return = diff (log(AMZN.xts)) # Log return calculation
amzn_return = amzn_return [-1] # removing the first empty observation, received after return calculation

# Stock 3 - Google
GOOG = read.csv("GOOG.csv")
GOOG$Date = as.Date(GOOG$Date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
GOOG.xts = xts(GOOG['Close'], order.by=GOOG$Date)  # converting a data frame into xts onject (time-series object)
head(GOOG.xts)

goog_return = diff (log(GOOG.xts)) # Log return calculation
goog_return = goog_return [-1] # removing the first empty observation, received after return calculation

# Stock 4 - Microsoft
MSFT = read.csv("MSFT.csv")
MSFT$Date = as.Date(MSFT$Date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
MSFT.xts = xts(MSFT['Close'], order.by=MSFT$Date)  # converting a data frame into xts onject (time-series object)
head(MSFT.xts)

msft_return = diff (log(MSFT.xts)) # Log return calculation
msft_return = msft_return [-1] # removing the first empty observation, received after return calculation

# Stock 5 - Tesla
TSLA = read.csv("TSLA.csv")
TSLA$Date = as.Date(TSLA$Date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
TSLA.xts = xts(TSLA['Close'], order.by=TSLA$Date)  # converting a data frame into xts onject (time-series object)
head(TSLA.xts)

tsla_return = diff (log(TSLA.xts)) # Log return calculation
tsla_return = tsla_return [-1] # removing the first empty observation, received after return calculation

# Cypto 1 – Cardano
ADA = read.csv("ADA-USD.csv")
ADA$Date = as.Date(ADA$Date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
ADA.xts = xts(ADA['Close'], order.by=ADA$Date)  # converting a data frame into xts onject (time-series object)
head(ADA.xts)

ada_return = diff (log(ADA.xts)) # Log return calculation
ada_return = ada_return [-1] # removing the first empty observation, received after return calculation

# Cypto 2 – BNB USD
BNB = read.csv("BNB-USD.csv")
BNB$Date = as.Date(BNB$Date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
BNB.xts = xts(BNB['Close'], order.by=BNB$Date)  # converting a data frame into xts onject (time-series object)
head(BNB.xts)

bnb_return = diff (log(BNB.xts)) # Log return calculation
bnb_return = bnb_return [-1] # removing the first empty observation, received after return calculation

# Cypto 3 – Bitcoin
BTC = read.csv("BTC-USD.csv")
BTC$Date = as.Date(BTC$Date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
BTC.xts = xts(BTC['Close'], order.by=BTC$Date)  # converting a data frame into xts onject (time-series object)
head(BTC.xts)

btc_return = diff (log(BTC.xts)) # Log return calculation
btc_return = btc_return [-1] # removing the first empty observation, received after return calculation

# Cypto 4 – Ethereum
ETH = read.csv("ETH-USD.csv")
ETH$Date = as.Date(ETH$Date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
ETH.xts = xts(ETH['Close'], order.by=ETH$Date)  # converting a data frame into xts onject (time-series object)
head(ETH.xts)

eth_return = diff (log(ETH.xts)) # Log return calculation
eth_return = eth_return [-1] # removing the first empty observation, received after return calculation

# Cypto 5 – XRP
XRP = read.csv("XRP-USD.csv")
XRP$Date = as.Date(XRP$Date,format="%Y-%m-%d", tz = "")# converting a date column into date format 
XRP.xts = xts(XRP['Close'], order.by=XRP$Date)  # converting a data frame into xts onject (time-series object)
head(XRP.xts)

xrp_return = diff (log(XRP.xts)) # Log return calculation
xrp_return = xrp_return [-1] # removing the first empty observation, received after return calculation

# Combine assets into a matrix
assets <- cbind(aapl_return$Close, amzn_return$Close, goog_return$Close, 
                msft_return$Close, tsla_return$Close, ada_return$Close, 
                bnb_return$Close, btc_return$Close, eth_return$Close, 
                xrp_return$Close)

merged_data <- merge(AAPL.xts, AMZN.xts, GOOG.xts, MSFT.xts, TSLA.xts, ADA.xts, 
                     BNB.xts, BTC.xts, ETH.xts, XRP.xts, all=FALSE)
head(merged_data)
# Calculate correlation matrix
correlation_matrix <- cor(merged_data)

colnames(correlation_matrix) <- c("Apple","Amazon","Google","Microsoft","Tesla",
                                  "Cardano","BNB","Bitcoin","Ethereum","XRP")
rownames(correlation_matrix) <-c("Apple","Amazon","Google","Microsoft","Tesla",
                                 "Cardano","BNB","Bitcoin","Ethereum","XRP")

# Print correlation matrix
print(correlation_matrix)

# Load the 'knitr' package
library(knitr)

# Convert the correlation matrix to a data frame
correlation_df <- as.data.frame(correlation_matrix)

# Print the table using the kable() function
kable(correlation_df, format = "markdown")

## Causality based on DAG
install.packages("pcalg")
library(pcalg)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("graph")
BiocManager::install("RBGL")
BiocManager::install("Rgraphviz")

library(pcalg)
library(graph)
library(ggm)

data = cbind(AAPL$Close, AAPL$Volume, AMZN$Close, AMZN$Volume, 
             GOOG$Close, GOOG$Volume, MSFT$Close, MSFT$Volume,
             TSLA$Close,TSLA$Volume, ADA$Close, ADA$Volume,
             BNB$Close, BNB$Volume, BTC$Close, BTC$Volume,
             ETH$Close, ETH$Volume, XRP$Close, XRP$Volume
             )
colnames(data) = c("AAPL_Close", "AAPL_Volume", "AMZN_Close", "AMZN_Volume", 
                   "GOOG_Close", "GOOG_Volume", "MSFT_Close", "MSFT_Volume",
                   "TSLA_Close","TSLA_Volume", "ADA_Close", "ADA_Volume",
                   "BNB_Close", "BNB_Volume", "BTC_Close", "BTC_Volume",
                   "ETH_Close", "ETH_Volume", "XRP_Close", "XRP_Volume")
head(data)

# Estimate the DAG:
suffStat = list(C = cor(data), n = nrow(data))
dag = pc(suffStat, indepTest = gaussCItest, alpha = 0.05, labels = colnames(data))
summary(dag)
# Plot the estimated DAG:
library(Rgraphviz)
plot(dag)

## Volatility

# Stock 1 - Apple:
## GARCH model
# Y return AAPL
Y = aapl_return$Close

library (tseries)
aapl.garch.1 <- garch(Y, order =c(1,1))
summary (aapl.garch.1)

library (rugarch)
aapl.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
aapl.garch.2 = ugarchfit(aapl.garch.spec, Y)
summary(aapl.garch.2)
plot (aapl.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

# Stock 1 - Cardano:
## GARCH model
# Y return ADA
Y = ada_return$Close

library (tseries)
ada.garch.1 <- garch(Y, order =c(1,1))
summary (ada.garch.1)

library (rugarch)
ada.garch.spec = ugarchspec(mean.model=list(armaOrder=c(0,0)), distribution="norm") 
ada.garch.2 = ugarchfit(ada.garch.spec, Y)
summary(ada.garch.2)
plot (ada.garch.2)

library(fGarch) # library for GARCH models
summary(garchFit(~garch(1,1), X ))

# Install and load the quantmod package
install.packages("quantmod")
library(quantmod)

# Volatility Calculation
sd(aapl_return$Close)
sd(amzn_return$Close)
sd(goog_return$Close)
sd(msft_return$Close)
sd(tsla_return$Close)
sd(ada_return$Close)
sd(bnb_return$Close)
sd(btc_return$Close)
sd(eth_return$Close)
sd(xrp_return$Close)

# Factor Analysis
df = cbind(AAPL$Close, AMZN$Close, 
             GOOG$Close, MSFT$Close,
             TSLA$Close,ADA$Close,
             BNB$Close, BTC$Close, 
             ETH$Close, XRP$Close
             )
colnames(df) <- c("Apple","Amazon","Google","Microsoft","Tesla",
                                  "Cardano","BNB","Bitcoin","Ethereum","XRP")
head(df)
fa_none = factanal(df[,1:10],4,rotation="none") # 4 factor model without rotation
print(fa_none,cutoff=0.1) # By convention, any loading with an absolute value less than the parameter cutoff is not printed, and the default value of cutoff is 0.1

# VARIMAX rotation
fa_vari = factanal(df[,1:10],4,rotation="varimax") #factor model with rotation

print(fa_vari,cutoff=0.1,sort=T)
print(fa_vari,cutoff=0.1)
sum(fa_vari$loadings[,1]^2)
B=fa_vari$loadings[,] # factor loadings

# Going into details

library (psych) # the library for factor analysis
library (GPArotation) # to estimate oblimin rotation
assets = df[,1:10]
describe(df) # general description of the data 

##Assessing the Factorability of the Data
#Bartlett's Test of Sphericity
cortest.bartlett(df)

#KMO
KMO(df)


##Determining the Number of Factors to Extract
# scree plot
scree(df)

#Parallel Analysis
fa.parallel (df) #

# estimation factor model
factor.model <- fa(df, nfactors = 2, fm="ols", max.iter = 100, rotate = "oblimin")
# we estimate a factor model with 2 factors, estimated by means of OLS, 100 is the number of iterations or attempts to use when identifying the "best"" solution
# rotate - we apply oblimin rotation, allowing factors to correlate.

# make it visual
fa.diagram(factor.model) # 

# Communality 
factor.model$communality

#Eeigenvalues
factor.model$e.values

#Percentage of Variance Accounted For
100*factor.model$e.values/length(factor.model$e.values)


print(factor.model$loadings, cutoff=0, digits=3)
print(factor.model$Structure, cutoff=0, digits=3)


## Monte Carlo Simulation to predict assets' price
# Stock 1 - Apple
library(quantmod)

# Define parameters for the simulation
days <- 365
trials <- 1000
lastPrice <- tail(AAPL$Close, n=1)

# Run the Monte Carlo simulation
simulatedPrices <- matrix(nrow=days, ncol=trials)
for (i in 1:trials) {
  simulatedReturns <- rnorm(days, mean=mean(aapl_return), sd=sd(aapl_return))
  simulatedPrices[,i] <- cumprod(1+simulatedReturns)*lastPrice
}

# Plot the results
colors <- rainbow(trials) # Generate a sequence of colors
plot(simulatedPrices[,1], type="l", col="blue", ylim=c(min(simulatedPrices),max(simulatedPrices)), xlab="Days", ylab="Apple Price")
for (i in 2:trials) {
  lines(simulatedPrices[,i], col=colors[i])
}

# Stock 2 - Amazon
library(quantmod)

# Define parameters for the simulation
days <- 365
trials <- 1000
lastPrice <- tail(AMZN$Close, n=1)

# Run the Monte Carlo simulation
simulatedPrices <- matrix(nrow=days, ncol=trials)
for (i in 1:trials) {
  simulatedReturns <- rnorm(days, mean=mean(amzn_return), sd=sd(amzn_return))
  simulatedPrices[,i] <- cumprod(1+simulatedReturns)*lastPrice
}

# Plot the results
colors <- rainbow(trials) # Generate a sequence of colors
plot(simulatedPrices[,1], type="l", col="blue", ylim=c(min(simulatedPrices),max(simulatedPrices)), xlab="Days", ylab="Amazon Price")
for (i in 2:trials) {
  lines(simulatedPrices[,i], col=colors[i])
}

# Stock 3 - Google
library(quantmod)

# Define parameters for the simulation
days <- 365
trials <- 1000
lastPrice <- tail(GOOG$Close, n=1)

# Run the Monte Carlo simulation
simulatedPrices <- matrix(nrow=days, ncol=trials)
for (i in 1:trials) {
  simulatedReturns <- rnorm(days, mean=mean(goog_return), sd=sd(goog_return))
  simulatedPrices[,i] <- cumprod(1+simulatedReturns)*lastPrice
}

# Plot the results
colors <- rainbow(trials) # Generate a sequence of colors
plot(simulatedPrices[,1], type="l", col="blue", ylim=c(min(simulatedPrices),max(simulatedPrices)), xlab="Days", ylab="Google Price")
for (i in 2:trials) {
  lines(simulatedPrices[,i], col=colors[i])
}

# Stock 4 - Microsoft
library(quantmod)

# Define parameters for the simulation
days <- 365
trials <- 1000
lastPrice <- tail(MSFT$Close, n=1)

# Run the Monte Carlo simulation
simulatedPrices <- matrix(nrow=days, ncol=trials)
for (i in 1:trials) {
  simulatedReturns <- rnorm(days, mean=mean(msft_return), sd=sd(msft_return))
  simulatedPrices[,i] <- cumprod(1+simulatedReturns)*lastPrice
}

# Plot the results
colors <- rainbow(trials) # Generate a sequence of colors
plot(simulatedPrices[,1], type="l", col="blue", ylim=c(min(simulatedPrices),max(simulatedPrices)), xlab="Days", ylab="Microsoft Price")
for (i in 2:trials) {
  lines(simulatedPrices[,i], col=colors[i])
}

# Stock 5 - Tesla
library(quantmod)

# Define parameters for the simulation
days <- 365
trials <- 1000
lastPrice <- tail(TSLA$Close, n=1)

# Run the Monte Carlo simulation
simulatedPrices <- matrix(nrow=days, ncol=trials)
for (i in 1:trials) {
  simulatedReturns <- rnorm(days, mean=mean(tsla_return), sd=sd(tsla_return))
  simulatedPrices[,i] <- cumprod(1+simulatedReturns)*lastPrice
}

# Plot the results
colors <- rainbow(trials) # Generate a sequence of colors
plot(simulatedPrices[,1], type="l", col="blue", ylim=c(min(simulatedPrices),max(simulatedPrices)), xlab="Days", ylab="Tesla Price")
for (i in 2:trials) {
  lines(simulatedPrices[,i], col=colors[i])
}

# Cryptocurrency 1 - Cardano
library(quantmod)

# Define parameters for the simulation
days <- 365
trials <- 1000
lastPrice <- tail(ADA$Close, n=1)

# Run the Monte Carlo simulation
simulatedPrices <- matrix(nrow=days, ncol=trials)
for (i in 1:trials) {
  simulatedReturns <- rnorm(days, mean=mean(ada_return), sd=sd(ada_return))
  simulatedPrices[,i] <- cumprod(1+simulatedReturns)*lastPrice
}

# Plot the results
colors <- rainbow(trials) # Generate a sequence of colors
plot(simulatedPrices[,1], type="l", col="blue", ylim=c(min(simulatedPrices),max(simulatedPrices)), xlab="Days", ylab="Cardano Price")
for (i in 2:trials) {
  lines(simulatedPrices[,i], col=colors[i])
}

# Cryptocurrency 2 - BNB
library(quantmod)

# Define parameters for the simulation
days <- 365
trials <- 1000
lastPrice <- tail(BNB$Close, n=1)

# Run the Monte Carlo simulation
simulatedPrices <- matrix(nrow=days, ncol=trials)
for (i in 1:trials) {
  simulatedReturns <- rnorm(days, mean=mean(bnb_return), sd=sd(bnb_return))
  simulatedPrices[,i] <- cumprod(1+simulatedReturns)*lastPrice
}

# Plot the results
colors <- rainbow(trials) # Generate a sequence of colors
plot(simulatedPrices[,1], type="l", col="blue", ylim=c(min(simulatedPrices),max(simulatedPrices)), xlab="Days", ylab="BNB Price")
for (i in 2:trials) {
  lines(simulatedPrices[,i], col=colors[i])
}

# Cryptocurrency 3 - BTC
library(quantmod)

# Define parameters for the simulation
days <- 365
trials <- 1000
lastPrice <- tail(BTC$Close, n=1)

# Run the Monte Carlo simulation
simulatedPrices <- matrix(nrow=days, ncol=trials)
for (i in 1:trials) {
  simulatedReturns <- rnorm(days, mean=mean(btc_return), sd=sd(btc_return))
  simulatedPrices[,i] <- cumprod(1+simulatedReturns)*lastPrice
}

# Plot the results
colors <- rainbow(trials) # Generate a sequence of colors
plot(simulatedPrices[,1], type="l", col="blue", ylim=c(min(simulatedPrices),max(simulatedPrices)), xlab="Days", ylab="Bitcoin Price")
for (i in 2:trials) {
  lines(simulatedPrices[,i], col=colors[i])
}

# Cryptocurrency 4 - ETH
library(quantmod)

# Define parameters for the simulation
days <- 365
trials <- 1000
lastPrice <- tail(ETH$Close, n=1)

# Run the Monte Carlo simulation
simulatedPrices <- matrix(nrow=days, ncol=trials)
for (i in 1:trials) {
  simulatedReturns <- rnorm(days, mean=mean(eth_return), sd=sd(eth_return))
  simulatedPrices[,i] <- cumprod(1+simulatedReturns)*lastPrice
}

# Plot the results
colors <- rainbow(trials) # Generate a sequence of colors
plot(simulatedPrices[,1], type="l", col="blue", ylim=c(min(simulatedPrices),max(simulatedPrices)), xlab="Days", ylab=" Ethereum Price")
for (i in 2:trials) {
  lines(simulatedPrices[,i], col=colors[i])
}

# Cryptocurrency 5 - XRP
library(quantmod)

# Define parameters for the simulation
days <- 365
trials <- 1000
lastPrice <- tail(XRP$Close, n=1)

# Run the Monte Carlo simulation
simulatedPrices <- matrix(nrow=days, ncol=trials)
for (i in 1:trials) {
  simulatedReturns <- rnorm(days, mean=mean(xrp_return), sd=sd(xrp_return))
  simulatedPrices[,i] <- cumprod(1+simulatedReturns)*lastPrice
}

# Plot the results
colors <- rainbow(trials) # Generate a sequence of colors
plot(simulatedPrices[,1], type="l", col="blue", ylim=c(min(simulatedPrices),max(simulatedPrices)), xlab="Days", ylab=" XRP Price")
for (i in 2:trials) {
  lines(simulatedPrices[,i], col=colors[i])
}











