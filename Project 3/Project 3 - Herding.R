library (xts)
library (zoo)
library (PerformanceAnalytics)
require (sandwich)
require(lmtest)

## read the file to load the data
close.prices = read.csv("Hourly_CryptoCompare_Index_BTC_USDT.csv", header = TRUE) #importing the data
close.prices$Index = as.POSIXct(close.prices$Index,format="%d/%m/%Y %H", tz = "") # converting the first column into date format

close.prices.xts <- xts(close.prices[,-1], order.by=close.prices[,1]) # convert the object into xts object (time series object)
close.prices.zoo  <- as.zoo(close.prices.xts) # convert the time series object into zoo object
head(close.prices)
## calculate the return

return = Return.calculate( close.prices.xts , method = "log") # automatically calculate return

library(pastecs)
descriptive.stat.return = stat.desc(return) # descriptive statistics

# a function to create CSAD and Rm
exchange.herd = function(return) 
{
  n=ncol(return)
  Rm = rowMeans(return)
  temp_dif =abs(return-Rm)
  temp_sum = rowSums(temp_dif)
  CSAD = temp_sum / ncol(return)
  CSAD = cbind (CSAD, Rm)
  return (CSAD)
}


f = exchange.herd(return) # calling the function "exchange.herd" that calculates CSAD and Rm
head (f) # show the first 6 rows

CSAD.df = fortify.zoo(f) # converting f into a dataframe (to simplify further calculations)
CSAD.df$Rm2 = CSAD.df$Rm^2 # calculating Rm^2
CSAD.df = CSAD.df[-c(1),] # removing the first row with NAs
head (CSAD.df) # show the first 6 rows
tail (CSAD.df) # show the last 6 rows

# Create a dummy variable for up days
CSAD.df$Dup = ifelse(CSAD.df$Rm > 0, 1, 0)

# Create interaction terms
CSAD.df$DupRm = CSAD.df$Dup * abs(CSAD.df$Rm)
CSAD.df$DupRm2 = CSAD.df$Dup * CSAD.df$Rm2
CSAD.df$DownRm = (1 - CSAD.df$Dup) * CSAD.df$Rm
CSAD.df$DownRm2 = (1 - CSAD.df$Dup) * CSAD.df$Rm2

# Estimate the model
model = lm(CSAD ~ DupRm + DownRm + DupRm2 + DownRm2, data = CSAD.df)

library(ggplot2)

# Extract the coefficient estimates and standard errors
coefficients <- coef(model)[-1]  # Exclude the intercept
se <- summary(model)$coefficients[-1, "Std. Error"]  # Exclude the intercept

# Create a data frame for plotting
result_df <- data.frame(Coefficient = names(coefficients),
                        Estimate = coefficients,
                        SE = se)

# Plot the coefficients with error bars
ggplot(result_df, aes(x = Coefficient, y = Estimate, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE)) +
  geom_point() +
  geom_errorbar(width = 0.2) +
  coord_flip() +
  labs(x = "Coefficient", y = "Estimate") +
  ggtitle("Herding in Finance: Coefficient Estimates") +
  theme_minimal()


# reassign my columns as Y and Xs to look better in the regression model
#y = CSAD.df$CSAD  # reassign my columns as Y and Xs to look better in the regression model
#x1 = abs (CSAD.df$Rm)
#x2 = CSAD.df$Rm2

y = CSAD.df$CSAD
x1 = CSAD.df$DupRm
x2 = CSAD.df$DownRm
x3 = CSAD.df$DupRm2
x4 = CSAD.df$DownRm2

#Linear model
linearMod <- lm(y~x1+x2+x3+x4)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

#Newey-West Heteroscedasticity and Autocorrelation consistent (HAC) estimators
coeftest(linearMod,vcov=NeweyWest(linearMod,verbose=T))

# estimate TV Linear Regression
require (tvReg)
tvlm.fit = tvLM(y~x1+x2+x3+x4, bw = NULL  ) #bw=20
head (tvlm.fit$coefficients)
plot(tvlm.fit$coefficients[,1], type="l")
plot(tvlm.fit$coefficients[,2], type="l")
plot(tvlm.fit$coefficients[,3], type="l")
plot(tvlm.fit$coefficients[,4], type="l")

# Bayesian models
library (brms)
hourly = cbind(y, x1, x2, x3, x4)
model = brm(formula = y ~ x1+x2+x3+x4, 
            data    = hourly,
            seed    = 123)
summary(model)

# Markow regime-switching model
library (MSwM)

nstates <- 2 # a number of states
msEuro = msmFit(linearMod, k = nstates, sw = rep(TRUE, 4)) # estimation; linearMod is an object from a linear estimation
summary(msEuro) #show the 
plotProb(msEuro ,which=1)

#Quantile regression
library (quantreg)
taus<-seq(from = .1, to = .9, by = .1) 
coef0 <- rq( y ~ x1+x2, tau=taus)
summary (coef0)







