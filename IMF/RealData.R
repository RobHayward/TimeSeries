# Session 6 looking ar real world data
library(forecast)
da <- read.csv('Data/Module3_data/PE_Ratios.csv')
head(da)
# create a date object. 
da$date <- as.Date(da[,1], format = '%Y-%m-%d')
plot(da$date, da$pe_usa, type = 'l')
daSA <- subset(da, da$date >= '1995-09-01', select = c(date, pe_saf))
head(daSA)
plot(daSA, type = 'l', main = "South African PE ratio")
# Looks stationary
par(mfrow = c(2, 1))
print(acf(daSA$pe_saf))
pacf(daSA$pe_saf)
# Suggestion is AR4? 
eqAR4 <- arima(daSA$pe_saf, order = c(4, 0, 0))
print(eqAR4)
acf(eqAR4$residuals)
pacf(eqAR4$residuals)
eqAR6 <- arima(daSA$pe_saf, order = c(6, 0, 0))
print(eqAR6)
acf(eqAR6$residuals)
pacf(eqAR6$residuals)
eqARMA61 <- arima(daSA$pe_saf, order = c(6, 0, 1))
print(eqARMA61)
# The AR4 has the lowest AIC and SBC. 
eq <- auto.arima(daSA$pe_saf)
summary(eq)
# Now looks like ARMA(2, 1)
acf(eq$residuals)
pacf(eq$residuals)
#-----------------------------
# Now apply this to the indian data to get the answers questions

library(forecast)
da <- read.csv('Data/Module3_data/PE_Ratios.csv')
head(da)
# create a date object. 
da$date <- as.Date(da[,1], format = '%Y-%m-%d')
plot(da$date, da$pe_ind, type = 'l')
daIN <- subset(da, da$date >= '2000-01-01', select = c(date, pe_ind))
head(daIN)
plot(daIN, type = 'l', main = "Indian PE ratio")
# Looks stationary
par(mfrow = c(2, 1))
print(acf(daIN$pe_ind))
print(pacf(daIN$pe_ind))
# Suggestion is AR4? 
eqAR4 <- arima(daIN$pe_ind, order = c(4, 0, 0))
print(eqAR4)
acf(eqAR4$residuals)
pacf(eqAR4$residuals)
eqAR6 <- arima(daSIN$pe_ind, order = c(6, 0, 0))
print(eqAR6)
acf(eqAR6$residuals)
pacf(eqAR6$residuals)
eqARMA61 <- arima(daIN$pe_ind, order = c(6, 0, 1))
print(eqARMA61)
# The AR4 has the lowest AIC and SBC. 
eq <- auto.arima(daIN$pe_ind, stationary = TRUE)
summary(eq)
# Now looks like ARMA(2, 1)
acf(eq$residuals)
pacf(eq$residuals)