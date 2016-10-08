da <- read.csv('Data/italy.csv')
# find the dates for timeseries
head(da)
tail(da)
y <- ts(na.omit(da$ATTKIT), start = 1970, end = 1988, frequency = 4)
#--------------------------------------------
library(forecast)
# Look at the serial correlation of the terrorist attacks
acfz <- acf(y, plot = FALSE)
acfz$acf
# It would be good to get the Q stat and p-value.
Box.test(y, lag =4, type = 'Ljung-Box')
# the null is idenpdendnece
acfyz <- acf(y, z, plot = FALSE)
summary(acfyz)
#------------------------------
op <- par(no.readonly = TRUE)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(y, ylab = "Unemployment rate (log)")
acf(y, main = "Autocorrelations", xlab = "", ylim = c(-1, 1))
pacf(y, main = 'Partial Autocorrelations', ylab = "", ylim = c(-1, 1))
par(op)
#====================
library(urca)
data(npext)
y <- ts(na.omit(npext$unemploy), start = 1909, end = 1988, frequency = 1)
op <- par(no.readonly = TRUE)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
plot(y, ylab = "Unemployment rate (log)")
acf(y, main = "Autocorrelations", xlab = "", ylim = c(-1, 1))
pacf(y, main = 'Partial Autocorrelations', ylab = "", ylim = c(-1, 1))
par(op)
##tentative ARMA(2,0)
arma20 <- arima(y, order = c(2, 0, 0))
ll20 <- logLik(arma20)
ll20
aic20 <- arma20$aic
aic20
res20 <- residuals(arma20)
Box.test(res20, lag = 20, type = 'Ljung-Box')
shapiro.test(res20)
# Alternative speficifations
#Arma(3, 0)
arma30 <- arima(y, order = c(3, 0, 0))
ll30 <- logLik(arma30)
ll30
aic30 <- arma30$aic
aic30
lrtest <- as.numeric(2 * (1130 - 1120))
chi.pcval <- pchisq(lrtest, df = 1, lower.tail = FALSE)
chi.pcval
##ARMA(1, 1)
arma11 <- arima(y, order = c(1, 0, 1))
ll11 <- logLik(arma11)
ll11
aci11 <- arma11$aic
aci11
tsdiag(arma11)
res11 <- residuals(arma11)
Box.test(res11, lag = 20, type = 'Ljung-Box')
shapiro.test(res11)
tsdiag(arma11)
# Using auto arima
library(forecast)
auto.arima(y, max.p = 3, max.q = 3, start.p = 1, start.q = 1, ic = 'aic')

