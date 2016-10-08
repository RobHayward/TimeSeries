# Comparing models
library(forecast)
da <- read.csv('Data/Module3_data/PE_Ratios.csv')
head(da)
fit <- Arima(da$pe_ind, order = c(1, 0, 0))
summary(fit)
Box.test(fit$residuals, fitdf = 1, lag = 1)
fit2 <- Arima(da$pe_ind, order = c(1,0, 0), seasonal = c(4, 8))
summary(fit2)
#  Need find a way fit a AR(1, 8) model. 
