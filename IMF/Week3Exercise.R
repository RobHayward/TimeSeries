# Week 3 exercise
da <- read.csv('Data/Module3_data/Identifying_ARMA.csv')
head(da)
round(mean(da$et, na.rm = TRUE), 3) 
sd(da$et, na.rm = TRUE)
#------------------
da$yar = rep(NA, 500)
da$yar[1] <- 0
for(i in 2:500){
  da$yar[i] = 3.0 + 0.55 * da$yar[i -1] + da$et[i]
}
round(da$yar[5], 3)
#-----------------------
da$yma = rep(NA, 500)
da$yma[1] <- 0
for(i in 2:500){
  da$yma[i] = -2.5 + da$et[i] + 0.7 * da$et[i - 1]
}
signif(da$yma[5], 3)
#===========================
acf(da$yar, na.action = na.pass)
pacf(da$yar, na.action = na.pass)
acf(da$yma, na.action = na.pass)
pacf(da$yma, na.action = na.pass)
#================================
da$yarma <- rep(NA, 500)
da$yarma[1] <- 0
for(i in 2:500){
  da$yarma[i] <- 0.5 + 0.55 * da$yarma[i - 1] + da$et[i] + 
    0.7 * da$et[i - 1] 
}
signif(da$yarma[5], 4)
#----------------------------
acf(da$et, plot = FALSE, na.action = na.pass)[1]
acf(da$yar, plot = FALSE, na.action = na.pass)[1]
acf(da$yma, plot = FALSE, na.action = na.pass)[1]
acf(da$yarma, plot = FALSE, na.action = na.pass)[1]
#==================================
 0.55*0.55 
#====================================
acf(da$et, plot = FALSE, na.action = na.pass)[2]
acf(da$yar, plot = FALSE, na.action = na.pass)[2]
acf(da$yma, plot = FALSE, na.action = na.pass)[2]
acf(da$yarma, plot = FALSE, na.action = na.pass)[2]
#========================
pacf(da$yar, plot = FALSE, na.action = na.pss)[1]
pacf(da$yar, plot = FALSE, na.action = na.pss)[2]
#-----------------------------------------------
# from http://stats.stackexchange.com/questions/25889/
#lagging-over-a-grouped-time-series
da$yarl[2:length(da$yar)] <- da$yar[1:length(da$yar) - 1]
head(da)
eq1 <- lm(da$yar ~ da$yarl, na.action = na.omit)
summary(eq1)
#--------------------------------------------
da$yarl2[3:length(da$yar)] <- da$yar[2:length(da$yar) - 2]
head(da)
eq2 <- lm(da$yar ~ da$yarl + da$yarl2)
summary(eq2)
#-------------------------------
par(mfrow = c(2, 1))
plot(da$yar, type = 'l', main = "Autogressive")
plot(da$yma, type = 'l', main = "Moving Average")
