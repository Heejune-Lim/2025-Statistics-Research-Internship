library(forecast)
library(dplyr)
library(ggplot2)
library(glue)
library(tseries)
library(rugarch)


elec_frame <- read.table("max_elec.csv", header = TRUE, sep=",", col.names = c("year","month","day","able_MW","supply_MW","max_MW","surplus_MW","surplus_percent","hour"))
elec_frame <- unique(elec_frame) #Delete duplicates
elec_frame <- elec_frame[nrow(elec_frame):1, ]
rownames(elec_frame) <- NULL #Reverse the order
plot(elec_frame$max_MW[1:731], type = "l")

ggplot() +
  geom_line(aes(x = date_vector <- seq(as.Date("2015-01-01"), as.Date("2016-12-31"), by = "day"), y = elec_frame$max_MW[1:731])) +
  xlab("Date") + ylab("전력(MW)")


#It would be beneficial to stabilize the variance by means of log-transformation.
elec_frame$log <- log(elec_frame$max_MW)
plot(elec_frame$log[1827:2192], type = "l")
#The downward spikes coincide with national holiday breaks. 
#They are fixed dates that follow a lunar calendar which has its own periodic pattern.
#It is easier to treat them separately as outliers. max_MW = exp(S_365 + S_7 + T + H + X)
holiday <- read.csv("holidays.csv")
holiday$locdate <- as.Date(as.character(holiday$response.body.items.item.locdate), format = "%Y%m%d")
elec_frame$date <- as.Date(glue("{elec_frame$year}-{elec_frame$month}-{elec_frame$day}"))
breaks <- holiday$locdate[holiday$response.body.items.item.dateName == "설날" | holiday$response.body.items.item.dateName == "추석"]
elec_frame$holiday <- match(elec_frame$date, holiday$locdate, nomatch = 0) > 0
elec_frame$breaks <- match(elec_frame$date, breaks, nomatch = 0) > 0
elec_frame$weekdays <- rep(c("Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed"), length.out = nrow(elec_frame))
elec_frame$weekdays <- factor(elec_frame$weekdays, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

#On weekends/substitute holidays after a break period (such as Lunar New Year and Chuseok)
#The ESS advises against estimating calendar-related effects other than those of special days written in the calendar themselves.
#Ergo, weekends/substitute holidays trailing a break will be considered a regular holiday, instead of a part of a break. 
elec_frame$factor <- as.factor(elec_frame$holiday + elec_frame$breaks)
#train - validation - test set separation
train <- 1:2922
valid <- 2923:3287
test <- 3288:3653
boxplot(max_MW ~ weekdays*factor, data = elec_frame[train,])

#Temperature
temp <- read.table("Temperature.csv", header = TRUE, sep=",", col.names = c("date","place","avg","min","max"))
ggplot(data = temp) + 
  geom_line(mapping = aes(x = 1:3653, y = avg)) +
  geom_line(mapping = aes(x = 1:3653, y = min), colour = 'blue') +
  geom_line(mapping = aes(x = 1:3653, y = max), colour = 'red')
quart <- read.table("주요지표_분기지표__20250422140830.csv", header = TRUE, sep=",")
year <- read.table("주요지표_연간지표__20250422135907.csv", header = TRUE, sep=",")
plot(x = 1:40,y = quart[1, 2:41], type = "l")
temp$date <- as.Date(temp$date)
elec_frame$temp <- temp$avg
plot(elec_frame$temp)
elec_frame$abs <- abs(elec_frame$temp - 15)
elec_train <- elec_frame[train,]
ggplot() +
  geom_point(mapping = aes(x = elec_train$temp[(elec_train$weekends == "Weekday")], y = elec_train$log[(elec_train$weekends == "Weekday")])) +
  geom_point(mapping = aes(x = elec_train$temp[(elec_train$weekends == "Weekend")], y = elec_train$log[(elec_train$weekends == "Weekend")]), color = 'red') +
  xlab('temperature') + ylab('log(demand)')

ggplot(elec_train, aes(temp, log)) +
  geom_point(mapping = aes(color = weekends)) +
  xlab('temperature') + ylab('log(demand)')

#This is the (seasonally adjusted) quarterly GDP growth rate. 
#To depict the actual trend of GDP, we should multiply one by one (100+growth rate)%
dev <- as.numeric(100 + quart[1, 2:41])/100
quart_trend <- rep(dev[1], 40)
for(i in 2:40){
  quart_trend[i] = quart_trend[i-1] * dev[i]
}


#Blast it, just use the actual quarterly GDP data instead 
#https://snapshot.bok.or.kr/dashboard/C1
GDPquart <- read.table("GDPquart.csv", header = TRUE, sep = ",", col.names = c("quart", "GDP", "trend"))

ggplot() + 
  geom_line(mapping = aes(x = 1:40, y = quart_trend*450.4092), color = 'blue')+
  geom_line(mapping = aes(x = 1:40, y = GDPquart$GDP[221:260]), color = 'red')
#Needless to say, both plots are near-identical. It is preferable to use raw data.
#(In fairness the quarterly GDP data is not raw data as it has been seasonally adjusted)
get_year_quarter <- function(date_input) {
  date_input <- as.Date(date_input)
  year <- format(date_input, "%Y")
  month <- as.numeric(format(date_input, "%m"))
  quarter <- ceiling(month / 3)
  return(paste0(year, "-Q", quarter))
}

elec_frame$GDP <- numeric(nrow(elec_frame))
for(i in 1:nrow(elec_frame)) {
  elec_frame$GDP[i] <- GDPquart$GDP[GDPquart$quart == get_year_quarter(elec_frame$date[i])]
}

ggplot(data = elec_frame) + 
  geom_line(mapping = aes(x = 1:3653, y = temp), colour = 'blue') +
  geom_line(mapping = aes(x = 1:3653, y = GDP), colour = 'red')
elec_frame$weekends <- as.factor(case_when(elec_frame$weekdays == "Sat" ~ "Weekend", elec_frame$weekdays == "Sun" ~ "Weekend", TRUE ~ "Weekday"))
boxplot(max_MW ~ weekdays*factor, data = elec_frame[train,])
boxplot(max_MW ~ weekends*factor, data = elec_frame[train,])
boxplot(GDP ~ weekdays*factor, data = elec_frame[train,])

plot(elec_frame$GDP)

linear1 <- lm(log ~ GDP + abs + factor + weekends, data = elec_frame[train,])
linear2 <- lm(log ~ GDP + abs + factor*weekends, data = elec_frame[train,])
linear2.5 <- lm(log ~ GDP*abs + factor + weekends, data = elec_frame[train,])
linear3 <- lm(log ~ GDP*abs + factor*weekends, data = elec_frame[train,])
linear4 <- lm(log ~ GDP*abs*factor*weekends, data = elec_frame[train,])
linear5 <- lm(log ~ abs + factor + weekends, data = elec_frame[train,])
linear6 <- lm(log ~ abs + factor*weekends, data = elec_frame[train,])
linear7 <- lm(log ~ abs + holiday + weekends, data = elec_frame[train,])
linear8 <- lm(log ~ abs + holiday*weekends, data = elec_frame[train,])

linear12 <- lm(log ~ (GDP + abs + factor + weekends)^2 - GDP:weekends, data = elec_frame[train,])

linear_test <- lm(log ~ (GDP + abs)^2, data = elec_frame[train,])

AIC(linear1, linear2, linear3, linear4, linear5, linear6, linear7, linear8)
summary(linear1)
summary(linear2)
summary(linear3)
summary(linear4)
summary(linear5)
summary(linear6)
summary(linear7)
summary(linear8)

decomp <- mstl(msts(elec_frame$max_MW[train], c(7, 365)))
base_mstl <- forecast(decomp)
plot(forecast(decomp))
plot(elec_frame$GDP[train], decomp[,2])
plot(decomp)


### holt winters
base_hw <- dshw(elec_frame$log[train], period1 = 7, period2 = 364, h = 731)
plot(base_hw)
summary(base_hw)


fc_HW = numeric(366)
for(i in test) {
  temp_obj <- dshw(elec_frame$log[1:i-1], period1 = 7, period2 = 364, model = base_hw)
  fc <- forecast(temp_obj, h = 1)
  fc_HW[i - NROW(train) - NROW(valid)] <- fc$mean[1]
}

MAPE(exp(unlist(fc_HW)), elec_frame$max_MW[test])

base_hw$mean[1:731]
###

ggplot() + 
  geom_line(mapping = aes(x = train, y = decomp[,2]/100)) +
  geom_line(mapping = aes(x = train, y = elec_frame$GDP[train]), colour = 'red')

#baseline bats
bats_bl <- bats(elec_frame$max_MW, seasonal.periods = c(7, 365))
plot(bats_bl)
bats_forecast <- forecast(bats_bl)
plot(bats_forecast)
plot(bats_forecast$mean[1:365] - elec_frame$max_MW[valid])

#MAPE function
MAPE <- function(pred, raw){
  mape <- mean(abs(pred - raw)/raw)
  return(mape)
}
MAPE(bats_forecast$mean[1:365], elec_frame$max_MW[valid])
MAPE(exp(base_hw$mean[1:365]), elec_frame$max_MW[valid])
MAPE(exp(base_mstl$mean[1:365]), elec_frame$max_MW[valid])

#Fitting
MAPE(exp(predict(linear1, elec_frame[valid,])), elec_frame$max_MW[valid])
MAPE(exp(predict(linear2, elec_frame[valid,])), elec_frame$max_MW[valid])
MAPE(exp(predict(linear12, elec_frame[valid,])), elec_frame$max_MW[valid])

MAPE(exp(predict(linear12, elec_frame[test,])), elec_frame$max_MW[test])


MAPE(exp(predict(linear3, elec_frame[valid,])), elec_frame$max_MW[valid])
MAPE(exp(predict(linear4, elec_frame[valid,])), elec_frame$max_MW[valid])
MAPE(exp(predict(linear5, elec_frame[valid,])), elec_frame$max_MW[valid])
MAPE(exp(predict(linear6, elec_frame[valid,])), elec_frame$max_MW[valid])
MAPE(exp(predict(linear7, elec_frame[valid,])), elec_frame$max_MW[valid])
MAPE(exp(predict(linear8, elec_frame[valid,])), elec_frame$max_MW[valid])

MAPE(exp(predict(linear_test, elec_frame[valid,])), elec_frame$max_MW[valid])

ggplot(data = elec_frame[valid,]) + 
  geom_line(mapping = aes(x = 1:365, y = exp(predict(linear_test, elec_frame[valid,]))), colour = 'blue') +
  geom_line(mapping = aes(x = 1:365, y = max_MW), colour = 'red')
plot(elec_frame$log[test] - predict(linear2, elec_frame[test,]))



#The all-important ARIMA/ARCH/GARCH fittings
#method 1
fit_AR1 <- auto.arima(elec_frame$log[train], stepwise = FALSE, stationary = FALSE, seasonal = FALSE, allowmean = TRUE, xreg = cbind(elec_frame$GDP[train], elec_frame$abs[train], elec_frame$factor[train], elec_frame$weekends[train]))
fc_AR1 <- numeric(365) 
summary(fit_AR1)
for(i in valid) {
  temp_obj <- Arima(elec_frame$log[1:(i-1)], xreg = cbind(elec_frame$GDP[1:(i-1)], elec_frame$abs[1:(i-1)], elec_frame$factor[1:(i-1)], elec_frame$weekends[1:(i-1)]), model = fit_AR1)
  fc <- forecast(temp_obj, h = 1, xreg = cbind(elec_frame$GDP[i], elec_frame$abs[i], elec_frame$factor[i], elec_frame$weekends[i]))
  fc_AR1[i - NROW(train)] <- fc$mean[1]
}
acf(residuals(fit_AR1))
MAPE(exp(unlist(fc_AR1)), elec_frame$max_MW[valid])
ggplot(data = elec_frame[valid,]) + 
  geom_line(mapping = aes(x = 1:365, y = exp(unlist(fc_AR1)))) +
  geom_line(mapping = aes(x = 1:365, y = max_MW), colour = 'red')

fc_year2 <- forecast(fit_AR1, h = 365, xreg = cbind(elec_frame$GDP[valid], elec_frame$abs[valid], elec_frame$factor[valid], elec_frame$weekends[valid]))
plot(fc_year2)
coef(fit_AR2)

#method 2
reg1 <- model.matrix(~ GDP + abs + factor + weekends, data = elec_frame)
fit_AR1 <- auto.arima(max.order = 10, elec_frame$log[train], xreg = reg1[train,], stepwise = FALSE, stationary = TRUE, seasonal = FALSE, allowmean = TRUE)
summary(fit_AR1)
fc_AR1 <- numeric(365) 
for(i in valid) {
  temp_obj <- Arima(elec_frame$log[1:(i-1)], xreg = reg1[1:(i-1),], model = fit_AR1)
  fc <- forecast(temp_obj, h = 1, xreg = reg1[i,,drop = FALSE])
  fc_AR1[i - NROW(train)] <- fc$mean[1]
}
MAPE(exp(unlist(fc_AR1)), elec_frame$max_MW[valid])
ggplot(data = elec_frame[valid,]) + 
  geom_line(mapping = aes(x = 1:365, y = exp(unlist(fc_AR1)))) +
  geom_line(mapping = aes(x = 1:365, y = max_MW), colour = 'red')

fit_AR9 <- auto.arima(elec_frame$log[train], stepwise = FALSE, stationary = TRUE, seasonal = FALSE, allowmean = TRUE, xreg = cbind(elec_frame$GDP[train], elec_frame$hot[train], elec_frame$cold[train], elec_frame$factor[train], elec_frame$weekends[train]))
fc_AR9 <- numeric(365) 
for(i in valid) {
  temp_obj <- Arima(elec_frame$log[1:(i-1)], xreg = cbind(elec_frame$GDP[1:(i-1)], elec_frame$hot[1:(i-1)], elec_frame$cold[1:(i-1)], elec_frame$factor[1:(i-1)], elec_frame$weekends[1:(i-1)]), model = fit_AR9)
  fc <- forecast(temp_obj, h = 1, xreg = cbind(elec_frame$GDP[i], elec_frame$hot[i], elec_frame$cold[i], elec_frame$factor[i], elec_frame$weekends[i]))
  fc_AR9[i - NROW(train)] <- fc$mean[1]
}
MAPE(exp(unlist(fc_AR9)), elec_frame$max_MW[valid])
ggplot(data = elec_frame[valid,]) + 
  geom_line(mapping = aes(x = 1:365, y = exp(unlist(fc_AR9)))) +
  geom_line(mapping = aes(x = 1:365, y = max_MW), colour = 'red')

#(5/9)No need to feed residuals of lm
reg2 <- model.matrix(~ GDP + abs + factor*weekends, data = elec_frame)
fit_AR2 <- auto.arima(max.order = 10, elec_frame$log[train], xreg = reg2[train,], stepwise = FALSE, stationary = TRUE, seasonal = FALSE, allowmean = TRUE)
fc_AR2 <- numeric(365)
for(i in valid) {
  temp_obj <- Arima(elec_frame$log[1:(i-1)], xreg = reg2[1:(i-1),], model = fit_AR2)
  fc <- forecast(temp_obj, h = 1, xreg = reg2[i,,drop = FALSE])
  fc_AR2[i - NROW(train)] <- fc$mean[1]
}
plot(unlist(fc_AR2), type = "l")
MAPE(exp(unlist(fc_AR2)), elec_frame$max_MW[valid])
ggplot(data = elec_frame[valid,]) + 
  geom_line(mapping = aes(x = 1:365, y = exp(unlist(fc_AR2)))) +
  geom_line(mapping = aes(x = 1:365, y = max_MW), colour = 'red')

fc_year2 <- forecast(temp_obj, h = 365, xreg = reg2[valid,,drop = FALSE])
plot(fc_year2)
coef(fit_AR2)

#(5/9)No need to feed residuals of lm
reg2.5 <- model.matrix(~ (GDP*abs) + factor + weekends, data = elec_frame)
fit_AR2.5 <- auto.arima(elec_frame$log[train], xreg = reg2.5[train,], stepwise = FALSE, stationary = TRUE, seasonal = FALSE, allowmean = TRUE)
fc_AR2.5 <- numeric(365)
for(i in valid) {
  temp_obj <- Arima(elec_frame$log[1:(i-1)], xreg = reg2.5[1:(i-1),], model = fit_AR2.5)
  fc <- forecast(temp_obj, h = 1, xreg = reg2.5[i,,drop = FALSE])
  fc_AR2.5[i - NROW(train)] <- fc$mean[1]
}
plot(unlist(fc_AR2.5), type = "l")
MAPE(exp(unlist(fc_AR2.5)), elec_frame$max_MW[valid])
ggplot(data = elec_frame[valid,]) + 
  geom_line(mapping = aes(x = 1:365, y = exp(unlist(fc_AR2.5)))) +
  geom_line(mapping = aes(x = 1:365, y = max_MW), colour = 'red')


#reg12
reg12 <- model.matrix(~ (GDP + abs + factor + weekends)^2 - GDP:weekends, data = elec_frame)
fit_AR12 <- auto.arima(elec_frame$log[train], xreg = reg12[train,], stepwise = FALSE, stationary = TRUE, seasonal = FALSE, allowmean = TRUE)
fc_AR12 <- numeric(365) 
for(i in valid) {
  temp_obj <- Arima(elec_frame$log[1:(i-1)], xreg = reg12[1:(i-1),], model = fit_AR12)
  fc <- forecast(temp_obj, h = 1, xreg = reg12[i,,drop = FALSE])
  fc_AR12[i - NROW(train)] <- fc$mean[1]
}
fit_AR12
MAPE(exp(unlist(fc_AR12)), elec_frame$max_MW[valid])
ggplot(data = elec_frame[valid,]) + 
  geom_line(mapping = aes(x = 1:365, y = exp(unlist(fc_AR12)))) +
  geom_line(mapping = aes(x = 1:365, y = max_MW), colour = 'red')

fc_SAR12_test <- numeric(366)
for(i in test) {
  temp_obj <- Arima(elec_frame$log[1:(i-1)], xreg = reg12[1:(i-1),], model = fit_SAR12)
  fc <- forecast(temp_obj, h = 1, xreg = reg12[i,,drop = FALSE])
  fc_SAR12_test[i - NROW(train) - NROW(valid)] <- fc$mean[1]
}

MAPE(exp(unlist(fc_SAR12_test)), elec_frame$max_MW[test])

overfit_AR12 <- arima(elec_frame$log[train], order = c(4, 0, 1) , xreg = reg12[train,2:14])
overfit_AR12_2 <- arima(elec_frame$log[train], order = c(3, 0, 2) , xreg = reg12[train,2:14])
overfit_AR12
overfit_AR12_2

#AR-ARCH and AR-GARCH models with external regressors

#
#garch1

for(i in 1:10) {garch1 <- ugarchspec(
  mean.model = list(armaOrder = c(i, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = reg1[, -1], archex = FALSE)
)
garchfit1 <- ugarchfit(garch1, elec_frame$log, out.sample = 731)
print(infocriteria(garchfit1))

}
garch1 <- ugarchspec(
  mean.model = list(armaOrder = c(9, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = reg1[, -1], archex = FALSE)
)
#p=9, AIC = -4.232781
garchfit1 <- ugarchfit(garch1, elec_frame$log, out.sample = 731)
garchfor1 <- t(fitted(
  ugarchforecast(garchfit1, n.ahead = 1, n.roll = 364, external.forecasts = list(mregfor = reg1[valid,-1]))
))

MAPE(exp(garchfor1), elec_frame$max_MW[valid]) #2.198%
ggplot(data = elec_frame[valid,]) + 
  geom_line(mapping = aes(x = 1:365, y = exp(garchfor1))) +
  geom_line(mapping = aes(x = 1:365, y = max_MW), colour = 'red')

#garch2

for(i in 1:10) {garch2 <- ugarchspec(
  mean.model = list(armaOrder = c(i, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = reg2[, -1], archex = FALSE)
)
garchfit2 <- ugarchfit(garch2, elec_frame$log, out.sample = 731)
print(infocriteria(garchfit2))

}
#p=8 AIC = -4.401347
garch2 <- ugarchspec(
  mean.model = list(armaOrder = c(8, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = reg2[, -1], archex = FALSE)
)
garchfit2 <- ugarchfit(garch2, elec_frame$log, out.sample = 731)
garchfor2 <- t(fitted(
  ugarchforecast(garchfit2, n.ahead = 1, n.roll = 364, external.forecasts = list(mregfor = reg2[valid,-1]))
))
MAPE(exp(garchfor2), elec_frame$max_MW[valid]) #2.056%
ggplot(data = elec_frame[valid,]) + 
  geom_line(mapping = aes(x = 1:365, y = exp(garchfor2))) +
  geom_line(mapping = aes(x = 1:365, y = max_MW), colour = 'red')

#garch12

for(i in 1:10) {garch12 <- ugarchspec(
  mean.model = list(armaOrder = c(i, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = reg12[, -1], archex = FALSE)
)
garchfit12 <- ugarchfit(garch12, elec_frame$log, out.sample = 731)
print(infocriteria(garchfit12))

}
#p= 10 AIC = -4.4625
garchfit12 <- ugarchspec(
  mean.model = list(armaOrder = c(10, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = reg12[, -1], archex = FALSE)
)
garchfit12 <- ugarchfit(garch12, elec_frame$log, out.sample = 731)
garchfor12 <- t(fitted(
  ugarchforecast(garchfit12, n.ahead = 1, n.roll = 730, external.forecasts = list(mregfor = reg12[2923:3653,-1]))
))
MAPE(exp(garchfor12[366:731]), elec_frame$max_MW[test]) #valid 1.969%, test 2.106%
ggplot(data = elec_frame[valid,]) + 
  geom_line(mapping = aes(x = 1:365, y = exp(garchfor12))) +
  geom_line(mapping = aes(x = 1:365, y = max_MW), colour = 'red')

garchfit12 <- ugarchspec(
  mean.model = list(armaOrder = c(10, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = reg12[, -1], archex = FALSE)
)
garchfit12 <- ugarchfit(garch12, elec_frame$log, out.sample = 731)
garchfor12 <- t(fitted(
  ugarchforecast(garchfit12, n.ahead = 1, n.roll = 364, external.forecasts = list(mregfor = reg12[valid,-1]))
))
MAPE(exp(garchfor12), elec_frame$max_MW[valid])

#AR(8)-GARCH(1, 1)
reg_base <- model.matrix(~ abs + factor + weekends, data = elec_frame)
garch_base <- ugarchspec(
  mean.model = list(armaOrder = c(8, 0), include.mean = TRUE, archm = FALSE, archpow = 1, arfima = FALSE, external.regressors = reg_base[, -1], archex = FALSE)
)

fit_base <- ugarchfit(garch_base, elec_frame$log, out.sample = 731)
garchfor_base <- t(fitted(
  ugarchforecast(fit_base, n.ahead = 1, n.roll = 730, external.forecasts = list(mregfor = reg_base[2923:3653,-1]))
))
MAPE(exp(garchfor_base[366:731]), elec_frame$max_MW[test])

#SARIMA1
acf(elec_frame$log[train])
pacf(elec_frame$log[train])
fit_SAR1 <- auto.arima(
  ts(elec_frame$log[train], frequency = 7), max.order = 10, xreg = reg1[train,], seasonal = TRUE, stepwise = FALSE, stationary = TRUE, allowmean = TRUE
)
summary(fit_SAR1)
acf(residuals(fit_SAR1))
pacf(residuals(fit_SAR1))
fc_SAR1 <- numeric(365) 
for(i in valid) {
  temp_obj <- Arima(elec_frame$log[1:(i-1)], xreg = reg1[1:(i-1),], model = fit_SAR1)
  fc <- forecast(temp_obj, h = 1, xreg = reg1[i,,drop = FALSE])
  fc_SAR1[i - NROW(train)] <- fc$mean[1]
}
MAPE(exp(unlist(fc_SAR1)), elec_frame$max_MW[valid])
ggplot(data = elec_frame[valid,]) + 
  geom_line(mapping = aes(x = 1:365, y = exp(unlist(fc_SAR1)))) +
  geom_line(mapping = aes(x = 1:365, y = max_MW), colour = 'red')

#SARIMA2
fit_SAR2 <- auto.arima(
  ts(elec_frame$log[train], frequency = 7), max.order = 10, xreg = reg2[train,], seasonal = TRUE, stepwise = FALSE, stationary = TRUE, allowmean = TRUE
)
summary(fit_SAR2)
acf(residuals(fit_SAR2))
pacf(residuals(fit_SAR2))
fc_SAR2 <- numeric(365) 
for(i in valid) {
  temp_obj <- Arima(elec_frame$log[1:(i-1)], xreg = reg2[1:(i-1),], model = fit_SAR2)
  fc <- forecast(temp_obj, h = 1, xreg = reg2[i,,drop = FALSE])
  fc_SAR2[i - NROW(train)] <- fc$mean[1]
}
MAPE(exp(unlist(fc_SAR2)), elec_frame$max_MW[valid])
ggplot(data = elec_frame[valid,]) + 
  geom_line(mapping = aes(x = 1:365, y = exp(unlist(fc_SAR2)))) +
  geom_line(mapping = aes(x = 1:365, y = max_MW), colour = 'red')

test_SAR2 <- numeric(366) 
for(i in test) {
  temp_obj <- Arima(elec_frame$log[1:(i-1)], xreg = reg2[1:(i-1),], model = fit_SAR2)
  fc <- forecast(temp_obj, h = 1, xreg = reg2[i,,drop = FALSE])
  test_SAR2[i - NROW(train) - NROW(valid)] <- fc$mean[1]
}
MAPE(exp(unlist(test_SAR2)), elec_frame$max_MW[test])
ggplot(data = elec_frame[test,]) + 
  geom_line(mapping = aes(x = 1:366, y = exp(unlist(test_SAR2)))) +
  geom_line(mapping = aes(x = 1:366, y = max_MW), colour = 'red')



#SARIMA12
fit_SAR12 <- auto.arima(
  ts(elec_frame$log[train], frequency = 7), max.order = 10, xreg = reg12[train,], seasonal = TRUE, stepwise = FALSE, stationary = TRUE, allowmean = TRUE
)
fit_SAR12

overfit_SAR12 <- arima(ts(elec_frame$log[train], frequency = 7), order = c(5, 0, 3), seasonal = c(2, 0, 0), xreg = reg12[train,2:14])
overfit_SAR12

overfit_SAR12_2 <- arima(ts(elec_frame$log[train], frequency = 7), order = c(4, 0, 4), seasonal = c(2, 0, 0), xreg = reg12[train,2:14])
overfit_SAR12_2


fc_SAR12 <- numeric(365) 
for(i in valid) {
  temp_obj <- Arima(elec_frame$log[1:(i-1)], xreg = reg12[1:(i-1),], model = fit_SAR12)
  fc <- forecast(temp_obj, h = 1, xreg = reg12[i,,drop = FALSE])
  fc_SAR12[i - NROW(train)] <- fc$mean[1]
}
MAPE(exp(unlist(fc_SAR12)), elec_frame$max_MW[valid])

acf(residuals(fit_SAR12))
pacf(residuals(fit_SAR12))

test_SAR12 <- numeric(366) 
for(i in test) {
  temp_obj <- Arima(elec_frame$log[1:(i-1)], xreg = reg12[1:(i-1),], model = fit_SAR12)
  fc <- forecast(temp_obj, h = 1, xreg = reg12[i,,drop = FALSE])
  test_SAR12[i - NROW(train) - NROW(valid)] <- fc$mean[1]
}
MAPE(exp(unlist(test_SAR12)), elec_frame$max_MW[test])
ggplot(data = elec_frame[test,]) + 
  geom_line(mapping = aes(x = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), y = exp(unlist(test_SAR12))), colour = 'red') +
  geom_line(mapping = aes(x = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), y = max_MW)) +
  xlab("Date") + ylab("전력(MW)")



resid_lm <- residuals(linear12)
acf(resid_lm)
Box.test(resid_lm, lag = 7, "Ljung-Box")
resid_ar <- residuals(fit_AR12)
acf(resid_ar)
Box.test(resid_ar, lag = 7, "Ljung-Box")
resid_sar <- ts(residuals(fit_SAR12), frequency = 1)
acf(resid_sar)
adf.test(resid_sar)
Box.test(resid_sar, lag = 7, "Ljung-Box")
resid_garch <- as.vector(residuals(garchfit12))
acf(resid_garch)
Box.test(resid_garch, lag = 7, "Ljung-Box")

