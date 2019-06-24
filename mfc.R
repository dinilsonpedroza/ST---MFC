################# Criando a ST da taxa por mfc ######################
getwd()
tmfc<-read.csv2("txmfc.csv", header = T)
tmfc
tmfcst<-ts(tmfc, start = c(1999,01), frequency = 12)
tmfcst
class(tmfcst)
plot(tmfcst, ylab= "tx de óbitos", xlab= "anos", main= "taxa óbitos por mfc", col="blue")
################# Criando a ST da proporção por mfc ######################
pmfc<-read.csv2("prmfc.csv", header = T)
pmfc
pmfcst<-ts(pmfc, start = c(1999,01), frequency = 12)
class(pmfcst)
plot(pmfcst, ylab= "prop. óbitos por mfc", xlab= "anos", main= "proporção óbitos por mfc", 
     col="blue")

################# Decompondo as duas séries ##############################
plot(decompose(tmfcst), col="blue")
plot(decompose(pmfcst), col="blue")

################# Sazonalidade pelo monthplot ############################

monthplot(tmfcst, ylab= "tx óbitos por mfc", 
          xlib="meses", col="blue")

monthplot(pmfcst, ylab= "prop. óbitos por mfc", 
          xlib="meses", col="blue")

################### Teste de Dickey-Fuller Aumentado ######################
install.packages("urca")
library(urca)
summary(ur.df(tmfcst, type='none', lags=0))
summary(ur.df(pmfcst, type='none', lags=0))

summary(ur.df(tmfcst, type='trend', lags=0))
summary(ur.df(pmfcst, type='trend', lags=0))

################### Funções de autocorrelação #############################
install.packages("xts")
install.packages("astsa")
library(xts)
library(astsa)
acf2(tmfcst)
acf2(pmfcst)

#################### Estimando o modelo SARIMA ############################

install.packages("forecast")
library(forecast)

satmfcst1<-arima(tmfcst, order = c(1,1,1), seasonal = c(1,1,1), method = "ML")

plot(tmfcst, ylab= "taxa de óbitos", main= "Ajuste tx x SARIMA", 
     xlab= "tempo", col= 'red')
lines(fitted(satmfcst1), col='blue')

satmfcst<-arima(tmfcst, order = c(2,1,1), seasonal = c(2,1,2), method = "ML")

plot(tmfcst, ylab= "taxa de óbitos", main= "Ajuste tx x SARIMA", 
     xlab= "tempo", col= 'red')
lines(fitted(satmfcst), col='blue')

sapmfcst1<-arima(pmfcst, order = c(1,1,1), seasonal = c(0,1,1), method = "ML")

plot(pmfcst, ylab= "proporção de óbitos", main= "Ajuste tx x SARIMA", 
     xlab= "tempo", col= 'red')
lines(fitted(sapmfcst1), col='blue')

sapmfcst<-arima(pmfcst, order = c(2,1,1), seasonal = c(2,1,1), method = "ML")

plot(pmfcst, ylab= "proporção de óbitos", main= "Ajuste tx x SARIMA", 
     xlab= "tempo", col= 'red')
lines(fitted(sapmfcst), col='blue')


###################### Testando o modelo ###################################

accuracy(satmfcst1)
accuracy(sapmfcst1)

checkresiduals(satmfcst1)
checkresiduals(sapmfcst1)

accuracy(satmfcst)
accuracy(sapmfcst)

checkresiduals(satmfcst)
checkresiduals(sapmfcst)

###################### Previsões ###########################################

forecast.satmfcst <- forecast(satmfcst, h=60, level = 0.95)
forecast.satmfcst

write.csv(forecast.satmfcst, "tabprevtxmfc", row.names = T)

forecast.sapmfcst <- forecast(satmfcst, h=60, level = 0.95)
forecast.sapmfcst

write.csv(forecast.sapmfcst, "tabprevprmfc", row.names = T)

plot(forecast(satmfcst, h=60, level=0.95), ylab="taxa de mortalidade infantil", 
     xlab = "tempo",  main = "previsão de tx de mortalidade infantil")

plot(forecast(sapmfcst, h=60, level=0.95), ylab="proporção de mortalidade infantil", 
     xlab = "tempo",  main = "previsão de prop. de mortalidade infantil")


##################### Auto-ARIMA ###########################################

autotmfcst<- auto.arima(tmfcst, max.p=5, max.q=5, max.P=2, max.Q=2,
                     seasonal = T)

autopmfcst<- auto.arima(pmfcst, max.p=5, max.q=5, max.P=2, max.Q=2,
                        seasonal = T)

###################### Gráficos de ajuste dos auto-ARIMA ####################

plot(tmfcst, col= 'red')
lines(fitted(autotmfcst), col='blue')

plot(pmfcst, col= 'red')
lines(fitted(autopmfcst), col='blue')

####################### Acurácea dos auto-ARIMA #############################

accuracy(autotmfcst)
accuracy(autopmfcst)

######################## Resíduos dos auto-ARIMA ############################

checkresiduals(autotmfcst)
checkresiduals(autopmfcst)

############# Estudando a sazonalidade das séries com QS do Arima X-13 #######

download.file("https://www.census.gov/ts/x13as/pc/x13as_V1.1_B19.zip",
              destfile = "./x13.zip")
unzip("x13.zip")
install.packages("seasonal")
library("seasonal")
checkX13()

tmfcstajustada <- seas(tmfcst)
qs(tmfcstajustada)

pmfcstajustada <- seas(pmfcst)
qs(pmfcstajustada)
