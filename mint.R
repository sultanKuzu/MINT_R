#Excel'den veri transferi yapilir.
library(readxl)
Uygulama <- read_excel("nse_30.xlsx", sheet = "NSE_30")
attach(Uygulama)

#Günlük verilerin tanimlanmasi için "xts" paketi yüklenir ve aktive edilir.
install.packages("xts")
library(xts)

#Veriler tanimlanir.
tarih_yeni <- as.Date(Uygulama$Tarih, "%d-%m-%Y")
yeni_NSE30 <- xts(Uygulama$Nigeria, tarih_yeni) 
yeni_NSE30
plot(yeni_NSE30)

#NSE 30'a ait getiri serisi hesaplanarak grafigi ?izdirilir.

getiri_NSE <- log(yeni_NSE30/lag(yeni_NSE30,1))  
plot(getiri_NSE)

#Fark alindiginda bir gozlem degeri eksilir. NA iceren ifade bir takim istatistiklerin hesaplanmasini engeller.
getiri_NSE <- na.omit(getiri_NSE)

#Getirilere ait tanimlayici istatistikler
library(pastecs)
stat.desc(getiri_NSE)
library(moments)
skewness(getiri_NSE)
kurtosis(getiri_NSE)
library(tseries)
jarque.bera.test(getiri_NSE)

#1. Brock, Dechert ve Scheinkman Testi
# Ho: hata terimleri bagimsiz ve es dagilima sahiptir.
library(tseries)
bds_nse_30 <- bds.test(getiri_NSE)
bds_nse_30

#2. Ramsey RESET testi 
#H0: Dogrusal olma durumu
#h1: Dogrusal olmama durumu
library(lmtest)
resettest(getiriNse_30~lag(getiri_NSE,-1), power = 3)

#Tsay Testi
#H0: Dogrusal olma durumu
#h1: Dogrusal olmama durumu

Tsay_Nse_30 <- Tsay.test(getiri_NSE)
Tsay_Nse_30

#Engle Testi
#ARCH etkisinin varliginin testinde kullanilmaktadir.
library(NonlinearTSA)
ARCH_Nse_30 <- ARCH.Test(getiri_NSE,1)
ARCH_Nse_30

#LR testi
#SETAR modelinin yapisi sinanmaktadir.
library(TSA)
LR_Nse_30 <- tlrt(getiri_NSE)
LR_Nse_30    

#Dogrusal olmayan Birim k?k testleri
#ESTAR modeline dayanan birim k?k testleri
#Kapetanios, Shin ve Snell -2003 
#ho: birim k?k vardir. (Duragan degil).
#h1: Dogrusal olmayan duragan s?re?tir.
library(NonlinearTSA)
KSS_Unit_Root(getiri_NSE, case = 1, lags = 12, lsm = 3)
KSS_Unit_Root(getiri_NSE, case = 2, lags = 12, lsm = 3)
KSS_Unit_Root(getiri_NSE, case = 3, lags = 12, lsm = 3)

# Sollis testi
library(NonlinearTSA)
Sollis2009_Unit_Root(getiri_NSE, case = 1, lags = 12, lsm = 3)
Sollis2009_Unit_Root(getiri_NSE, case = 2, lags = 12, lsm = 3)
Sollis2009_Unit_Root(getiri_NSE, case = 3, lags = 12, lsm = 3)

#Kruse Test
library(NonlinearTSA)
Kruse_Unit_Root(getiri_NSE, case = 1, lags = 12, lsm = 3)
Kruse_Unit_Root(getiri_NSE, case = 2, lags = 12, lsm = 3)
Kruse_Unit_Root(getiri_NSE, case = 3, lags = 12, lsm = 3)

#ARIMA derecelerinin belirlenmesi
coeftest(arima(getiri_NSE, order = c(1,0,1)))  # uygun degil
coeftest(arima(getiri_NSE, order = c(1,0,0)))  # daha uygun
summary(arima(getiri_NSE, order = c(1,0,0)))
summary(arima(getiri_NSE, order = c(0,0,1)))
coeftest(arima(getiri_NSE, order = c(0,0,1)))  # uygun
summary(arima(getiri_NSE, order = c(3,0,2)))  # en uygun model

#ARCH modelleri
library(rugarch)
#NSE 30 i?in
arch_ozellik <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "sGARCH", garchOrder = c(1,0), distribution.model = "norm"))
arch_modeli <- ugarchfit(arch_ozellik, data = getiri_NSE, fit.control = list(scale=T))
arch_modeli

garch_ozellik <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "sGARCH", garchOrder = c(1,1), distribution.model = "norm"))
garch_modeli <- ugarchfit(garch_ozellik, data = getiri_NSE, fit.control = list(scale=T))
garch_modeli

egarch_ozellik <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "eGARCH", garchOrder = c(1,1), distribution.model = "norm"))
egarch_modeli <- ugarchfit(egarch_ozellik, data = getiri_NSE, fit.control = list(scale=T))
egarch_modeli

tgarch_ozellik <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "fGARCH", submodel="TGARCH", garchOrder = c(1,1), distribution.model = "norm"))
tgarch_modeli <- ugarchfit(tgarch_ozellik, data = getiri_NSE, fit.control = list(scale=T))
tgarch_modeli

gjrgarch_ozellik <- ugarchspec( mean.model = list(armaOrder = c(3,2), include.mean = F), variance.model = list(model = "fGARCH", submodel="GJRGARCH", garchOrder = c(1,1), distribution.model = "norm"))
gjrgarch_modeli <- ugarchfit(gjrgarch_ozellik, data = getiri_NSE, fit.control = list(scale=T))
gjrgarch_modeli

nlagarch_ozellik <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "fGARCH", submodel="NAGARCH", garchOrder = c(1,1), distribution.model = "norm"))
nlagarch_modeli <- ugarchfit(nlagarch_ozellik, data = getiri_NSE, fit.control = list(scale=T))
nlagarch_modeli

avgarch_ozellik <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "fGARCH", submodel="AVGARCH", garchOrder = c(1,1), distribution.model = "norm"))
avgarch_modeli <- ugarchfit(avgarch_ozellik, data = getiri_NSE, fit.control = list(scale=T))
avgarch_modeli

apgarch_ozellik <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "fGARCH", submodel="APARCH", garchOrder = c(1,1), distribution.model = "norm"))
apgarch_modeli <- ugarchfit(apgarch_ozellik, data = getiri_NSE, fit.control = list(scale=T))
apgarch_modeli
