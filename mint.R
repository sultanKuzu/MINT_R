library(readxl)
Uygulama1 <- read_excel("C:/Users/skuzu/Desktop/Kitap_bolum/Uygulama.xlsx", sheet = "BIST_30")
attach(Uygulama1)
bist_30 <- Turkey

Uygulama2 <- read_excel("C:/Users/skuzu/Desktop/Kitap_bolum/Uygulama.xlsx", sheet = "NSE_30")
attach(Uygulama2)
nse_30 <- Nigeria

Uygulama3 <- read_excel("C:/Users/skuzu/Desktop/Kitap_bolum/Uygulama.xlsx", sheet = "Mexico_34")
attach(Uygulama3)
mexico_34 <- Mexico

Uygulama4 <- read_excel("C:/Users/skuzu/Desktop/Kitap_bolum/Uygulama.xlsx", sheet = "Jakarta_45")
attach(Uygulama4)
jakarta_45 <- Indonesia

library(NonlinearTSA)
Bist_30 <-ts(bist_30, frequency=250, start=c(2012, 1))
getiriBist_30 <- log(Bist_30/lag(Bist_30,-1))
getiriBist_30


Nse_30 <- ts(nse_30, frequency=250, start=c(2012, 1))
getiriNse_30 <- log(Nse_30/lag(Nse_30,-1))
getiriNse_30

Mexico_34 <- ts(mexico_34, frequency=250, start=c(2012, 1))
getiriMexico_34 <- log(Mexico_34/lag(Mexico_34,-1))
getiriMexico_34

Jakarta_45 <- ts(jakarta_45, frequency=250, start=c(2012, 1))
getiriJakarta_45 <- log(Jakarta_45/lag(Jakarta_45,-1))
getiriJakarta_45

#Endeks Grafikleri
plot(Bist_30, xlab="" )
plot(Nse_30, xlab="")
plot(Mexico_34, xlab="")
plot(Jakarta_45, xlab="")

install.packages()
#Getiri Grafikleri
plot(getiriBist_30, xlab="")
plot(getiriNse_30, xlab="")
plot(getiriMexico_34, xlab="")
plot(getiriJakarta_45, xlab="")

#Getirilere ait tanimlayici istatistikler
stat.desc(getiriBist_30)
skewness(getiriBist_30)
kurtosis(getiriBist_30)
jarque.bera.test(getiriBist_30)

stat.desc(getiriNse_30)
skewness(getiriNse_30)
kurtosis(getiriNse_30)
jarque.bera.test(getiriNse_30)


stat.desc(getiriMexico_34)
skewness(getiriMexico_34)
kurtosis(getiriMexico_34)
jarque.bera.test(getiriMexico_34)

stat.desc(getiriJakarta_45)
skewness(getiriJakarta_45)
kurtosis(getiriJakarta_45)
jarque.bera.test(getiriJakarta_45)

#Dogrusal olmama testleri

#1. Brock, Dechert ve Scheinkman Testi
# Ho: hata terimleri bagimsiz ve es dagilima sahiptir.
library(tseries)
bds_bist_30 <- bds.test(getiriBist_30)
bds_bist_30

bds_nse_30 <- bds.test(getiriNse_30)
bds_nse_30

bds_mexico_34 <- bds.test(getiriMexico_34)
bds_mexico_34

bds_jakarta_45 <- bds.test(getiriJakarta_45)
bds_jakarta_45

# Sonu�: BDS testinde Ho hipotezinin reddedilmesi hata terimlerinin baglantili oldugunun ve baglantinin dogrusal olmadigini g�sterir.
# Burada da 4 borsa �zerinde yapilan islemlere g�re Ho reddedilmistir. Dogrusal olmama durumu s�z konusudur.

#2. Ramsey RESET testi 
#H0: Dogrusal olma durumu
#h1: Dogrusal olmama durumu
library(lmtest)
resettest(getiriBist_30~lag(getiriBist_30,-1), power = 3)         #dogrusal
resettest(getiriNse_30~lag(getiriNse_30,-1), power = 3)           #dogrusal degil
resettest(getiriMexico_34~lag(getiriMexico_34,-1), power = 3)     #dogrusal degil
resettest(getiriJakarta_45~lag(getiriJakarta_45,-1), power = 3)   #dogrusal 




#Tsay Testi
#H0: Dogrusal olma durumu
#h1: Dogrusal olmama durumu
library(TSA)
library(NonlinearTSA)
Tsay_Bist_30 <- Tsay.test(getiriBist_30)
Tsay_Bist_30
Tsay_Nse_30 <- Tsay.test(getiriNse_30)
Tsay_Nse_30
Tsay_Mexico_34 <- Tsay.test(getiriMexico_34)
Tsay_Mexico_34
Tsay_Jakarta_45 <- Tsay.test(getiriJakarta_45)
Tsay_Jakarta_45

#Tsay sonucuna g�re hepsi dogrusal olmama durumu g�stermektedir.

#Engle Testi
#ARCH etkisinin varliginin testinde kullanilmaktadir.
library(NonlinearTSA)
ARCH_Bist_30 <- ARCH.Test(getiriBist_30,1)
ARCH_Bist_30

ARCH_Nse_30 <- ARCH.Test(getiriNse_30,1)
ARCH_Nse_30

ARCH_Mexico_34 <- ARCH.Test(getiriMexico_34,1)
ARCH_Mexico_34

ARCH_Jakarta_45 <- ARCH.Test(getiriJakarta_45,1)
ARCH_Jakarta_45

# Engle testinin sonucuna g�re 4 modelde de olasilik degeri %5'den k���k oldugu i�in ARCH etkisi bulunmaktadir. 
# Bu durumda varyansta dogrusal olmayan modellerin kullanimi daha uygundur.


#LR testi
#SETAR modelinin yapisi sinanmaktadir.
library(TSA)
LR_Bist_30 <- tlrt(getiriBist_30)
LR_Bist_30    #dogrusal degil

LR_Nse_30 <- tlrt(getiriNse_30)
LR_Nse_30     #dogrusal 

LR_Mexico_34 <- tlrt(getiriMexico_34)
LR_Mexico_34    #dogrusal degil

LR_Jakarta_45 <- tlrt(getiriJakarta_45)
LR_Jakarta_45      #dogrusal degil

# Olasilik degeri %5 den b�y�k oldugundan Ho kabul edilir. Dogrusaldir.
# Olasilik degeri %5 den k���k oldugundan Ho reddedilir. Dogrusal degildir.

#Hansen Testi (KULLANMA !!!!!!)
#SETAR tipi dogrusal olmama test edilir.

#2 rejime karsi asagidaki gibidir.
library(tsDyn)
hansen_testi1 <- setarTest(Bist_30, m=1, nboot = 5, test = "1vs")
hansen_testi1   #dogrusaldir.

#3 rejime karsi asagidaki gibidir.
hansen_testi2 <- setarTest(Bist_30, m=1, nboot = 5, test = "2vs")
hansen_testi2   #dogrusaldir.

#2 rejime karsi asagidaki gibidir.
library(tsDyn)
hansen_testi1 <- setarTest(Nse_30, m=1, nboot = 5, test = "1vs")
hansen_testi1   #dogrusaldir.

#3 rejime karsi asagidaki gibidir.
hansen_testi2 <- setarTest(Nse_30, m=1, nboot = 5, test = "2vs")
hansen_testi2   #dogrusaldir.

#2 rejime karsi asagidaki gibidir.
library(tsDyn)
hansen_testi1 <- setarTest(Mexico_34, m=1, nboot = 5, test = "1vs")
hansen_testi1   #dogrusaldir.

#3 rejime karsi asagidaki gibidir.
hansen_testi2 <- setarTest(Mexico_34, m=1, nboot = 5, test = "2vs")
hansen_testi2   #dogrusaldir.

#Dogrusal olmayan Birim k�k testleri
#ESTAR modeline dayanan birim k�k testleri
#Kapetanios, Shin ve Snell -2003 
#ho: birim k�k vardir. (Duragan degil).
#h1: Dogrusal olmayan duragan s�re�tir.
library(NonlinearTSA)
KSS_Unit_Root(getiriBist_30, case = 1, lags = 12, lsm = 3)
KSS_Unit_Root(getiriBist_30, case = 2, lags = 12, lsm = 3)
KSS_Unit_Root(getiriBist_30, case = 3, lags = 12, lsm = 3)

KSS_Unit_Root(getiriNse_30, case = 1, lags = 12, lsm = 3)
KSS_Unit_Root(getiriNse_30, case = 2, lags = 12, lsm = 3)
KSS_Unit_Root(getiriNse_30, case = 3, lags = 12, lsm = 3)

KSS_Unit_Root(getiriMexico_34, case = 1, lags = 12, lsm = 3)
KSS_Unit_Root(getiriMexico_34, case = 2, lags = 12, lsm = 3)
KSS_Unit_Root(getiriMexico_34, case = 3, lags = 12, lsm = 3)

KSS_Unit_Root(getiriJakarta_45, case = 1, lags = 12, lsm = 3)
KSS_Unit_Root(getiriJakarta_45, case = 2, lags = 12, lsm = 3)
KSS_Unit_Root(getiriJakarta_45, case = 3, lags = 12, lsm = 3)

# Sollis testi
library(NonlinearTSA)
Sollis2009_Unit_Root(getiriBist_30, case = 1, lags = 12, lsm = 3)
Sollis2009_Unit_Root(getiriBist_30, case = 2, lags = 12, lsm = 3)
Sollis2009_Unit_Root(getiriBist_30, case = 3, lags = 12, lsm = 3)

Sollis2009_Unit_Root(getiriNse_30, case = 1, lags = 12, lsm = 3)
Sollis2009_Unit_Root(getiriNse_30, case = 2, lags = 12, lsm = 3)
Sollis2009_Unit_Root(getiriNse_30, case = 3, lags = 12, lsm = 3)

Sollis2009_Unit_Root(getiriMexico_34, case = 1, lags = 12, lsm = 3)
Sollis2009_Unit_Root(getiriMexico_34, case = 2, lags = 12, lsm = 3)
Sollis2009_Unit_Root(getiriMexico_34, case = 3, lags = 12, lsm = 3)

Sollis2009_Unit_Root(getiriJakarta_45, case = 1, lags = 12, lsm = 3)
Sollis2009_Unit_Root(getiriJakarta_45, case = 2, lags = 12, lsm = 3)
Sollis2009_Unit_Root(getiriJakarta_45, case = 3, lags = 12, lsm = 3)

#Kruse Test
Kruse_Unit_Root(getiriBist_30, case = 1, lags = 12, lsm = 3)
Kruse_Unit_Root(getiriBist_30, case = 2, lags = 12, lsm = 3)
Kruse_Unit_Root(getiriBist_30, case = 3, lags = 12, lsm = 3)

Kruse_Unit_Root(getiriNse_30, case = 1, lags = 12, lsm = 3)
Kruse_Unit_Root(getiriNse_30, case = 2, lags = 12, lsm = 3)
Kruse_Unit_Root(getiriNse_30, case = 3, lags = 12, lsm = 3)

Kruse_Unit_Root(getiriMexico_34, case = 1, lags = 12, lsm = 3)
Kruse_Unit_Root(getiriMexico_34, case = 2, lags = 12, lsm = 3)
Kruse_Unit_Root(getiriMexico_34, case = 3, lags = 12, lsm = 3)

Kruse_Unit_Root(getiriJakarta_45, case = 1, lags = 12, lsm = 3)
Kruse_Unit_Root(getiriJakarta_45, case = 2, lags = 12, lsm = 3)
Kruse_Unit_Root(getiriJakarta_45, case = 3, lags = 12, lsm = 3)

#ARIMA derecelerinin belirlenmesi

coeftest(arima(getiriBist_30, order = c(1,0,1))) # uygun
model1 <- arima(getiriBist_30, order = c(1,0,1))
summary(arima(getiriBist_30, order = c(1,0,1)))
coeftest(arima(getiriBist_30, order = c(1,0,0))) # uygun degil
coeftest(arima(getiriBist_30, order = c(0,0,1))) # uygun degil
coeftest(arima(getiriBist_30, order = c(2,0,1))) # uygun degil

coeftest(arima(getiriNse_30, order = c(1,0,1)))  # uygun degil
coeftest(arima(getiriNse_30, order = c(1,0,0)))  # daha uygun
summary(arima(getiriNse_30, order = c(1,0,0)))
summary(arima(getiriNse_30, order = c(0,0,1)))
coeftest(arima(getiriNse_30, order = c(0,0,1)))  # uygun
summary(arima(getiriNse_30, order = c(3,0,2)))  # en uygun model

coeftest(arima(getiriMexico_34, order = c(1,0,1))) # uygun degil
coeftest(arima(getiriMexico_34, order = c(1,0,0))) # uygun 
coeftest(arima(getiriMexico_34, order = c(0,0,1))) # uygun 
coeftest(arima(getiriMexico_34, order = c(2,0,1))) # uygun 
summary(arima(getiriMexico_34, order = c(1,0,0)))
summary(arima(getiriMexico_34, order = c(0,0,1)))
summary(arima(getiriMexico_34, order = c(2,0,1)))
coeftest(arima(getiriMexico_34, order = c(2,0,2))) # uygun degil
coeftest(arima(getiriMexico_34, order = c(3,0,1))) # uygun degil
coeftest(arima(getiriMexico_34, order = c(3,0,3))) # uygun degil
coeftest(arima(getiriMexico_34, order = c(3,0,2))) # uygun degil

coeftest(arima(getiriJakarta_45, order = c(1,0,1))) # uygun 
coeftest(arima(getiriJakarta_45, order = c(1,0,0))) # uygun 
coeftest(arima(getiriJakarta_45, order = c(0,0,1))) # uygun 
summary(arima(getiriJakarta_45, order = c(1,0,1)))
summary(arima(getiriJakarta_45, order = c(1,0,0)))
summary(arima(getiriJakarta_45, order = c(0,0,1)))
coeftest(arima(getiriJakarta_45, order = c(2,0,1))) # uygun degil
coeftest(arima(getiriJakarta_45, order = c(2,0,2))) # uygun degil
coeftest(arima(getiriJakarta_45, order = c(2,0,0))) # uygun 
coeftest(arima(getiriJakarta_45, order = c(0,0,2))) # uygun 
summary(arima(getiriJakarta_45, order = c(2,0,0))) # uygun 
summary(arima(getiriJakarta_45, order = c(0,0,2))) # uygun
coeftest(arima(getiriJakarta_45, order = c(3,0,3))) # uygun degil



#ARCH modelleri
#BIST 30 i�in
library(rugarch)
arch_model_spe1 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "sGARCH", garchOrder = c(1,0), distribution.model = "norm"))
arch_modeli1 <- ugarchfit(arch_model_spe1, data = getiriBist_30, fit.control = list(scale=T))
arch_modeli1

garch_model_spe1 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "sGARCH", garchOrder = c(1,1), distribution.model = "norm"))
garch_modeli1 <- ugarchfit(garch_model_spe1, data = getiriBist_30, fit.control = list(scale=T))
garch_modeli1

egarch_model_spe1 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "eGARCH", garchOrder = c(1,1), distribution.model = "norm"))
egarch_modeli1 <- ugarchfit(egarch_model_spe1, data = getiriBist_30, fit.control = list(scale=T))
egarch_modeli1

tgarch_model_spe1 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="TGARCH", garchOrder = c(1,1), distribution.model = "norm"))
tgarch_modeli1 <- ugarchfit(tgarch_model_spe1, data = getiriBist_30, fit.control = list(scale=T))
tgarch_modeli1


gjrgarch_model_spe1 <- ugarchspec( mean.model = list(armaOrder = c(1,1), include.mean = F), variance.model = list(model = "fGARCH", submodel="GJRGARCH", garchOrder = c(1,1), distribution.model = "norm"))
gjrgarch_modeli1 <- ugarchfit(gjrgarch_model_spe1, data = getiriBist_30, fit.control = list(scale=T))
gjrgarch_modeli1

nlgarch_model_spe1 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="NGARCH", garchOrder = c(1,1), distribution.model = "norm"))
nlgarch_modeli1 <- ugarchfit(nlgarch_model_spe1, data = getiriBist_30, fit.control = list(scale=T))
nlgarch_modeli1

nlagarch_model_spe1 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="NAGARCH", garchOrder = c(1,1), distribution.model = "norm"))
nlagarch_modeli1 <- ugarchfit(nlagarch_model_spe1, data = getiriBist_30, fit.control = list(scale=T))
nlagarch_modeli1

avgarch_model_spe1 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="AVGARCH", garchOrder = c(1,1), distribution.model = "norm"))
avgarch_modeli1 <- ugarchfit(avgarch_model_spe1, data = getiriBist_30, fit.control = list(scale=T))
avgarch_modeli1

apgarch_model_spe1 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="APARCH", garchOrder = c(1,1), distribution.model = "norm"))
apgarch_modeli1 <- ugarchfit(apgarch_model_spe1, data = getiriBist_30, fit.control = list(scale=T))
apgarch_modeli1


#NSE 30 i�in
arch_model_spe2 <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "sGARCH", garchOrder = c(1,0), distribution.model = "norm"))
arch_modeli2 <- ugarchfit(arch_model_spe2, data = getiriNse_30, fit.control = list(scale=T))
arch_modeli2

garch_model_spe2 <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "sGARCH", garchOrder = c(1,1), distribution.model = "norm"))
garch_modeli2 <- ugarchfit(garch_model_spe2, data = getiriNse_30, fit.control = list(scale=T))
garch_modeli2

egarch_model_spe2 <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "eGARCH", garchOrder = c(1,1), distribution.model = "norm"))
egarch_modeli2 <- ugarchfit(egarch_model_spe2, data = getiriNse_30, fit.control = list(scale=T))
egarch_modeli2

tgarch_model_spe2 <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "fGARCH", submodel="TGARCH", garchOrder = c(1,1), distribution.model = "norm"))
tgarch_modeli2 <- ugarchfit(tgarch_model_spe2, data = getiriNse_30, fit.control = list(scale=T))
tgarch_modeli2

gjrgarch_model_spe2 <- ugarchspec( mean.model = list(armaOrder = c(3,2), include.mean = F), variance.model = list(model = "fGARCH", submodel="GJRGARCH", garchOrder = c(1,1), distribution.model = "norm"))
gjrgarch_modeli2 <- ugarchfit(gjrgarch_model_spe2, data = getiriNse_30, fit.control = list(scale=T))
gjrgarch_modeli2

nlgarch_model_spe2 <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "fGARCH", submodel="NGARCH", garchOrder = c(1,1), distribution.model = "norm"))
nlgarch_modeli2 <- ugarchfit(nlgarch_model_spe2, data = getiriNse_30, fit.control = list(scale=T))
nlgarch_modeli2

nlagarch_model_spe2 <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "fGARCH", submodel="NAGARCH", garchOrder = c(1,1), distribution.model = "norm"))
nlagarch_modeli2 <- ugarchfit(nlagarch_model_spe2, data = getiriNse_30, fit.control = list(scale=T))
nlagarch_modeli2

avgarch_model_spe2 <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "fGARCH", submodel="AVGARCH", garchOrder = c(1,1), distribution.model = "norm"))
avgarch_modeli2 <- ugarchfit(avgarch_model_spe2, data = getiriNse_30, fit.control = list(scale=T))
avgarch_modeli2

apgarch_model_spe2 <- ugarchspec( mean.model = list(armaOrder = c(3, 2), include.mean = F), variance.model = list(model = "fGARCH", submodel="APARCH", garchOrder = c(1,1), distribution.model = "norm"))
apgarch_modeli2 <- ugarchfit(apgarch_model_spe2, data = getiriNse_30, fit.control = list(scale=T))
apgarch_modeli2

#Mexico IPC i�in
arch_model_spe3 <- ugarchspec( mean.model = list(armaOrder = c(2, 1), include.mean = F), variance.model = list(model = "sGARCH", garchOrder = c(1,0), distribution.model = "norm"))
arch_modeli3 <- ugarchfit(arch_model_spe3, data = getiriMexico_34, fit.control = list(scale=T))
arch_modeli3

garch_model_spe3 <- ugarchspec( mean.model = list(armaOrder = c(2, 1), include.mean = F), variance.model = list(model = "sGARCH", garchOrder = c(1,1), distribution.model = "norm"))
garch_modeli3 <- ugarchfit(garch_model_spe3, data = getiriMexico_34, fit.control = list(scale=T))
garch_modeli3

egarch_model_spe3 <- ugarchspec( mean.model = list(armaOrder = c(2, 1), include.mean = F), variance.model = list(model = "eGARCH", garchOrder = c(1,1), distribution.model = "norm"))
egarch_modeli3 <- ugarchfit(egarch_model_spe3, data = getiriMexico_34, fit.control = list(scale=T))
egarch_modeli3

tgarch_model_spe3 <- ugarchspec( mean.model = list(armaOrder = c(2, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="TGARCH", garchOrder = c(1,1), distribution.model = "norm"))
tgarch_modeli3 <- ugarchfit(tgarch_model_spe3, data = getiriMexico_34, fit.control = list(scale=T))
tgarch_modeli3

gjrgarch_model_spe3 <- ugarchspec( mean.model = list(armaOrder = c(2,1), include.mean = F), variance.model = list(model = "fGARCH", submodel="GJRGARCH", garchOrder = c(1,1), distribution.model = "norm"))
gjrgarch_modeli3 <- ugarchfit(gjrgarch_model_spe3, data = getiriMexico_34, fit.control = list(scale=T))
gjrgarch_modeli3

nlgarch_model_spe3 <- ugarchspec( mean.model = list(armaOrder = c(2, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="NGARCH", garchOrder = c(1,1), distribution.model = "norm"))
nlgarch_modeli3 <- ugarchfit(nlgarch_model_spe3, data = getiriMexico_34, fit.control = list(scale=T))
nlgarch_modeli3

nlagarch_model_spe3 <- ugarchspec( mean.model = list(armaOrder = c(2, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="NAGARCH", garchOrder = c(1,1), distribution.model = "norm"))
nlagarch_modeli3 <- ugarchfit(nlagarch_model_spe3, data = getiriMexico_34, fit.control = list(scale=T))
nlagarch_modeli3

avgarch_model_spe3 <- ugarchspec( mean.model = list(armaOrder = c(2, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="AVGARCH", garchOrder = c(1,1), distribution.model = "norm"))
avgarch_modeli3 <- ugarchfit(avgarch_model_spe3, data = getiriMexico_34, fit.control = list(scale=T))
avgarch_modeli3

apgarch_model_spe3 <- ugarchspec( mean.model = list(armaOrder = c(2, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="APARCH", garchOrder = c(1,1), distribution.model = "norm"))
apgarch_modeli3 <- ugarchfit(apgarch_model_spe3, data = getiriMexico_34, fit.control = list(scale=T))
apgarch_modeli3

#Jakarta 45 i�in
arch_model_spe4 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "sGARCH", garchOrder = c(1,0), distribution.model = "norm"))
arch_modeli4 <- ugarchfit(arch_model_spe4, data = getiriJakarta_45, fit.control = list(scale=T))
arch_modeli4

garch_model_spe4 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "sGARCH", garchOrder = c(1,1), distribution.model = "norm"))
garch_modeli4 <- ugarchfit(garch_model_spe4, data = getiriJakarta_45, fit.control = list(scale=T))
garch_modeli4

egarch_model_spe4 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "eGARCH", garchOrder = c(1,1), distribution.model = "norm"))
egarch_modeli4 <- ugarchfit(egarch_model_spe4, data = getiriJakarta_45, fit.control = list(scale=T))
egarch_modeli4

tgarch_model_spe4 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="TGARCH", garchOrder = c(1,1), distribution.model = "norm"))
tgarch_modeli4 <- ugarchfit(tgarch_model_spe4, data = getiriJakarta_45 , fit.control = list(scale=T))
tgarch_modeli4

gjrgarch_model_spe4 <- ugarchspec( mean.model = list(armaOrder = c(1,1), include.mean = F), variance.model = list(model = "fGARCH", submodel="GJRGARCH", garchOrder = c(1,1), distribution.model = "norm"))
gjrgarch_modeli4 <- ugarchfit(gjrgarch_model_spe4, data = getiriJakarta_45, fit.control = list(scale=T))
gjrgarch_modeli4

nlgarch_model_spe4 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="NGARCH", garchOrder = c(1,1), distribution.model = "norm"))
nlgarch_modeli4 <- ugarchfit(nlgarch_model_spe4, data = getiriJakarta_45, fit.control = list(scale=T))
nlgarch_modeli4

nlagarch_model_spe4 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="NAGARCH", garchOrder = c(1,1), distribution.model = "norm"))
nlagarch_modeli4 <- ugarchfit(nlagarch_model_spe4, data = getiriJakarta_45, fit.control = list(scale=T))
nlagarch_modeli4

avgarch_model_spe4 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="AVGARCH", garchOrder = c(1,1), distribution.model = "norm"))
avgarch_modeli4 <- ugarchfit(avgarch_model_spe4, data = getiriJakarta_45, fit.control = list(scale=T))
avgarch_modeli4

apgarch_model_spe4 <- ugarchspec( mean.model = list(armaOrder = c(1, 1), include.mean = F), variance.model = list(model = "fGARCH", submodel="APARCH", garchOrder = c(1,1), distribution.model = "norm"))
apgarch_modeli4 <- ugarchfit(apgarch_model_spe4, data = getiriJakarta_45, fit.control = list(scale=T))
apgarch_modeli4








