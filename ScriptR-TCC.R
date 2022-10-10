#versao final#

####################
#PACOTES UTILIZADOS#
####################

pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp","ggplot","data.table","datasets",
             "forecast","fpp2","tseries","patchwork", "DataCombine", "TTR")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



pacotes <- c("dplyr", "datasets","prophet", "forecast","fpp2","tseries","patchwork", "DataCombine", "TTR")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#####################
#IMPORTANDO OS DADOS#
#####################

library(readxl)


IPCA_02_2022 <- read_excel("C:/Users/li_de/OneDrive/TCC/IPCA mensal .xls")
IGPM_06_22 <- read_excel("C:/Users/li_de/OneDrive/TCC/IGP -M 06.2022 .xls")
Salario_min_REAL_02_22 <- read_excel("C:/Users/li_de/OneDrive/TCC/Salario min real 02.22.xls")
Salario_min_VIGENTE_02_22 <- read_excel("C:/Users/li_de/OneDrive/TCC/Salario min vigente 02.22.xls")
Tx_desocupacao_2012<- read_excel("C:/Users/li_de/OneDrive/TCC/Taxa de desocupacao .xlsx")
Rendimento_med_2012 <-read_excel("C:/Users/li_de/OneDrive/TCC/Rendimento medio .xlsx")
PIB_1996<- read_excel("C:/Users/li_de/OneDrive/TCC/PIB_taxa.xlsx")

#################################################
#DATA WRANGLING R - preparaÃ§Ã£o do banco de dados# 
#################################################

library("tidyverse")

IPCA_2007_2022<-slice(IPCA_02_2022,-(1:324))
view(IPCA_2007_2022)
rm(IPCA_02_2022)

PIB_2012<-slice(PIB_1996,-(1:44))
view(PIB_2012)
rm(PIB_1996)

Salario_min_REAL_2007_2022<-slice(Salario_min_REAL_02_22,-(1:798))
view(Salario_min_REAL_2007_2022)
rm(Salario_min_REAL_02_22)

Salario_min_VIGENTE_2007_2022<-slice(Salario_min_VIGENTE_02_22,-(1:798))
view(Salario_min_VIGENTE_2007_2022)
rm(Salario_min_VIGENTE_02_22)

IGPM_2007_2022<-slice(IGPM_06_22, -(1:210))
view(IGPM_2007_2022)
rm(IGPM_06_22)


##################################################
#SALVANDO OS DADOS EM FORMATO DE S?RIES TEMPORAIS#
##################################################

igpm_ts<-ts(IGPM_2007_2022, start=c(2007,1),end = c(2022,1), frequency = 12)
ipca_ts<-ts(IPCA_2007_2022, start=c(2007,1),end = c(2022,6), frequency = 12)
PIB_TS<-ts(PIB_2012,start = c(2007,1),end = c(2022,1), frequency = 4)
sal_min_REAL_ts<- ts(Salario_min_REAL_2007_2022,start=c(2007,1),end = c(2022,1), 
                     frequency = 12)
sal_min_VIG_ts<-ts(Salario_min_VIGENTE_2007_2022,start=c(2007,1),end = c(2022,1), 
                   frequency = 12)
tx_desoc_ts<-ts(Tx_desocupacao_2012,start=c(2012,1),end = c(2022,5), 
                frequency = 4) 
rend_med_ts<-ts(Rendimento_med_2012, start = c(2012,1), end = c(2022,6), 
                frequency = 4)

plot(igpm_ts)
plot(ipca_ts)
plot(PIB_TS)
plot(sal_min_REAL_ts)
plot(sal_min_VIG_ts)
plot(tx_desoc_ts)
plot(rend_med_ts)

###########################
#GRAFICOS SERIES TEMPORAIS#
###########################

#IGPM
igpm_ts<- igpm_ts[,-1]
plot(igpm_ts,main='IGPM %AM',
     ylab='Indice', xlab = 'ANO',
     bty='l',col='red',lty=1)+axis(1, at = c(2007:2022))+
axis(2, at = c(0:10))+ grid(col='darkgrey',
     lwd=2)

#PIB
PIB_TS <- PIB_TS[,-1]
plot(PIB_TS,main='PIB brasileiro',
     ylab='Indice', xlab = 'Ano',
     bty='l',col='blue',lty=1)+axis(1, at = c(2007:2022))+
axis(2, at = c(0:10))+grid(col='darkgrey',
     lwd=2)

#IPCA
ipca_ts<-ipca_ts[,-1]
plot(ipca_ts,main='IPCA',
     ylab='Indice', xlab = 'Ano',
     bty='l',col='purple',lty=1)+axis(1, at = c(2007:2022))
+axis(2, at = c(0:10))+grid(col='darkgrey',
     lwd=2)

#SALARIO MIN REAL 
sal_min_REAL_ts<-sal_min_REAL_ts[,-1]
plot(sal_min_REAL_ts,main='Salario min REAL',
     ylab='Indice', xlab = 'Ano',
     bty='l',col='darkred',lty=1)+axis(1, at = c(2007:2022))
+axis(2, at = c(0:10))+grid(col='darkgrey',
     lwd=2)

#SALARIO MIN VIGENTE 
sal_min_VIG_ts<-sal_min_VIG_ts[,-1]
plot(sal_min_VIG_ts,main='Salario min VIGENTE',
     ylab='Indice', xlab = 'Ano',
     bty='l',col='darkBLUE',lty=1)+axis(1, at = c(2007:2022))
+axis(2, at = c(0:10))+grid(col='darkgrey',
                            lwd=2)
#TAXA DE DESOCUPACAO 
tx_desoc_ts<-tx_desoc_ts[,-1]
plot(tx_desoc_ts,main='Taxa de DesocupaÃ§Ã£o',
     ylab='Indice', xlab = 'Ano',
     bty='l',col='darkgreen',lty=1)+axis(1, at = c(2012:2022))
+axis(2, at = c(0:10))+grid(col='darkgrey',
                            lwd=2)

#RENDIMENTO MEDIO 
rend_med_ts<-rend_med_ts[,-1]
plot(rend_med_ts,main='Rendimento MÃ©dio',
     ylab='Indice', xlab = 'Ano',
     bty='l',col='#009999',lty=1)+axis(1, at = c(2012:2022))
+axis(2, at = c(0:10))+grid(col='darkgrey',
                            lwd=2)


##################################
#ANALISE SAZONALIDADE NOS ?NDICES#
##################################
# irei rodar a regress?o e, para isso, devemos criar os dummies sazonais

#PIB#
Q<-ordered(cycle(PIB_TS))
pib_reg<-lm(PIB_TS~Q)
summary(pib_reg)

#IPCA
W<-ordered(cycle(ipca_ts))
ipca_reg<-lm(ipca_ts~W)
summary(ipca_reg)

#IGPM
Z<-ordered(cycle(igpm_ts))
igpm_reg<- lm(igpm_ts~Z)
summary(igpm_reg)

#SAL?RIO M?N REAL 
N<-ordered(cycle(sal_min_REAL_ts))
sal_min_real_reg<-lm(sal_min_REAL_ts~N)
summary(sal_min_real_reg)

#SAL?RIO M?N VIGENTE 
Z<-ordered(cycle(sal_min_VIG_ts))
sal_min_vig_reg<- lm(sal_min_VIG_ts~Z)
summary(sal_min_vig_reg)

#TAXA DESOCUPA??O
U<-ordered(cycle(tx_desoc_ts))
tx_desoc_reg<-lm(tx_desoc_ts~U)
summary(tx_desoc_reg)

#RENDIMENTO MEDIO 
P<-ordered(cycle(rend_med_ts))
rend_med_reg<-lm(rend_med_ts~P)
summary(rend_med_reg)

#######################
#DESSAZONALIZAR O IPCA#
######################
ipca.des<-ts(resid(ipca_reg),
             start=c(2007,1),end = c(2022,6), frequency = 12)

ipca.hat<-ts(fitted(ipca_reg), start=c(2007,1),end = c(2022,6), frequency = 12)

par(mfrow=c(1,2))
plot(ipca.des)
plot(ipca.hat)

############################################################################
#ao subtrair o IPCA^ da nossa sÃ©rie original, tambÃ©m subtraÃ­mos sua mÃ©dia. # 
#para normalizar a sÃ©rie, precisamos adicionar sua mÃ©dia.                  #
############################################################################

ipca.desn<-ipca.des+mean(fitted(ipca_reg))

################################################
#GrÃ¡fico IPCA original VS. IPCA dessazonalizado#
################################################

par(mfrow=c(1,1))
plot(ipca_ts,
     main='',
     xlab='ANO', ylab='',
     col='blue',
     type="line")+axis(1, at = c(2007:2022))
+axis(2, at = c(0:10))

par(new=TRUE)
plot(ipca.desn,
     axes=F, ann=F,
     col='red',
     lty=2)
legend('topleft',
       c('IPCA', 'IPCA dessazonalizado'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')
grid(col='darkgrey')

########################################################################
#TRANSFORMANDO A SÉRIE EM ESTACIONARIA ATRAVÉS DA REMOÇÃO DE TENDENCIAS#
########################################################################

#PIB#
ndiffs(PIB_TS)
PIB_TS_DIFF<- diff(PIB_TS,1)
PIB_TS <- PIB_TS[,-1]
par(mfrow=c(1,1))
plot(PIB_TS,main='PIB brasileiro',
     ylab='Indice', xlab = 'Ano',
     bty='l',col='blue',lty=1)+axis(1, at = c(2007:2022))+
  axis(2, at = c(0:10))+grid(col='darkgrey',
                             lwd=2)
PIB_TS_DIFF<- PIB_TS_DIFF[,-1]
par(new=TRUE)
plot(PIB_TS_DIFF,
     axes=F, ann=F,
     col='red',
     lty=2)
legend('topright',
       c('PIB', 'PIB estacionário'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')


#SALARIO MINIMO REAL#
ndiffs(sal_min_REAL_ts)
SAL_REAL_TS_DIFF<- diff(sal_min_REAL_ts,1)
sal_min_REAL_ts <- sal_min_REAL_ts[,-1]
par(mfrow=c(1,1))
plot(sal_min_REAL_ts,main='Salario min REAL',
     ylab='Indice', xlab = 'Ano',
     bty='l',col='blue',lty=1)+axis(1, at = c(2007:2022))
+axis(2, at = c(0:10))+grid(col='darkgrey',
                            lwd=2)
SAL_REAL_TS_DIFF<- SAL_REAL_TS_DIFF[,-1]
par(new=TRUE)
plot(SAL_REAL_TS_DIFF,
     axes=F, ann=F,
     col='red',
     lty=2)
legend('topleft',
       c('Salário Mín Real', 'Salário Mín Real estacionário'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')

#SALARIO MIN VIGENTE#
ndiffs(sal_min_VIG_ts)
SAL_VIG_TS_DIFF<-diff(sal_min_VIG_ts,2)
sal_min_VIG_ts <- sal_min_VIG_ts[,-1]
plot(sal_min_VIG_ts,main='Salario min VIGENTE',
     ylab='Indice', xlab = 'Ano',
     bty='l',col='BLUE',lty=1)+axis(1, at = c(2007:2022))
+axis(2, at = c(0:10))+grid(col='darkgrey',
                            lwd=2)
SAL_VIG_TS_DIFF <- SAL_VIG_TS_DIFF[,-1]
par(new=TRUE)
plot(SAL_VIG_TS_DIFF,
     axes=F, ann=F,
     col='red',
     lty=2)
legend('topleft',
       c('Salário Mín Vigente', 'Salário Mín vigente estacionário'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')

#TX DE DESOCUPACAO#
ndiffs(tx_desoc_ts)
tx_desoc_TS_DIFF<-diff(tx_desoc_ts,1)
tx_desoc_ts<-tx_desoc_ts[,-1]
plot(tx_desoc_ts,main='Taxa de Desocupação',
     ylab='Indice', xlab = 'Ano',
     bty='l',col='blue',lty=1)+axis(1, at = c(2012:2022))
+axis(2, at = c(0:10))+grid(col='darkgrey',
                            lwd=2)
tx_desoc_TS_DIFF<- tx_desoc_TS_DIFF[,-1]
par(new=TRUE)
plot(tx_desoc_TS_DIFF,
     axes=F, ann=F,
     col='red',
     lty=2)
legend('topleft',
       c('Tx de desocupação', 'Tx desocupação estacionário'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')

#RENDIMENTO MEDIO#
ndiffs(rend_med_ts)
rend_med_TS_DIFF<-diff(rend_med_ts,1)
rend_med_ts<-rend_med_ts[,-1]
plot(rend_med_ts,main='Rendimento Médio',
     ylab='Indice', xlab = 'Ano',
     bty='l',col='blue',lty=1)+axis(1, at = c(2012:2022))
+axis(2, at = c(0:10))+grid(col='darkgrey',
                            lwd=2)
rend_med_TS_DIFF<- rend_med_TS_DIFF[,-1]
par(new=TRUE)
plot(rend_med_TS_DIFF,
     axes=F, ann=F,
     col='red',
     lty=2)
legend('topleft',
       c('Rendimento médio', 'Rendimento médio estacionário'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')

#IGPM#
ndiffs(igpm_ts)
igpm_TS_DIFF<-diff(igpm_ts,1)
igpm_ts<- igpm_ts[,-1]
plot(igpm_ts,main='IGPM %AM',
     ylab='Indice', xlab = 'ANO',
     bty='l',col='blue',lty=1)+axis(1, at = c(2007:2022))+
  axis(2, at = c(0:10))+ grid(col='darkgrey',
                              lwd=2)
igpm_TS_DIFF<- igpm_TS_DIFF[,-1]
par(new=TRUE)
plot(igpm_TS_DIFF,
     axes=F, ann=F,
     col='red',
     lty=2)
legend('topleft',
       c('IGPM', 'IGPM estacionário'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')

#IPCA#
ndiffs(ipca.desn)
IPCA_DESN_DIFF<- diff(ipca.desn,1)
ipca_ts<-ipca_ts[,-1]
plot(ipca_ts,main='IPCA',
     ylab='Indice', xlab = 'Ano',
     bty='l',col='BLUE',lty=1)+axis(1, at = c(2007:2022))
+axis(2, at = c(0:10))+grid(col='darkgrey',
                            lwd=2)
IPCA_DESN_DIFF<- IPCA_DESN_DIFF[,-1]
par(new=TRUE)
plot(IPCA_DESN_DIFF,
     axes=F, ann=F,
     col='red',
     lty=2)
legend('topleft',
       c('IPCA', 'IPCA estacionário'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')

#####################################
          #ARIMA#                   #
#####################################

########IGPM########

#CORRELOGRAMA E CORRELOGRAMA PARCIAL#

acf(igpm_TS_DIFF, lag.max = 20) # plotando um correlograma

pacf(igpm_TS_DIFF, lag.max=20)             # plotando um correlograma parcial

#p=3
#q=4

#plotando ARIMA#

igpm_ARIMA<- arima(igpm_TS_DIFF, order=c(3,0,4))
igpm_ARIMA

#previsão até 2026#
igpm_forecast_2026  <- forecast(igpm_ARIMA, h=48)
igpm_forecast_2026
plot(igpm_forecast_2026)+axis(1, at = c(2020:2026))
+grid(col='lightgrey',lwd=2)
                              
  
######IPCA########
acf(IPCA_DESN_DIFF,lag.max = 20)
pacf(IPCA_DESN_DIFF, lag.max=20)          

#p= 3
#q= 4

ipca_ARIMA<-arima(IPCA_DESN_DIFF, order=c(3,0,4))
ipca_ARIMA

ipca_forecast_2026 <- forecast(ipca_ARIMA, h=48)
plot(igpm_forecast_2026)+axis(1, at = c(2020:2026))
+grid(col='lightgrey',lwd=2)
      

######PIB########
acf(PIB_TS_DIFF,lag.max = 20)
pacf(PIB_TS_DIFF, lag.max=20)          

#p= 4
#q= 2

PIB_ARIMA<-arima(PIB_TS_DIFF, order=c(4,0,2))
PIB_ARIMA

PIB_forecast_2026 <- forecast(PIB_ARIMA, h=19)
PIB_forecast_2026
plot(PIB_forecast_2026)+axis(1, at = c(2020:2026))
+grid(col='lightgrey',lwd=2)

######RENDIMENTO MÉDIO########
acf(rend_med_TS_DIFF,lag.max = 20)
pacf(rend_med_TS_DIFF, lag.max=20)          

#p= 2
#q= 1

RENDA_ARIMA<-arima(rend_med_TS_DIFF, order=c(2,0,1))
RENDA_ARIMA

RENDA_forecast_2026 <- forecast(RENDA_ARIMA, h=14)
RENDA_forecast_2026
plot(RENDA_forecast_2026)+axis(1, at = c(2020:2026))
+grid(col='lightgrey',lwd=2)

######TAXA DESOCUPACAO########
acf(tx_desoc_TS_DIFF,lag.max = 20)
pacf(tx_desoc_TS_DIFF, lag.max=20)          

#p= 4
#q= 2

TX_DESOCUP_ARIMA<-arima(tx_desoc_TS_DIFF, order=c(4,0,2))
TX_DESOCUP_ARIMA

TX_DESOCUP_forecast_2026 <- forecast(TX_DESOCUP_ARIMA, h=15)
TX_DESOCUP_forecast_2026
plot(TX_DESOCUP_forecast_2026)+axis(1, at = c(2020:2026))
+grid(col='lightgrey',lwd=2)

######SALÁRIO MIN VIGENTE #########
acf(SAL_VIG_TS_DIFF,lag.max = 20)
pacf(SAL_VIG_TS_DIFF, lag.max=20)          

#p= 5
#q= 9

SAL_VIG_ARIMA<-arima(SAL_VIG_TS_DIFF, order=c(5,0,9))
SAL_VIG_ARIMA

SAL_VIG_forecast_2026 <- forecast(SAL_VIG_ARIMA, h=48)
SAL_VIG_forecast_2026
plot(SAL_VIG_forecast_2026)+axis(1, at = c(2020:2026))
+grid(col='lightgrey',lwd=2)


######SALÁRIO MIN REAL #########
acf(SAL_REAL_TS_DIFF,lag.max = 20)
pacf(SAL_REAL_TS_DIFF, lag.max=20)          

#p= 2
#q= 3

SAL_REAL_ARIMA<-arima(SAL_REAL_TS_DIFF, order=c(2,0,3))
SAL_REAL_ARIMA

SAL_REAL_forecast_2026 <- forecast(SAL_REAL_ARIMA, h=48)
SAL_REAL_forecast_2026
plot(SAL_REAL_forecast_2026)+axis(1, at = c(2020:2026))
+grid(col='lightgrey',lwd=2)



##################################
#ANÁLISE DOS ÍNDICES POR GOVERNO#
#################################

#SEGUNDO GOVERNO LULA - 2007 ATÉ 2011#

#PIB
plot(PIB_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "PIB",las = 1, 
     xlim = c(2007,2011), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd=2)+grid(col='darkgrey',
                                                    lwd=2)+
points(PIB_TS_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('PIB'),
       col=c('darkblue'), lty=1:2,
       bty='n')


#salário Mín Vigente e Real 

par(mfrow=c(1,1))
plot(SAL_VIG_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "Salário Mín VIGENTE x REAL",las = 1, 
     xlim = c(2007,2011), ylim = c(-20, 120), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd= 2)

points(SAL_VIG_TS_DIFF, 
       pch = 23,
       cex = 1.3)

par(new=TRUE)
plot(SAL_REAL_TS_DIFF, xlab = "Ano", ylab = "índice",  main = " ",las = 1, 
     xlim = c(2007,2011), ylim = c(-20, 120), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkred',lty=1, lwd =2)+grid(col='darkgrey',
                                                           lwd=1)+
  points(SAL_REAL_TS_DIFF, 
         pch = 23,
         cex = 1.3)


legend('topleft',
       c('VIGENTE', 'REAL'),
       col=c('darkblue', 'darkred'), lty=1:2,
       bty='n')



# Rendimento Médio - dados coletados a partir de 2012

# Taxa de desocupação - Dados coletados à partir de 2012

# IPCA VS IGPM

par(mfrow=c(1,1))
plot(IPCA_DESN_DIFF, xlab = "Ano", ylab = "índice",  main = "IPCA X IGPM",las = 1, 
     xlim = c(2007,2011), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd = 2)
points(IPCA_DESN_DIFF, 
       pch = 23,
       cex = 1.3)


par(new=TRUE)
plot(igpm_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "",las = 1, 
     xlim = c(2007,2011), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkred',lty=1, lwd =2)+grid(col='darkgrey',
                                                           lwd=1)+
  points(igpm_TS_DIFF, 
         pch = 23,
         cex = 1.3)
legend('topleft',
       c('IPCA', 'IGPM'),
       col=c('darkblue', 'darkred'), lty=1:2,
       bty='n')


###############GOVERNO DILMA - 2011 - 2016 ATÉ AGOSTO ###################

#PIB
plot(PIB_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "PIB",las = 1, 
     xlim = c(2011,2016), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd=2)+grid(col='darkgrey',
                                                    lwd=2)+
  points(PIB_TS_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('PIB'),
       col=c('darkblue'), lty=1:2,
       bty='n')


#salário Mín Vigente e Real 

par(mfrow=c(1,1))
plot(SAL_VIG_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "Salário Mín VIGENTE x REAL",las = 1, 
     xlim = c(2011,2016), ylim = c(-20, 150), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd= 2)

points(SAL_VIG_TS_DIFF, 
       pch = 23,
       cex = 1.3)

par(new=TRUE)
plot(SAL_REAL_TS_DIFF, xlab = "Ano", ylab = "índice",  main = " ",las = 1, 
     xlim = c(2011,2016), ylim = c(-20, 150), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkred',lty=1, lwd =2)+grid(col='darkgrey',
                                                           lwd=1)+
  points(SAL_REAL_TS_DIFF, 
         pch = 23,
         cex = 1.3)


legend('topleft',
       c('VIGENTE', 'REAL'),
       col=c('darkblue', 'darkred'), lty=1:2,
       bty='n')

# Rendimento Médio#

plot(rend_med_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "Rendimento Médio",las = 1, 
     xlim = c(2011,2016), ylim = c(-12, 24), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd=2)+grid(col='darkgrey',
                                                    lwd=2)+
  points(rend_med_TS_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('Rendimento Médio'),
       col=c('darkblue'), lty=1:2,
       bty='n')


# Taxa de desocupação

plot(tx_desoc_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "Taxa Desocupação",las = 1, 
     xlim = c(2011,2016), ylim = c(-2, 2), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd=2)+grid(col='darkgrey',
                                                    lwd=2)+
  points(tx_desoc_TS_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('Taxa Desocupação'),
       col=c('darkblue'), lty=1:2,
       bty='n')


#IPCA 

plot(IPCA_DESN_DIFF, xlab = "Ano", ylab = "índice",  main = "IPCA",las = 1, 
     xlim = c(2011,2016), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1)+grid(col='darkgrey',
                                                    lwd=2)+
  points(IPCA_DESN_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('IPCA'),
       col=c('darkblue'), lty=1:2,
       bty='n')

#IGPM

plot(igpm_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "IGPM",las = 1, 
     xlim = c(2011,2016), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1)+grid(col='darkgrey',
                                                    lwd=2)+
  points(igpm_TS_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('IGPM'),
       col=c('darkblue'), lty=1:2,
       bty='n')

# IPCA VS IGPM#

par(mfrow=c(1,1))
plot(IPCA_DESN_DIFF, xlab = "Ano", ylab = "índice",  main = "IPCA X IGPM",las = 1, 
     xlim = c(2011,2016), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd = 2)
points(IPCA_DESN_DIFF, 
       pch = 23,
       cex = 1.3)


par(new=TRUE)
plot(igpm_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "",las = 1, 
     xlim = c(2011,2016), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkred',lty=1, lwd =2)+grid(col='darkgrey',
                                                           lwd=1)+
  points(igpm_TS_DIFF, 
         pch = 23,
         cex = 1.3)
legend('topleft',
       c('IPCA', 'IGPM'),
       col=c('darkblue', 'darkred'), lty=1:2,
       bty='n')



##########GOVERNO TEMER - AGOSTO 2016 ATÉ 2018 ###############

#PIB
plot(PIB_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "PIB",las = 1, 
     xlim = c(2016,2018), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd=2)+grid(col='darkgrey',
                                                    lwd=2)+
  points(PIB_TS_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('PIB'),
       col=c('darkblue'), lty=1:2,
       bty='n')


#salário Mín Vigente e Real 

par(mfrow=c(1,1))
plot(SAL_VIG_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "Salário Mín VIGENTE x REAL",las = 1, 
     xlim = c(2016,2018), ylim = c(-20, 75), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd= 2)

points(SAL_VIG_TS_DIFF, 
       pch = 23,
       cex = 1.3)

par(new=TRUE)
plot(SAL_REAL_TS_DIFF, xlab = "Ano", ylab = "índice",  main = " ",las = 1, 
     xlim = c(2016,2018), ylim = c(-20, 75), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkred',lty=1, lwd =2)+grid(col='darkgrey',
                                                           lwd=1)+
  points(SAL_REAL_TS_DIFF, 
         pch = 23,
         cex = 1.3)


legend('topleft',
       c('VIGENTE', 'REAL'),
       col=c('darkblue', 'darkred'), lty=1:2,
       bty='n')


# Rendimento Médio#

plot(rend_med_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "Rendimento Médio",las = 1, 
     xlim = c(2016,2018), ylim = c(-30, 35), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd =2 )+grid(col='darkgrey',
                                                    lwd=2)+
  points(rend_med_TS_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('Rendimento Médio'),
       col=c('darkblue'), lty=1:2,
       bty='n')


# Taxa de desocupação

plot(tx_desoc_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "Taxa Desocupação",las = 1, 
     xlim = c(2016,2018), ylim = c(-2, 2), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd=2)+grid(col='darkgrey',
                                                    lwd=2)+
  points(tx_desoc_TS_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('Taxa Desocupação'),
       col=c('darkblue'), lty=1:2,
       bty='n')


#IPCA 

plot(IPCA_DESN_DIFF, xlab = "Ano", ylab = "índice",  main = "IPCA",las = 1, 
     xlim = c(2016,2018), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1)+grid(col='darkgrey',
                                                    lwd=2)+
  points(IPCA_DESN_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('IPCA'),
       col=c('darkblue'), lty=1:2,
       bty='n')

#IGPM

plot(igpm_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "IGPM",las = 1, 
     xlim = c(2016,2018), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1)+grid(col='darkgrey',
                                                    lwd=2)+
  points(igpm_TS_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('IGPM'),
       col=c('darkblue'), lty=1:2,
       bty='n')

# IPCA VS IGPM#

par(mfrow=c(1,1))
plot(IPCA_DESN_DIFF, xlab = "Ano", ylab = "índice",  main = "IPCA X IGPM",las = 1, 
     xlim = c(2016,2018), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd = 2)
points(IPCA_DESN_DIFF, 
       pch = 23,
       cex = 1.3)



par(new=TRUE)
plot(igpm_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "",las = 1, 
     xlim = c(2016,2018), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkred',lty=1, lwd =2)+grid(col='darkgrey',
                                                           lwd=1)+
  points(igpm_TS_DIFF, 
         pch = 23,
         cex = 1.3)
legend('topleft',
       c('IPCA', 'IGPM'),
       col=c('darkblue', 'darkred'), lty=1:2,
       bty='n')


##########GOVERNO BOLSONARO 2019 - dias atuais##############

#PIB
plot(PIB_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "PIB",las = 1, 
     xlim = c(2019,2022), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd= 2)+grid(col='darkgrey',
                                                    lwd=2)+
  points(PIB_TS_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('PIB'),
       col=c('darkblue'), lty=1:2,
       bty='n')


#salário Mín Vigente e Real 


par(mfrow=c(1,1))
plot(SAL_VIG_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "Salário Mín VIGENTE x REAL",las = 1, 
     xlim = c(2019,2022), ylim = c(-20, 75), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd= 2)

points(SAL_VIG_TS_DIFF, 
       pch = 23,
       cex = 1.3)

par(new=TRUE)
plot(SAL_REAL_TS_DIFF, xlab = "Ano", ylab = "índice",  main = " ",las = 1, 
     xlim = c(2019,2022), ylim = c(-20, 75), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkred',lty=1, lwd =2)+grid(col='darkgrey',
                                                           lwd=1)+
  points(SAL_REAL_TS_DIFF, 
         pch = 23,
         cex = 1.3)


legend('topleft',
       c('VIGENTE', 'REAL'),
       col=c('darkblue', 'darkred'), lty=1:2,
       bty='n')


# Rendimento Médio#

plot(rend_med_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "Rendimento Médio",las = 1, 
     xlim = c(2019,2022), ylim = c(-30, 35), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd = 2)+grid(col='darkgrey',
                                                    lwd=2)+
  points(rend_med_TS_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('Rendimento Médio'),
       col=c('darkblue'), lty=1:2,
       bty='n')


# Taxa de desocupação

plot(tx_desoc_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "Taxa Desocupação",las = 1, 
     xlim = c(2019,2022), ylim = c(-2, 2), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd = 2)+grid(col='darkgrey',
                                                    lwd=2)+
  points(tx_desoc_TS_DIFF, 
         pch = 23,
         cex = 1.3)

legend('topleft',
       c('Taxa Desocupação'),
       col=c('darkblue'), lty=1:2,
       bty='n')


# IPCA VS IGPM#

par(mfrow=c(1,1))
plot(IPCA_DESN_DIFF, xlab = "Ano", ylab = "índice",  main = "IPCA X IGPM",las = 1, 
     xlim = c(2019,2022), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkblue',lty=1, lwd = 2)
points(IPCA_DESN_DIFF, 
       pch = 23,
       cex = 1.3)

points(SAL_VIG_TS_DIFF, 
       pch = 23,
       cex = 1.3)

par(new=TRUE)
plot(igpm_TS_DIFF, xlab = "Ano", ylab = "índice",  main = "",las = 1, 
     xlim = c(2019,2022), ylim = c(-4, 7), xaxs = "i", yaxs = "i",
     cex.axis=1, bty='l',col='darkred',lty=1, lwd =2)+grid(col='darkgrey',
                                                           lwd=1)+
  points(igpm_TS_DIFF, 
         pch = 23,
         cex = 1.3)
legend('topleft',
       c('IPCA', 'IGPM'),
       col=c('darkblue', 'darkred'), lty=1:2,
       bty='n')


#FIM#