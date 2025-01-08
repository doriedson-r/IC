getwd()
setwd("C:/Users/suelio.moura/OneDrive")

# Carregar os pacotes necessários
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(readxl)
library(dplyr)

#Carregando pacotes de ST
install.packages("broom")
library(broom)
install.packages("tidyverse")
library(tidyverse)
install.packages("tibble")
library(tibble)
install.packages("forecast")
library(forecast)
install.packages("fpp2")
library(fpp2)
install.packages("pwt8")
library(pwt8)

### MODELAGEM DE PREDIÇÃO ###
#----------- 1.	Cobertura de Atendimento (ME + EPP) -----------#
# Carregar os dados
# Ler as bases de dados
indicador_me_epp <- read_excel("indicador_me_epp_metas_mobilizadoras.xlsx")%>% 
  na.omit()
attach(indicador_me_epp)
dim(indicador_me_epp)
View(indicador_me_epp)
str(indicador_me_epp)

#Algumas anaálises descritivas

# Tabela de frequencia absoluta
# relativa e acumulada das variáveis contínuas. 

desc_continuas <- function(x) {
  x <- na.omit(x)
  summary <- summary(x)  
  media <- mean(x)
  sd <- sd(x)
  cv <- (sd(x)/mean(x))*100
  length <- length(x)
  IClow <- (mean(x)-qnorm(0.975)*sqrt(var(x)/length(x)))
  IChigh <- (mean(x)+qnorm(0.975)*sqrt(var(x)/length(x)))
  return(list(Sumario=summary,
              Teste=c(N=length,Media=media,sd=sd,cv=cv,
                      IC_low=IClow,IC_high=IChigh)))
}

desc_continuas(indicador_me_epp$CT_ATENDIMENTO_CLIENTE_ME_EPP)
summary(indicador_me_epp$CT_ATENDIMENTO_CLIENTE_ME_EPP)

#Transformando um Data Frame em uma Série Temporal ME+EPP E MEI
EPPME_AC12=indicador_me_epp$CT_ATENDIMENTO_CLIENTE_ME_EPP %>%
  zoo::rollsum(12)# Calcula a soma acumulada em 12 meses
ts_indicador_me_epp_acumulado<-ts(data = EPPME_AC12,
                                  start = c(2014,12), freq=12) #dispõe em série temporal


class(ts_indicador_me_epp_acumulado)
start(ts_indicador_me_epp_acumulado)
end(ts_indicador_me_epp_acumulado)
frequency(ts_indicador_me_epp_acumulado)
View(ts_indicador_me_epp_acumulado)
plot(ts_indicador_me_epp_acumulado)

desc_continuas(ts_indicador_me_epp_acumulado)
summary(ts_indicador_me_epp_acumulado)

#Testando de autocorrelação, Teste de LJUNG-BOX
#H_0: os resíduos são i.i.d.
#H_1: Os residuos não são i.i.d. (ou seja os dados são autocorrelacionados)
teste_LJUNG_BOX = Box.test(ts_indicador_me_epp_acumulado, type="Ljung-Box")
print(teste_LJUNG_BOX)

#Decomposição da série temporal "ts_indicador_me_epp_acumulado"
correlograma_ts_indicador_me_epp <- autoplot(ts_indicador_me_epp_acumulado,
                                             main ="Série da cobertura de atendimento 
                                             para Microempresas (ME) e Empresas de Pequeno
                                             Porte (EPP), Dezembro/2014 a Agosto/2024",
                                             ylab="Cobertura de Atendimento (ME + EPP)",
                                             xlab="Tempo")
png("correlograma_ts_indicador_me_epp_acumulado.png", width = 800, height = 480)
print(correlograma_ts_indicador_me_epp)
dev.off()

existencia_sazonalidade <- ggseasonplot(ts_indicador_me_epp_acumulado,
                                        main = "Analisando a existência de
                                        sazonalidade da série temporal cobertura
                                        de atendimento para Microempresas (ME) e
                                        Empresas de Pequeno Porte (EPP)")# Analisando a existência de sazonalidade 
monthplot(ts_indicador_me_epp_acumulado, col.base=2, labels = month.abb)

#Avaliando quais os componentes estão presentes na ST: Ciclo, Tendência ou sazonalidade

decomp_ts_indicador_me_epp <-decompose(ts_indicador_me_epp_acumulado)
decomposicao_ts_indicador_me_epp <- autoplot(decomp_ts_indicador_me_epp,
                                             main="Decomposição da série
                                             temporal da Cobertura de atendimento
                                             para (ME) mais (EPP) de dezembro de
                                             2014 a agosto de 2024", xlab="Tempo")
print(decomposicao_ts_indicador_me_epp)

#SUAVIZAÇÃO EXPONENCIAL SAZONAL DE HOLT-WINTERS (HW), ST com sazonalidade
#Apresenta sazonalidade assim para realizarmos previsão utiliza-se HW

#Método Aditivo
met_aditivo_ts_indicador_me_epp<- hw(ts_indicador_me_epp_acumulado, seasonal = "additive", h = 16)
autoplot(met_aditivo_ts_indicador_me_epp,
         main="Cobertura de atendimento para (ME) mais (EPP) de dezembro de 2014
         a agosto de 2024 com previsão para os próximos 16 meses") + 
  ylab("Cobertura de atendimento") 
summary(met_aditivo_ts_indicador_me_epp)
accuracy(met_aditivo_ts_indicador_me_epp)

# Calcular as previsões usando o modelo
previsoes <- forecast(met_aditivo_ts_indicador_me_epp)
# Calcular a precisão das previsões
metricas_precisao <- accuracy(previsoes)
# Extrair o EQM
eqm <- metricas_precisao[,"RMSE"]^2
# Mostrar o EQM
print(eqm)

#Método Multiplicativo
met_multiplicativo_ts_indicador_me_epp <- hw(ts_indicador_me_epp_acumulado, seasonal = "multiplicative", h = 16)
autoplot(met_multiplicativo_ts_indicador_me_epp, main="Cobertura de atendimento para (ME) mais (EPP) de dezembro de 2014 a agosto de 2024 com previsão para os próximos 12 meses")+ylab("Cobertura de atendimento") 
summary(met_multiplicativo_ts_indicador_me_epp)
accuracy(met_multiplicativo_ts_indicador_me_epp)

#Sendo assim, agora precisaremos o teste para avaliarmos a existência ou não da estacionariedade
# Dickey Fuller Aumentado (ADF) - Teste da raíz unitária
#Teste ADF para raiz unitária avaliando somente a Raiz unitária + constante (no R = drift)
library(urca)
require(urca)
adf.drift <- ur.df(y = ts_indicador_me_epp_acumulado, type = c("drift"), lags = 24, selectlags = "AIC")
acf(adf.drift@res, lag.max = 36)

summary(adf.drift)
summary(adf.drift)@teststat #estatística do teste
summary(adf.drift)@cval #valores críticos

# Ao analisar a estatística de teste (-0.6805) notamos que seu valor é SUPERIOR ao valor crítico associado ao nível de confiança de 95% (-2.88);

#Dessa forma, conclui-se que a série temporal é NÃO estacionária (rejeição da hipótese nula).

#Diferenciação da ST IBC_Br
adf.drift_2_diff <- diff(ts_indicador_me_epp_acumulado)
lbdiff <- Box.test(adf.drift_2_diff,type="Ljung-Box")
print(lbdiff)

#CONCLUSÃO: p-valor (0.002323) é menor que 5%. Indício que a série é não estacionária, precisaremos, por exemplo, aplicarmos mais um diferença. O teste de Box-Ljung nos mostra ainda que os dados são autocorrelacionados

ndiffs(adf.drift_2_diff)#números de diferenças

acf(adf.drift_2_diff, lag.max = 36)

#Como a diferenciação é igual a ZERO, isso significa que mesmo aplicando outra diferença, a não estacionariedade não será resolvida. Assim, precisaremos de outros modelos para resolvermos a não estacionaridade (AR, MA, ARMA, ARIMA) e inclusive avaliar a existência da sazonalidade na série em estudo (SARIMA)

#Modelando a série temporal
#Os processos 𝐴𝑅(𝑝), 𝑀𝐴(𝑞), 𝐴𝑅𝑀𝐴(𝑝𝑑) e 𝐴𝑅I𝑀𝐴(𝑝d𝑑)

# AR(p): processo autorregressivo de ordem p, PACF
# MA(q): processo de médias móveis de ordem q, na ACF

#Identificação
layout(1:2)
pacf(adf.drift_2_diff, lag.max = 36, main = "Função de Autocorelação Parcial para o indicador de cobertura de atendimento \n para (ME) mais (EPP) de dezembro de 2014 a agosto de 2024") # avalia o parâmetro associado ao AR(p), no gráfico contamos quantas informações estão fora do limite de significância
acf(adf.drift_2_diff, lag.max = 36, main = "Função de Autocorelação para o indicador para o indicador de cobertura de atendimento \n para (ME) mais (EPP) de dezembro de 2014 a agosto de 2024") # avalia o parâmetro associado ao MA(q), no gráfico contamos a quantidade de parãmetros depois do zero


#Estimação
#modelo  SARIMA(1,1,1)(1,1,1)12
#A diferença é que temos de diferenciar a série com respeito a delta e delta_12 (no nosso caso, estamos considerando só séries mensaais com períod s=12), a fim de produzir estacionariedade. Com isto, obtos valores para d e D que, na maioria das vezes, assumem valores no máximo iguais a 2. 
library("forecast")
fit.air <- Arima(ts_indicador_me_epp_acumulado, order = c(3,1,3), seasonal = c(3,1,3), method = "ML", lambda = 0) #CSS (Conditional Sum of Squares)
summary(fit.air)
checkresiduals(fit.air)
AIC(fit.air)
BIC(fit.air)

t.test <- function(modelo_arima){
  # estatística t
  coef <- modelo_arima$coef
  se <- sqrt(diag(modelo_arima$var.coef))
  t <- abs(coef/se)
  # Teste t
  ok <- t > qt(0.975, length(modelo_arima$x) - sum(modelo_arima$arma[c(1,2,3,4,5,6)]))
  resul <- data.frame(Coef = coef, sd = se, t = t, rej_H0 = ok)
  return(resul)
}

t.test(fit.air)


fit.air01 <- Arima(ts_indicador_me_epp_acumulado, order = c(1,1,0), seasonal = c(0,1,0), method = "ML", lambda = 0) #CSS (Conditional Sum of Squares)
summary(fit.air01)
checkresiduals(fit.air01)
t.test(fit.air01)
AIC(fit.air01)
BIC(fit.air01)

#Realizando a análise de Diagnóstico
diag <- tsdiag(fit.air01, gof.lag = 20)

require(forecast)
prev01 <- forecast(object = fit.air01, h=16, level = 0.95) #h=16, corresponde um ano e 4 meses de predição
autoplot(prev01, main = "Modelo Forecasts SARIMA(1,1,0)(0,1,0)12 para o indicador de cobertura de atendimento para (ME) mais (EPP) de set/2024 a dez/2025", ylab = "Cobertura de atendimento para (ME) mais (EPP)", xlab = "Tempo")
accuracy(fit.air01) #ou podemos chamar pelo objeto armazenado
accuracy(prev01)
#Analisando o MAPE, por exemplo, que é uma medida percentual do módulo dos erros;
#Erro de previsão está apenas em 3,64%.

prev01
layout(1:1)
plot(prev01, main = "Modelo Forecasts SARIMA(1,1,0)(0,1,0)12 para o indicador de cobertura de atendimento para (ME) mais (EPP) de set/2024 a dez/2025", ylab = "Cobertura de atendimento para (ME) mais (EPP)", xlab = "Tempo")

## Omelhor dos melhores modelos, mas as predições são bem altas
best_model <- auto.arima(ts_indicador_me_epp_acumulado, lambda = 0, method = "ML", seasonal = TRUE)

# Ver o resumo do melhor modelo
summary(best_model)
checkresiduals(best_model)
t.test(best_model)
AIC(best_model)
BIC(best_model)
#Realizando a análise de Diagnóstico
diag <- tsdiag(best_model, gof.lag = 20)

require(forecast)
prev01 <- forecast(object = best_model, h=16, level = 0.95) #h=16, corresponde um ano e 4 meses de predição
autoplot(prev01, main = "Modelo Forecasts SARIMA(0,2,2)(0,0,1)12 para o indicador de cobertura de atendimento para (ME) mais (EPP) de set/2024 a dez/2025", ylab = "Cobertura de atendimento para (ME) mais (EPP)", xlab = "Tempo")
accuracy(best_model) #ou podemos chamar pelo objeto armazenado
accuracy(prev01)
#Analisando o MAPE, por exemplo, que é uma medida percentual do módulo dos erros;
#Erro de previsão está apenas em 3,64%.

prev01
layout(1:1)
plot(prev01, main = "Modelo Forecasts SARIMA(0,2,2)(0,0,1)12 para o indicador de cobertura de atendimento para (ME) mais (EPP) de set/2024 a dez/2025", ylab = "Cobertura de atendimento para (ME) mais (EPP)", xlab = "Tempo")
dataset01 <- prev01$residuals
View(dataset01)
library(openxlsx)
write.xlsx(prev01, "dataset_predicao_me_epp_acumulado.xlsx", rowNames = FALSE)




