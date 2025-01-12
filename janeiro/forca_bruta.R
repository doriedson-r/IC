library(tidyverse)
source("abrev.R")

calcular_estatisticas_e_adicionar <- function(arquivo_csv, colunas_para_calculo,
                                              novas_colunas, linha_inicial,
                                              linha_final) {
  # Lê o arquivo CSV e transforma em um dataframe
  dados <- read.csv(arquivo_csv)[1:10]
  
  # Cria uma nova coluna para identificar cada linha (opcional)
  #dados <- dados %>%
  #  mutate(linha = row_number())
  
  # Cria uma função auxiliar para calcular as estatísticas
  calcular_estatisticas <- function(linha) {
    # Extrai os valores das colunas para cálculo
    valores <- linha[colunas_para_calculo]
    p = valores[1]; P = valores[4]
    d = valores[2]; D = valores[5]
    q = valores[3]; Q = valores[6]
    
    print(valores)
    # Calcula as estatísticas desejadas
    tryCatch({
      #model <- arima(data, order = c(p, d, q), method="ML")
      model = forecast::Arima(data, order = c(p,d,q), seasonal = c(P,D,Q))
      shapiro_test <- shapiro.test(residuals(model))
      adf = tseries::adf.test(residuals(model))
      indep = Box.test(residuals(model), type="Ljung")
      
      p_estac = adf$p.value
      p_norm <- shapiro_test$p.value
      p_ind <- indep$p.value
      
      #Result = rbind(Result, c(p, d, q, P, D, Q, p_norm, p_estac,
                              #p_ind, AIC(model)))
      
      if (check_normality(model$residuals) && AIC(model) < best_aic) {
        best_model <- model
        best_aic <- AIC(model)
      }
    }, error = function(e) {
      # Handle errors gracefully, e.g., print a warning message
      warning("Error fitting ARIMA(", p, ", ", d, ", ", q, "): ", e$message)
    })
    
    # Retorna um vetor com os resultados
    return(c(p_norm, p_estac, p_ind, AIC(model)))
  }
  
  # Aplica a função calcular_estatisticas para cada linha e adiciona as novas colunas
  resultados <- dados %>%
    slice(linha_inicial:linha_final) %>%
    mutate(
      across(.cols = all_of(colunas_para_calculo),
             .fns = list(normal_ = p_norm, estacio_ = p_estac,
                         indep_ = p_ind, aic = AIC(model)))
    )
  
  # Escreve os resultados em um novo arquivo CSV
  write_csv(resultados, paste0(arquivo_csv, "_com_estatisticas.csv"))
}

# Function to check normality of residuals
check_normality <- function(residuals) {
  shapiro_test <- shapiro.test(residuals)
  p_value <- shapiro_test$p.value
  return(p_value > 0.05)
}

lendo_escrevendo = function(dados=read.csv(serie_final),
                            arquivo=read.csv(combinacoes),
                            linha_inicial, linha_final){
  # Ajusta modelos com todas combinações de coeficientes disponibilizados.
  # Também testa normalidade e estacionariedade de cada modelo.
  
  best_model <- NULL
  best_aic <- Inf
  
  ini = proc.time()
  
  Result = data.frame(p=0, d=0, q=0, P=0, D=0, Q=0,
                      p_valor=0, p_estac=0, p_ind=0, AIC=0)
  
  serie = ts(dados[3], start=c(2018, 1), frequency = 12)
  
  for (linha in c(linha_inicial:linha_final)) {
    atual = arquivo[linha, ]
    p = atual[[2]]; d = atual[[3]]; q = atual[[4]]
    P = atual[[5]]; D = atual[[6]]; Q = atual[[7]]
              
    tryCatch({
      #model <- arima(data, order = c(p, d, q), method="ML")
      model = forecast::Arima(serie, order = c(p,d,q), seasonal = c(P,D,Q))
      shapiro_test <- shapiro.test(residuals(model))
      adf = tseries::adf.test(residuals(model))
      indep = Box.test(residuals(model), type="Ljung")
      
      p_estac = adf$p.value
      p_norm <- shapiro_test$p.value
      p_ind <- indep$p.value
      
      #Result = rbind(Result, c(p, d, q, P, D, Q, p_norm, p_estac, p_ind, AIC(model)))
      Result = data.frame(p, d, q, P, D, Q, p_norm, p_estac, p_ind, AIC(model))
      
      if (check_normality(model$residuals) && AIC(model) < best_aic) {
        best_model <- model
        best_aic <- AIC(model)
      }
    }, error = function(e) {
      # Handle errors gracefully, e.g., print a warning message
      warning("Error fitting ARIMA(", p, ", ", d, ", ", q, "): ", e$message)
    })
    
    #atual[c(8:11)] <- c(p_norm, p_estac, p_ind, AIC(model))
    cat("  ", linha)
    
    write.table(Result, file = "escrita_.csv", append = TRUE, col.names = F)
  
  }
  
  x = proc.time() - ini
  cat("\n"); print(x)
  
  #write.csv(Result,
            #file = paste0("escrita_de", linha_inicial, "a", linha_final, ".csv"))
}


lendo_escrevendo(linha_inicial =  17001, linha_final = 20737)
