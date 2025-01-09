'''
ÁREA 4: CIÊNCIAS EXATAS E DA TERRA:
  1 Agricultura 5.0.
  1.1 Noções de inteligência artificial, big data, data warehouse, descoberta de
conhecimento e mineração de dados, aprendizado de máquina e Internet das coisas (IoT).
2 Métodos de análise multivariada.
  2.1 PCA (análise de componentes principais), análise de clusters e análise discriminante.
3 Técnicas para redução de dimensionalidade e interpretação de dados complexos.
4 Estrutura de dados: variáveis, registros, banco de dados, estruturas de bancos de dados.
5 Fundamentos de estatística aplicada.
  5.1 Conceitos básicos de estatística descritiva: medidas de tendência central,
  dispersão e distribuição, forma assimétrica e curtose, associação entre variáveis
  quantitativas e qualitativas.
  5.2 Métodos de inferência estatística: estimativas, intervalos de confiança e
  testes de hipótese.
  5.3 Técnicas de amostragem, planejamento e análise de experimentos.
6 Métodos estatísticos para dados não-normais e não-paramétricos.
  6.1 Técnicas para análise de dados que não seguem distribuições normais.
  6.2 Métodos não-paramétricos: testes de Wilcoxon, Kruskal-Wallis e outros.
  6.3 Aplicação de técnicas robustas para dados com outliers e distribuições irregulares.
7 Modelagem estatística e regressão.
  7.1 Modelos de regressão linear e não linear: aplicação e interpretação.
  7.2 Regressão múltipla, análise de variância (ANOVA) e técnicas de modelagem avançada.
  7.3 Avaliação da adequação dos modelos e diagnóstico de problemas.
8 Processamento e análise de dados.
9 Agrometeorologia.
10 Bioclimatologia.
11 Engenharia de processos.
12 Geoprocessamento, sensoriamento remoto e geotecnologias.

OPÇÃO 40000237
ÁREA 12: MÉTODOS QUANTITATIVOS AVANÇADOS:
  1 Amostragem.
2 Probabilidade e estatística.
3 Estatística experimental.
4 Métodos de análise multivariada.
4.1 PCA (análise de componentes principais), análise de clusters e análise discriminante. 
5 Técnicas para redução de dimensionalidade e interpretação de dados complexos.
6 Estrutura de dados: variáveis, registros, banco de dados, estruturas de bancos de dados. 
7 Fundamentos de estatística aplicada. 
  7.1 Conceitos básicos de estatística descritiva: medidas de tendência central,
dispersão e distribuição, forma assimétrica e curtose, associação entre variáveis
quantitativas e qualitativas.
  7.2 Métodos de inferência estatística: estimativas, intervalos de confiança e testes de hipótese.
  7.3 Técnicas de amostragem, planejamento e análise de experimentos.
8 Métodos estatísticos para dados não-normais e não-paramétricos.
  8.1 Técnicas para análise de dados que não seguem distribuições normais.
  8.2 Métodos não-paramétricos: testes de Wilcoxon, Kruskal-Wallis e outros.
  8.3 Aplicação de técnicas robustas para dados com outliers e distribuições irregulares.
9 Modelagem estatística e regressão.
  9.1 Modelos de regressão linear e não linear: aplicação e interpretação.
  9.2 Regressão múltipla, análise de variância (ANOVA) e técnicas de modelagem avançada.
  9.3 Avaliação da adequação dos modelos e diagnóstico de problemas.
10 Gestão de dados.
11 Aprendizado de máquina.
12 Softwares R e Python.
'''

escreva = function(p_values=c(0:5), d_values=c(0:3), q_values=c(0:5),
                   P_values=c(0:5), D_values=c(0:3), Q_values=c(0:5)){
  
  Result = data.frame(p=0, d=0, q=0, P=0, D=0, Q=0,
                      p_valor=0, p_estac=0, p_ind=0, AIC=0)
  
  for (p in p_values) {
    for (d in d_values) {
      for (q in q_values) {
        for (P in P_values) {
          for (D in D_values) {
            for (Q in Q_values) {
              Result = rbind(Result, c(p, d, q, P, D, Q, 0, 0, 0, 0))
            }
          }
        }
      }
    }
  }
  write.csv(Result, "inicio.csv", sep=",")
}





