###---------- IMPORTANDO PACOTES E DADOS -------





# Instalando pacotes
install.packages('DataExplorer')
install.packages('ggplot2')
install.packages('gdplyr')
install.packages('lmtest')
install.packages('nima')
install.packages("car")
install.packages('psych')
install.packages('nlme')
install.packages('lme4')
install.packages('lattice')

# Importando pacotes
library(DataExplorer)
library(ggplot2)
library(dplyr)
library(lmtest)
library(nima)
library(car)
library(psych)
library(nlme)
library(lattice)
library(lme4)

# Importando os dados airquality
dados = airquality




  
###---------- PRÉ VIZUALIZAÇÃO DOS DADOS-----





# Visualizando as primeiras linhas dos dados dados
head(dados)

# Help para saber mais sobre as variáveis de atributo
?airquality

# Estrutura dos dados
str(dados)

# Pré análise dos dados
summary(dados)


# Porcentagem de dados faltantes
plot_missing(dados)

# Retorna a quantidade de linhas com ao menos 1 valor faltante
count(dados[rowSums(is.na(dados)) > 0,])





###---------- VALORES 'NA' -----


# Faz uma nova coluna do tipo date para os dias e meses
dia_inicio <- as.Date("1973/05/01")
dia_final <- as.Date("1973/09/30")
range <- seq(dia_inicio, dia_final,"days")
dados$Day <- range
dados$Month <- NULL
dados


dados_2 <- na.omit(dados)
dados_2


#Retorna a quantidade de observações sem os dados 'NA'
count(dados_2)

# Verificando outliers
boxplot(dados_2)
boxplot(dados_2$Wind)
boxplot(dados_2$Ozone)

max(dados_2$Ozone)

# Retorna a posição dos valores
which(dados_2$Ozone=='168')
which(dados_2$Ozone=='135')

# Exclui o valor com Ozone 168 na posição 77
dados_2 <- slice(dados_2, -c(77, 34))
dados_2


# Estrutura dos dados
str(dados_2)

# Retorna a quantidade de observações sem os dados 'NA'
count(dados_2)

#PowerPoint paginas 3 e 4


# Posso verificar se existe alguma relação entre Ozone e alguma variável
# Assim posso estimar um valor estatisticamente para os valores faltantes

# Correlação entre as variáveis
pairs.panels(dados_2)
# As variáveis Ozone e Temp tem uma forte correlação positiva
# As variáveis Ozone e Wind tem uma boa correlação negativa
# Podemos perceber que as variáveis independentes não tem uma forte
# correlação uma com a outra.

# Histograma Ozone
ggplot(data = dados_2, aes(x = Ozone)) +
  geom_histogram(fill = 'steelblue', color="black") +
  theme_bw() +
  ggtitle(" Histograma da variável Ozone") +
  labs(y = "Frequência", x="Ozone")
# A variável dependente não segue uma forma normal

# Teste shapiro wilk para verificar a normalidade da variável dependente
# Se maior do que 0.05 a distribuição é normal
shapiro.test(dados_2$Ozone)
shapiro.test(dados_2$Solar.R)
shapiro.test(dados_2$Wind)
shapiro.test(dados_2$Temp)
# O teste nos diz que a variável dependente NÂO estão normalmente distribuidos




# Scaterplot Ozone vs Temp
ggplot(dados_2) +
  geom_point(mapping = aes(x = Ozone, y = Temp))

# Scaterplot Ozone vs Wind
ggplot(dados_2) +
  geom_point(mapping = aes(x = Ozone, y = Wind))



# Observando os testes de normalidade e os gráficos, Os modelos lineares
# não teram boa performance com esse conjunto de dados

# PowerPoint pagina 5 e 6



# ----- Modelo Linear Multiplo ----

#Normalização com a raiz cúbica
cubica <- function(x){
  return(x^(1/3))
}

raizOz <- cubica(dados_2$Ozone)
raizOz

dados_2$Ozone


# Teste de normalidade
shapiro.test(raizOz)

dados_2$Ozone_R3 <- raizOz
dados_2

# Estimando o valor de Ozone
ozone_model <- lm(formula = Ozone_R3 ~
                    Temp
                  + Wind
                  + Solar.R, data=dados_2)


# Analizando os coeficientes
summary(ozone_model)

# 1º Para um modelo linear os erros devem ser normalmente distribuídos
# 2º para um modelo linear os erros são esperardos com vaiância constante  
# ou seja, devem ser homocedasticos
# 3º Para um modelo linear os erros não devem ser correlacionados
# 4º não deve haver multicolineariedade, ou seja, as variáveis independentes 
# não,devem ter uma forte correlação, normalmente acima de 0.8



# Histograma dos resíduos
ggplot(data = dados_2, aes(x = ozone_model$residuals)) +
  geom_histogram(fill = 'steelblue', color="black") +
  theme_bw()+
  ggtitle(" Histograma dos Resíduos do Modelo linear Ozone") +
  labs(y = "Frequência", x="Resíduos do Modelo Linear Ozone")
  

# QQplot para visualizar a normalidade dos resíduos
qqp(rstandard(ozone_model), "norm")

# Verifica a qualidade dos resíduos
par(mfrow=c(2,2))
plot(ozone_model)
par(mfrow=c(1,1))
# 1º gráfico: Resíduos dos valores ajustados, a linha vermelha deve estar 
# aproximadamente horizontal para que os resíduos sejam linear
# 2º gráfico: resíduos padronizados vs resíduos teóricos, os dados devem estar 
# em cima da linha pontilhada para considerar uma distribuição normal
# 3º gráfico: nos mostra se os resíduos são homocedasticos ou não, é  
# homocedastico se os dados estiverem dispersos retangularmente igual em todo  
# o grafico se triangular, considera-se heterocedastico
# 4º gráfico: se existir outliers entre os resíduos, haverá uma linha vermelha 
# nas bodas os dados que ultrapassarem para fora dessas bordas seram outliers




# Teste shapiro wilk para verificar a normalidade dos resíduos
# Se maior do que 0.05 a distribuição é normal
shapiro.test(ozone_model$residuals)
# O teste nos diz que os resíduos NÂO estão normalmente distribuidos


# Teste de Breush Pagan: se menor ou igual a 0.05 não consideramos sua
# homocedasticidade  
bptest(ozone_model)




# Predict do modelo linear multiplo
ozone_model_predito <- predict(ozone_model, data.frame(
  Temp = dados_2$Temp,
  Wind =dados_2$Wind,
  Solar.R=dados_2$Solar.R))

# Erro Percentual Absoluto Médio (MAPE)
# Porcentagem média de erro predito
mean(abs((dados_2$Ozone - ozone_model_predito^3) / dados_2$Ozone)) * 100





# ----- Modelo quadratico -----





# Estimando o valor de Ozone
ozone_quad <- lm(formula = Ozone_R3 ~
                   Temp
                 + I(Temp^2)
                 + Wind
                 + I(Wind^2)
                 + Solar.R,
                 data=dados_2)

# Analizando os coeficiente
summary(ozone_quad)
# Fiz vários testes e esse é o melhor ajuste que encontrei para este modelo

# Histograma dos resíduos
ggplot(data = dados_2, aes(x = ozone_quad$residuals)) +
  geom_histogram(fill = 'steelblue', color="black") +
  theme_bw()+
  ggtitle(" Histograma Resíduos do Modelo Quadrático de Ozone ") +
  labs(y = "Frequência", x="Resíduos do Modelo Quadrático")

# QQplot para visualizar a normalidade dos resíduos
qqp(rstandard(ozone_quad), "norm")


# Verifica a qualidade dos resíduos
par(mfrow=c(2,2))
plot(ozone_quad)
par(mfrow=c(1,1))

# Teste de normalidade, se p-value menor ou igual a 0.05 não consideramos
# sua normalidade
shapiro.test(ozone_quad$residuals)

# Verifica se há outliers nos resíduos, valor mínimo -3 e máximo +3
# além desses valóres significa que existem outliers
summary(rstandard(ozone_quad))

# Teste de Breush Pagan: se menor ou igual a 0.05 não consideramos sua
# homocedasticidade 
bptest(ozone_quad)



# Predict do modelo de regressão quadratica
ozone_quad_predito <- predict(ozone_quad, data.frame(
  Temp = dados_2$Temp,
  Wind =dados_2$Wind,
  Solar.R=dados_2$Solar.R))



# Erro Percentual absoluto médio (MAPE)
mean(abs((dados_2$Ozone - ozone_quad_predito^3) / dados_2$Ozone)) * 100



# ----- Modelo de regreção Logística ----





# Estimando o valor de Ozone
ozone_log <- lm(formula = Ozone_R3 ~
                  Temp
                + log(Temp)
                + log(Wind)
                + Solar.R, data=dados_2)

# Analizando os coeficiente
summary(ozone_log)

# Histograma dos resíduos
ggplot(data = dados_2, aes(x = ozone_log$residuals)) +
  geom_histogram(fill = 'steelblue', color="black") +
  theme_bw()+
  ggtitle(" Histograma Resíduos do Modelo Logístico de Ozone ") +
  labs(y = "Frequência", x="Resíduos do Modelo Logístico")


# Verifica a qualidade dos resíduos
par(mfrow=c(2,2))
plot(ozone_log)
par(mfrow=c(1,1))


# Verifica se há outliers nos resíduos, valor mínimo -3 e máximo +3
# além desses valóres significa que existem outliers
summary(rstandard(ozone_log))

# Teste de normalidade dosresíduos
shapiro.test(ozone_log$residuals)

# Teste de Breush Pagan: se menor ou igual a 0.05 não consideramos sua homocedasticidade 
bptest(ozone_log)


# Predict do modelo linear multiplo
ozone_log_predito <- predict(ozone_log, data.frame(
  Temp = dados_2$Temp,
  Wind =dados_2$Wind,
  Solar.R=dados_2$Solar.R))

# Porcentagem média de erro predito
mean(abs((dados_2$Ozone - ozone_log_predito^3) / dados_2$Ozone)) * 100





### --------- Teste de Qualidade dos modelos Ozone-----


# Anova nos retorna qual modelo estatisticamente é melhor
# O melhor modelo é o menor RSS
anova(ozone_model, ozone_quad, ozone_log)


# Verificando o comportamento dos valores preditos do modelo quadratico
# Gráfico valores reais vs preditos
# plot valores reais
plot(dados_2$Day, dados_2$Ozone,
     xlab = " Meses do Ano",
     ylab = "Ozônio em ppb",
     main = "Valores preditos X Valores reais")
grid(nx = NA, ny = NULL)
legend("topleft", legend=c("Valor Real", "Valor Predito"),
       col=c("black", "green"), lty=1:2, cex=0.6, merge = TRUE, inset = 0.01, )
lines(x = dados_2$Day,
      y = dados_2$Ozone,)
# linhas valores preditos
lines(x = dados_2$Day,
      y = round(ozone_quad_predito^3), col="green")
points(x = dados_2$Day,
      y = round(ozone_quad_predito^3), col="green")



### --------- Tratando os dados 'NA'-----

# Novo data frame com todas as linhas que contem 1 ou mais dados 'NA'
dados_na <- dados[rowSums(is.na(dados)) > 0,]
dados_na$Index <- seq.int(nrow(dados_na))
dados_na

# Tratando os dados NA da variável Solar.R utilizando a média simples
# Retorna a média da variável solar no data frame dados_2
media_solar <- round(mean(dados_2$Solar.R))
media_solar

# Retorna os dados NA da variável solar
solar_na <- dados_na[is.na(dados_na$Solar.R) > 0,]
solar_na <- solar_na$Index
solar_na

# Intera sobre os dados solar faltantes
dados_na$Solar.R[dados_na$Index %in% c(solar_na)] <- media_solar
dados_na



# Retorna a quantidade de linhas 
count(dados_na)
quantidade <- count(dados_na)

# Intera sobre o data frame dados_na, prediz o valor de ozone e substitui 
for (num in c(1:as.numeric(quantidade))){
  linha <- filter(dados_na, Index==num)

  if (is.na(linha$Ozone)){
    ozone_predito = predict(ozone_quad, data.frame(Temp = linha$Temp,
                                               Wind = linha$Wind,
                                               Solar.R = linha$Solar.R))
    dados_na$Ozone[dados_na$Index %in% c(num)] <- round(ozone_predito^3)
    
  }
}



# Remove a coluna Index
dados_na$Index <- NULL
dados_na

# Juntando os data frames dados_na + dados_2
dados <- bind_rows(dados_na, dados_2)


# Ordena por mês e por dia
dados <- dados[order(dados$Day),]
dados

# Refazendo a coluna raiz cubica de Ozone
cubica <- function(x){
  return(x^(1/3))
}

raizOz <- cubica(dados$Ozone)
dados$Ozone_R3 <- raizOz
dados


shapiro.test(dados$Ozone_R3)


# Estrutura dos dados
str(dados)

# Pré análise dos dados
summary(dados)


# Porcentagem de dados faltantes
plot_missing(dados)

# Retorna a quantidade de linhas com ao menos 1 valor faltante
count(dados[rowSums(is.na(dados)) > 0,])


# Exporta o data frame com a extensão .csv
write.csv(dados, "airqualitynotnull.csv", row.names = FALSE)





# ----- Estimando o modelo Principal ----



##PERGUNTAS
# O aumento do Ozônio tem alguma relação com a Temperatura?
plot(dados$Temp, dados$Ozone,
  xlab = " Temperaturaº",
  ylab = "Ozônio",
  main = "Temperatura Máxima Diária (Fº) vs Ozônio (ppb)")
# SIM existe essa relaçao, não linear, o ozone aumenta conforme a temperatura
# aumenta.


plot(dados$Wind, dados$Ozone,
     xlab = " Vento",
     ylab = "Ozônio",
     main = "Temperatura máxima diária (Fº) vs Ozônio (ppb)")
# SIM existe essa relaçao, não linear, o ozone diminue conforme a velocidade do
# vento aumenta

plot(dados$Wind, dados$Temp,
     xlab = " Vento",
     ylab = "Temperatura",
     main = "Temperatura máxima diária (Fº) vs Temperatura máxima diária (Fº)")
# Conforme a velocidade do vento aumenta, a temperatura diminui

# O aumento do Ozônio tem alguma relação com os raios solares?
plot(dados$Solar.R, dados$Ozone,
     xlab = " Raio Solar",
     ylab = "Ozônio",
     main = "Raio Solar (Langley) vs Ozônio (ppb)")
# O tem uma leve relaçao, se fosse na camada de ozonio seria a principal relação
# porém como estas medidas são a nível de solo, os raios solares chegam com
# menos força filtrados pela camada de ozonio



pairs.panels(dados)

# Estimando o modelo final para o valor de Ozone
Ozone_modelo_final <- lm(formula = Ozone_R3 ~
                           Temp
                         + I(Temp^2)
                         + Wind
                         + I(Wind^2)
                         + Solar.R,
                         data=dados)

# Analizando os coeficiente
summary(Ozone_modelo_final)
# Fiz vários testes e esse é o melhor ajuste que encontrei para o quadrático
# a variável Solar performa melhor sem elevar ao quadrado


# Predict do modelo de regressão quadratica
ozone_final_predito <- predict(Ozone_modelo_final, data.frame(Temp = dados$Temp, Wind =dados$Wind, Solar.R=dados$Solar.R))


# Porcentagem média de erro predito
mean(abs((dados$Ozone - ozone_final_predito^3) / dados$Ozone)) * 100




# Gráfico valores reais vs preditos
plot(dados$Day, dados$Ozone,
     xlab = " Meses do Ano",
     ylab = "Ozônio em ppb",
     main = "Valores preditos X Valores reais")
grid(nx = NA, ny = NULL)
legend("topleft", legend=c("Valor Real", "Valor Predito"),
       col=c("black", "green"), lty=1:2, cex=0.6, merge = TRUE, inset = 0.01, )
lines(x = dados$Day,
      y = dados$Ozone,)
lines(x = dados$Day,
      y = ozone_final_predito^3, col="green")
points(x = dados$Day,
       y = ozone_final_predito^3, col="green")


#----- Testando o modelo com valores reais----


# converter langley: https://hextobinary.com/unit/heatdensity/from/langley/to/whpmi2
# 1 Langlay é equivalente a 11.62 w/m²
#
# Ver a radiação solar em NY
# https://pt.tutiempo.net/radiacao-solar/new-york-la-guardia-airport.html
#
# Velocidade do vento e temperatura em Fº NY
# https://www.bing.com/search?q=tempo+em+nova+york&cvid=95d9d609c7874bf39c46210bd62903e3&aqs=edge.1.69i57j0l8.4111j0j1&pglt=131&FORM=ANNTA1&PC=U531
#
# Qualidade do ar em NY
# https://pt.tutiempo.net/qualidade-do-ar/new-york-la-guardia-airport.html


# Função para qualidade do ar de acordo com Ozone
# Converte w/m² para Langley 5 horas somadas
# Retorna o valor de Ozone e a qualidade
qualidade_do_ar <- function(F_Temp, Vento, R_Solar){
  wh_to_Lang <- round(R_Solar / 58.1)
  wh_to_Lang <- wh_to_Lang^(1/3)
  pred <- predict(Ozone_modelo_final,
                  data.frame(Temp = F_Temp,
                             Wind = Vento,
                             Solar.R = wh_to_Lang))
  pred <- round(pred^3)
  if (pred <= 54){
    cat("Ozone =", pred,",", "Qualidade do ar = Boa")
  }
  if (pred > 55 & pred <= 70){
    cat("Ozone =", pred,",", "Qualidade do ar = Moderada")
  }
  if (pred >= 71 & pred <= 85){
    cat("Ozone =", pred,",", "Qualidade do ar = Prejudicial para
grupos sensíveis")
  }
  if (pred >= 86 & pred <= 105){
    cat("Ozone =", pred,",", "Qualidade do ar = Prejudicial")
  }
  if (pred >= 106 & pred <= 200){
    cat("Ozone =", pred,",", "Qualidade do ar =  Muito prejudicial à saúde")
  }
  if (pred > 200){
    cat("Perigoso")
  }
}

# Tempo em graus Fahrenheit
# vento em Mph
# Raio Solar em W/m² calcular as ultimas 5 horas 
qualidade_do_ar(F_Temp = 84, Vento = 7, R_Solar = 2362)




