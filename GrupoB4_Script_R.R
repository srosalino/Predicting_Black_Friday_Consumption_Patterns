# Importação de bibliotecas

library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(hexbin)
library(car)
library(lmtest)
library(corrplot)
library(tseries)
library(fastDummies)
library(recipes)
library(stargazer)
library(MASS)
library(corrplot)
library(caTools)
library(Metrics)
library(readxl)
library(epiDisplay)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(psych)


# Importação de dados

bd <- read.csv("C:/Universidade_ISCTE/2ºAno/1.º_Semestre_21_22/Introdução A Modelos Dinâmicos/0 - Trabalho Grupo/BlackFriday_bd_inteira.csv")
View(bd)

# User_ID - Variável Qualitativa Nominal                 
# Product_ID - Variável Qualitativa Nominal        
# Gender - Variável Qualitativa Nominal                  
# Age - Variável Qualitativa Ordinal                      
# Occupation - Variável Qualitativa Nominal               
# City_Category - Variável Qualitativa Nominal             
# Stay_In_Current_City_Years - Variável Qualitativa Ordinal
# Marital_Status - Variável Qualitativa Nominal            
# Product_Category_1 - Variável Qualitativa Nominal         
# Product_Category_2 - Variável Qualitativa Nominal      
# Product_Category_3 - Variável Qualitativa Nominal       
# Purchase - Variável Quantitativa Discreta


# User_ID - Variável Independente                 
# Product_ID - Variável Independente        
# Gender - Variável Independente                  
# Age - Variável Independente                      
# Occupation - Variável Independente               
# City_Category - Variável Independente            
# Stay_In_Current_City_Years - Variável Independente 
# Marital_Status - Variável Independente             
# Product_Category_1 - Variável Independente          
# Product_Category_2 - Variável Independente      
# Product_Category_3 - Variável Independente       
# Purchase - Variável Dependente


# Sumário com informação estatística básica sobre as variáveis
summary(bd %>% dplyr::select(!c(User_ID, Product_ID)))

# Estrutura dos dados
str(bd)

###################
# Valores omissos #
###################


# Product_Category_1
somaprod1 = sum(is.na(bd$Product_Category_1))
cat("Existem", somaprod1, "valores omissos na coluna que corresponde à variável Product_Category_1")


# Product_Category_2
somaprod2 = sum(is.na(bd$Product_Category_2))
percentprod2 = percent(somaprod2/dim(bd)[1])
cat("Existem", somaprod2, "valores omissos, ou seja,", percentprod2,
    "dos registos da coluna que corresponde à variável Product_Category_2 não possuem um valor atribuído. 
Posto isto, procedemos à remoção das linhas que possuem valores omissos associados a esta variável.")

bdna = drop_na(bd, Product_Category_2)

# Product_Category_3
somaprod3 = sum(is.na(bd$Product_Category_3))
percentprod3 = percent(somaprod3/dim(bd)[1])
cat("Existem", somaprod3, "valores omissos, ou seja,", percentprod3,
    "dos registos da coluna que corresponde à variável Product_Category_3 não possuem um valor atribuído.
Dado que a quantidade de valores omissos excede os 50% esta variável não deve ser utilizada para inferência estatística")

bdna = bdna %>% dplyr::select(!c(Product_Category_3))


# Dataset contendo apenas registos relacionados aos produtos

bddistinctprod = bdna %>%
  group_by(Product_ID) %>%
  mutate(Purchase=sum(Purchase), Product_Category_1, Product_Category_2, .keep="used") %>%
  ungroup()

# Dataset contendo apenas registos com User_ID unico (utilizada apenas para efeitos de contagem)

bddistinctuser = bdna %>%
  group_by(User_ID) %>%
  mutate(Purchase=sum(Purchase), Gender, Age, Occupation, Stay_In_Current_City_Years, City_Category, Marital_Status,
         Product_Category_1, Product_Category_2, .keep="used") %>%
  distinct(User_ID, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(User_ID)


#Total de Mulheres que efetuaram compras
f1 = sum(bddistinctuser$Gender == "F")
f1

#Total de Homens que efetuaram compras
m1 = sum(bddistinctuser$Gender == "M")
m1

#Total de compras efetuado pelo sexo feminino
f2 = sum(bdna$Gender == "F")
f2

#Total de compras efetuado pelo sexo masculino
m2 = sum(bdna$Gender == "M")
m2


#####################################################
# Estatísticas descritivas (Após a remoção de NA's) #
#####################################################

summary(bdna %>%  dplyr::select(!c(User_ID, Product_ID)))

# Para tabela bddistinctuser


# Para Gender


tab1(bddistinctuser$Gender)


a <- ggplot(bddistinctuser, aes(x=Gender, fill = Gender)) + geom_bar(width= 0.3) +
  geom_text(stat="count", aes(label = ..count..), vjust = 1, colour = "black") +
  scale_y_continuous(name="Count", labels = comma) + theme_gray() +
  scale_fill_manual(name = "Gender", labels=c("Female","Male"), values=c("#FF3333","#3399FF"))

a


# Para Age


tab1(bddistinctuser$Age)


b <- ggplot(bddistinctuser, aes(x = Age)) + geom_bar(width= 0.3, fill="#A6A6A6") +
  geom_text(stat="count", aes(label = ..count..), vjust = 1, colour = "black") +
  scale_y_continuous(name="Count", labels = comma) + theme_light()

b

# Para Marital Status


tab1(bddistinctuser$Marital_Status)


c <-  ggplot(bddistinctuser, aes(x=Marital_Status, fill = as.factor(Marital_Status))) + geom_bar(width= 0.1) +
  geom_text(stat="count", aes(label = ..count..), vjust = 1, colour = "black") +
  scale_y_continuous(name="Count", labels = comma) + theme_gray() +
  scale_x_continuous(breaks = seq(0,1,1)) +
  scale_fill_manual(name = "Marital_Status", labels=c("Single","Married"), values=c("#FF3333","#3399FF"))

c


# Para Occupation


tab1(bddistinctuser$Occupation, sort.group = "decreasing")


d <-  ggplot(data=bddistinctuser, aes(x=Occupation, 
                                      y=Purchase,fill=Gender)) +
  geom_bar(stat="identity") + scale_y_continuous(labels = comma) + scale_x_continuous(breaks=c(0:20))

d


# Para City_Category


tab1(bddistinctuser$City_Category)


e <- ggplot(bddistinctuser, aes(x = City_Category)) + geom_bar(width= 0.3, fill=c("A"="#D9D9D9","B" = "#A6A6A6","C"= "#737373")) +
  geom_text(stat="count", aes(label = ..count..), vjust = 1, colour = "black") +
  scale_y_continuous(name="Count", labels = comma) + theme_light()

e

# Para Stay_In_Current_City_Years


tab1(bddistinctuser$Stay_In_Current_City_Years)


f <- ggplot(bddistinctuser, aes(x = Stay_In_Current_City_Years)) + geom_bar(width= 0.3,fill="#A6A6A6") +
  geom_text(stat="count", aes(label = ..count..), vjust = 1, colour = "black") +
  scale_y_continuous(name="Count", labels = comma) + theme_light()

f


# Para Purchase bddistinctuser


describe(bddistinctuser$Purchase)


# Em relação à tabela bddistinctprod


# Para Purchase_Category_1


tab1(bd$Product_Category_1)

options(scipen=999)

g <- ggplot(bd, aes(x=Product_Category_1)) +
  geom_bar(stat="count", fill = "#A6A6A6")+theme_light() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

g 


# Para Purchase_Category_2


tab1(bd$Product_Category_2)

options(scipen=999)

h <- ggplot(bd, aes(x=Product_Category_2)) +
  geom_bar(stat="count", fill = "#A6A6A6")+theme_light() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

h 


describe(bddistinctprod$Purchase)


###########
# Dummies #
###########

# Ao fatorizar as variáveis estamos a defini-las como variáveis dummy

bdna$Gender = as.factor(bdna$Gender)

bdna$Age = as.factor(bdna$Age)

bdna$City_Category = as.factor(bdna$City_Category)

bdna$Age = as.factor(bdna$Age)

bdna$Stay_In_Current_City_Years = as.factor(bdna$Stay_In_Current_City_Years)

bdna$Age = as.factor(bdna$Age)

bdna$Occupation = as.factor(bdna$Occupation)

bdna$User_ID = as.factor(bdna$User_ID)

bdna$Marital_Status = as.factor(bdna$Marital_Status)

bdna$Product_Category_1 = as.factor(bdna$Product_Category_1)

bdna$Product_Category_2 = as.factor(bdna$Product_Category_2)


# Criação de uma subamostra tendo como critério Stay_In_Current_City_Years == "4+"

samplecity = bdna %>% dplyr::filter(bdna$Stay_In_Current_City_Years == "4+") %>% 
  dplyr::select(!c(Stay_In_Current_City_Years, User_ID, Product_ID))

bdnum = samplecity %>% mutate_if(is.factor,as.numeric) # conversão das variáveis fatorizadas em numéricas

bdfactor = samplecity

########################################################################################################
# Estatísticas descritivas (Após a remoção de NA's, criação da subsample e fatorização das variáveis) #
########################################################################################################

summary(samplecity)


##########################################
# Correlação e causalidade (Subamostra)  #
##########################################

# Matriz de correlação (Pearson)
cor(bdnum)

# Forma visual de representar a matriz de correlação
corrplot(cor(bdnum), method = "color", type = "full", order = "original", number.cex = 0.75,
         addCoef.col = "black", tl.col = "black", tl.srt = 90, diag = FALSE, mar=c(0,0,1,0))

# representação gráfica de pares de variáveis
pairs(bdnum)


############
# Modelo 1 #
############

model1=lm(bdfactor$Purchase~. , bdfactor)

summary(model1)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
ols_step_both_aic(model1)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
ols_step_both_p(model1)

# Todas as variáveis se mantêm uma vez que são todas estatísticamente significativas 
# para o modelo (Ambos os métodos de escolha encontram-se em concordância)


###########################################
# Verificação dos pressupostos (Modelo 1) #
###########################################


residuos_model1 = model1$residuals

# Pressuposto 1

mean(residuos_model1)

# Pressuposto 2

bptest(model1)

# Pressuposto 3

bgtest(model1)

# Pressuposto 4

jarque.bera.test(residuos_model1)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model1, main="Model 1")


# Verifica-se a existência de multicolineariedade entre as variáveis independentes (VIF >5 significa que existe)
vif(model1)

#################################################################

############
# Modelo 2 #
############

model2 = lm(log(bdfactor$Purchase) ~ Gender + Age + Occupation + 
              City_Category + Marital_Status + Product_Category_1 + 
              Product_Category_2, data = bdfactor)

# Aplicou-se uma transformação logaritmica sobre a variável Purchase 

summary(model2)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
ols_step_both_aic(model2)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
ols_step_both_p(model2)

# Todas as variáveis se mantêm uma vez que são todas estatísticamente significativas 
# para o modelo (Ambos os métodos de escolha encontram-se em concordância)


###########################################
# Verificação dos pressupostos (Modelo 2) #
###########################################

residuos_model2 = model2$residuals

# Pressuposto 1

mean(residuos_model2)

# Pressuposto 2

bptest(model2)

# Pressuposto 3

bgtest(model2)

# Pressuposto 4

jarque.bera.test(residuos_model2)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model2, main="Model 2")

# Comparação do AIC de ambos os modelos
AIC(model1, model2)

# Verificação da existência de multicolinearidade entre as variáveis independentes (VIF >5)

vif(model2)

# Não existem variáveis multicolineares
# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica

# Verificar graficamente a existÊncia de extremos influenciadores
influenceIndexPlot (model2, id=list(n=3))

# Existem pontos influenciadores que não correspondem a outliers.

# distância de Cook
cooksd = cooks.distance(model2)

# detetar se existem influenciadores
influential = as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])

# visualizar os influenciadores
head(bdfactor[influential, ])


##############
# Modelo 3-8 #
##############

#############################################################
# Método de estimação dos mínimos quadrados com pesos (WLS) #
#############################################################

# Aplicação do método de estimação dos mínimos quadrados com pesos

N = length(bdfactor$Purchase) # tamanho do dataset
peso = 1/((1:N)^0.5) # cálculo do peso

model3 = lm(log(bdfactor$Purchase) ~ Gender + Age + Occupation + 
              City_Category + Marital_Status + Product_Category_1 + 
              Product_Category_2, data = bdfactor, weights = peso)

summary(model3)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model3)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model3)


###########################################
# Verificação dos pressupostos (Modelo 3) #
###########################################

residuos_model3 = model3$residuals

# Pressuposto 1

mean(residuos_model3)

# Pressuposto 2

bptest(model3)

# Pressuposto 3

bgtest(model3)

# Pressuposto 4

jarque.bera.test(residuos_model3)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model3, main="Model 3")

# Comparação do AIC dos modelos
AIC(model1, model2, model3)

# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica


################


N2 = predict(model2, data=bdfactor)
peso2 = 1/sqrt(N2)

model4 = lm(log(bdfactor$Purchase) ~ Gender + Age + Occupation + 
              City_Category + Marital_Status + Product_Category_1 + 
              Product_Category_2, data = bdfactor, weights = peso2)

summary(model4)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
ols_step_both_aic(model4)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model4)

###########################################
# Verificação dos pressupostos (Modelo 4) #
###########################################

residuos_model4 = model4$residuals 

# Pressuposto 1

mean(residuos_model4)

# Pressuposto 2

bptest(model4)

# Pressuposto 3

bgtest(model4)

# Pressuposto 4

jarque.bera.test(residuos_model4)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model4, main="Model 4")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4)

# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica


# Regressão Linear Robusta

model5 = rlm(formula = log(bdfactor$Purchase) ~ Gender + Age + Occupation + 
               City_Category + Marital_Status + Product_Category_1 + 
               Product_Category_2, data = bdfactor, weights = peso2)

summary(model5)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model5)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model5)

###########################################
# Verificação dos pressupostos (Modelo 5) #
###########################################

residuos_model5 = model5$residuals

# Pressuposto 1

mean(residuos_model5)

# Pressuposto 2

bptest(model5)

# Pressuposto 3

bgtest(model5)

# Pressuposto 4

jarque.bera.test(residuos_model5)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model5, main="Model 5")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5)

# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica



# Nova abordagem para o calculo dos pesos

peso3 = 1/sqrt(model2$residuals^2)

model6 = lm(formula = log(bdfactor$Purchase) ~ Gender + Age + Occupation + 
              City_Category + Marital_Status + Product_Category_1 + 
              Product_Category_2, data = bdfactor, weights = peso3)

summary(model6)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
ols_step_both_aic(model6)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model6)

###########################################
# Verificação dos pressupostos (Modelo 6) #
###########################################

residuos_model6 = model6$residuals

# Pressuposto 1

mean(residuos_model6)

# Pressuposto 2

bptest(model6)

# Pressuposto 3

bgtest(model6)

# Pressuposto 4

jarque.bera.test(residuos_model6)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model6, main="Model 6")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6)

# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica



# Nova abordagem para o calculo dos pesos

peso4 = model2$residuals^2

model7 = lm(formula = log(bdfactor$Purchase) ~ Gender + Age + Occupation + 
              City_Category + Marital_Status + Product_Category_1 + 
              Product_Category_2, data = bdfactor, weights = 1/peso4)

summary(model7)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model7)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model7)

###########################################
# Verificação dos pressupostos (Modelo 7) #
###########################################

residuos_model7 = model7$residuals

# Pressuposto 1

mean(residuos_model7)

# Pressuposto 2

bptest(model7)

# Pressuposto 3

bgtest(model7)

# Pressuposto 4

jarque.bera.test(residuos_model7)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model7, main="Model 7")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7)

# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica



# Outra abordagem para o calculo dos pesos


varfunc.ols = lm((model2$residuals^2) ~. , data = bdfactor)
varfunc1 = varfunc.ols$fitted.values
peso5 = 1/sqrt((varfunc1))


model8 = lm(log(bdfactor$Purchase) ~ Gender + Age + Occupation + 
              City_Category + Marital_Status + Product_Category_1 + 
              Product_Category_2, data = bdfactor, weights = peso5)

summary(model8)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model8)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model8)

###########################################
# Verificação dos pressupostos (Modelo 8) #
###########################################

residuos_model8 = model8$residuals

# Pressuposto 1

mean(residuos_model8)

# Pressuposto 2

bptest(model8)

# Pressuposto 3

bgtest(model8)

# Pressuposto 4

jarque.bera.test(residuos_model8)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model8, main="Model 8")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8)


#############################


#############
# Modelo 9A #
#############

# Para este modelo será testada a interação entre as variáveis Gender e Occupation.

model9A = lm(formula = log(bdfactor$Purchase) ~ Age + 
               City_Category + Marital_Status + Product_Category_1 + 
               Product_Category_2 + Gender*Occupation, data = bdfactor)

summary(model9A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model9A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model9A)


###########################################
# Verificação dos pressupostos (Modelo 9A) #
###########################################

residuos_model9A = model9A$residuals

# Pressuposto 1

mean(residuos_model9A)

# Pressuposto 2

bptest(model9A)

# Pressuposto 3

bgtest(model9A)

# Pressuposto 4

jarque.bera.test(residuos_model9A)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model9A, main="Model 9A")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A)

# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica


############
# Modelo 9B #
############

# Para este modelo será testada a interação entre as variáveis Gender e Occupatio com pesos

varfunc.ols9 = lm((model9A$residuals^2) ~. , data = bdfactor)
varfunc9 = varfunc.ols9$fitted.values
peso9a = 1/sqrt((varfunc9))

model9B = lm(formula = log(bdfactor$Purchase) ~ Age + 
               City_Category + Marital_Status + Product_Category_1 + 
               Product_Category_2 + Gender*Occupation, data = bdfactor,  weights = peso9a)

summary(model9B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model9B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model9B)


###########################################
# Verificação dos pressupostos (Modelo 9B)#
###########################################

residuos_model9B = model9B$residuals

# Pressuposto 1

mean(residuos_model9B)

# Pressuposto 2

bptest(model9B)

# Pressuposto 3

bgtest(model9B)

# Pressuposto 4

jarque.bera.test(residuos_model9B)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model9B, main="Model 9B")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B)

# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica



##############
# Modelo 10A #
##############

# Para este modelo será testada a interação entre as variáveis Gender e Marital_Status

model10A = lm(formula = log(bdfactor$Purchase) ~ Age + Occupation + Gender +
                City_Category + Marital_Status + Product_Category_1 + 
                Product_Category_2 + Gender*Marital_Status , data = bdfactor)

summary(model10A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model10A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model10A)


#############################################
# Verificação dos pressupostos (Modelo 10A) #
#############################################

residuos_model10A = model10A$residuals

# Pressuposto 1

mean(residuos_model10A)

# Pressuposto 2

bptest(model10A)

# Pressuposto 3

bgtest(model10A)

# Pressuposto 4

jarque.bera.test(residuos_model10A)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model10A, main="Model 10A")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B, model10A)

# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica



##############
# Modelo 10B #
##############

# Para este modelo será testada a interação entre as variáveis Gender e Marital_Status com pesos

varfunc.ols10 = lm((model10A$residuals^2) ~. , data = bdfactor)
varfunc10 = varfunc.ols10$fitted.values
peso10a = 1/sqrt((varfunc10))

model10B = lm(formula = log(bdfactor$Purchase) ~ Age + Occupation + Gender +
                City_Category + Marital_Status + Product_Category_1 + 
                Product_Category_2 + Gender*Marital_Status , data = bdfactor, weights = peso10a)

summary(model10B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model10B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model10B)


#############################################
# Verificação dos pressupostos (Modelo 10B) #
#############################################

residuos_model10B = model10B$residuals

# Pressuposto 1

mean(residuos_model10B)

# Pressuposto 2

bptest(model10B)

# Pressuposto 3

bgtest(model10B)

# Pressuposto 4

jarque.bera.test(residuos_model10B)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model10B, main="Model 10B")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B, model10A, model10B)

# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica


##############
# Modelo 11A #
##############

# Para este modelo será testada a interação entre as variáveis Occupation e Marital_Status

model11A = lm(formula = log(bdfactor$Purchase) ~ Age + Occupation + Gender +
                City_Category + Marital_Status + Product_Category_1 + 
                Product_Category_2 + Occupation*Marital_Status , data = bdfactor)

summary(model11A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model11A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model11A)


#############################################
# Verificação dos pressupostos (Modelo 11A) #
#############################################

residuos_model11A = model11A$residuals

# Pressuposto 1

mean(residuos_model11A)

# Pressuposto 2

bptest(model11A)

# Pressuposto 3

bgtest(model11A)

# Pressuposto 4

jarque.bera.test(residuos_model11A)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model11A, main="Model 11A")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B, model10A, model10B,
    model11A)


# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica


##############
# Modelo 11B #
##############

# Para este modelo será testada a interação entre as variáveis Occupation e Marital_Status com pesos

varfunc.ols11 = lm((model11A$residuals^2) ~. , data = bdfactor)
varfunc11 = varfunc.ols11$fitted.values
peso11a = 1/sqrt((varfunc11))

model11B = lm(formula = log(bdfactor$Purchase) ~ Age + Occupation + Gender +
                City_Category + Marital_Status + Product_Category_1 + 
                Product_Category_2 + Occupation*Marital_Status , data = bdfactor, weights = peso11a)

summary(model11B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model11B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model11B)


#############################################
# Verificação dos pressupostos (Modelo 11B) #
#############################################

residuos_model11B = model11B$residuals

# Pressuposto 1

mean(residuos_model11B)

# Pressuposto 2

bptest(model11B)

# Pressuposto 3

bgtest(model11B)

# Pressuposto 4

jarque.bera.test(residuos_model11B)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model11B, main="Model 11B")

# Comparação do AIC de ambos os modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B, model10A, model10B,
    model11A, model11B)


# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica



#############
# Modelo 12A #
#############

# Para este modelo será testada a interação entre as variáveis: Occupation e Marital_Status | Age e City_Category

model12A = lm(formula = log(bdfactor$Purchase) ~ Age + Occupation + Gender +
                City_Category + Marital_Status + Product_Category_1 + 
                Product_Category_2 + Occupation*Marital_Status + Age*City_Category , data = bdfactor)

summary(model12A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model12A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model12A)


###########################################
# Verificação dos pressupostos (Modelo 13) #
###########################################

residuos_model12A = model12A$residuals

# Pressuposto 1

mean(residuos_model12A)

# Pressuposto 2

bptest(model12A)

# Pressuposto 3

bgtest(model12A)

# Pressuposto 4

jarque.bera.test(residuos_model12A)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model12A, main="Model 12A")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B, model10A, model10B,
    model11A, model11B, model12A)



#############
# Modelo 12B #
#############

# Para este modelo será testada a interação entre as variáveis: Occupation e Marital_Status | Age e City_Category com pesos

varfunc.ols12 = lm((model12A$residuals^2) ~. , data = bdfactor)
varfunc12 = varfunc.ols12$fitted.values
peso12a = 1/sqrt((varfunc12))

model12B = lm(formula = log(bdfactor$Purchase) ~ Age + Occupation + Gender +
                City_Category + Marital_Status + Product_Category_1 + 
                Product_Category_2 + Occupation*Marital_Status + Age*City_Category , data = bdfactor, weights = peso12a)

summary(model12B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model12B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model12B)


###########################################
# Verificação dos pressupostos (Modelo 13) #
###########################################

residuos_model12B = model12B$residuals

# Pressuposto 1

mean(residuos_model12B)

# Pressuposto 2

bptest(model12B)

# Pressuposto 3

bgtest(model12B)

# Pressuposto 4

jarque.bera.test(residuos_model12B)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model12B, main="Model 12B")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B, model10A, model10B,
    model11A, model11B, model12A, model12B)


#############
# Modelo 13A #
#############

# Para este modelo será testada a interação entre as variáveis: Occupation e Gender | Age e City_Category

model13A = lm(formula = log(bdfactor$Purchase) ~ Age + Occupation + Gender +
                City_Category + Marital_Status + Product_Category_1 + 
                Product_Category_2 + Occupation*Gender + Age*City_Category , data = bdfactor)

summary(model13A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model13A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model13A)


###########################################
# Verificação dos pressupostos (Modelo 13A) #
###########################################

residuos_model13A = model13A$residuals

# Pressuposto 1

mean(residuos_model13A)

# Pressuposto 2

bptest(model13A)

# Pressuposto 3

bgtest(model13A)

# Pressuposto 4

jarque.bera.test(residuos_model13A)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model13A, main="Model 13A")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B, model10A, model10B,
    model11A, model11B, model12A, model12B, model13A)


#############
# Modelo 13B #
#############

# Para este modelo será testada a interação entre as variáveis: Occupation e Gender | Age e City_Category com pesos

varfunc.ols13 = lm((model13A$residuals^2) ~. , data = bdfactor)
varfunc13 = varfunc.ols13$fitted.values
peso13a = 1/sqrt((varfunc13))

model13B = lm(formula = log(bdfactor$Purchase) ~ Age + Occupation + Gender +
                City_Category + Marital_Status + Product_Category_1 + 
                Product_Category_2 + Occupation*Gender + Age*City_Category , data = bdfactor, weights = peso13a)

summary(model13B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model13B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model13B)


###########################################
# Verificação dos pressupostos (Modelo 13B) #
###########################################

residuos_model13B = model13B$residuals

# Pressuposto 1

mean(residuos_model13B)

# Pressuposto 2

bptest(model13B)

# Pressuposto 3

bgtest(model13B)

# Pressuposto 4

jarque.bera.test(residuos_model13B)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model13B, main="Model 13B")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B, model10A, model10B,
    model11A, model11B, model12A, model12B, model13A, model13B)


#############
# Modelo 14A#
#############

# Para este modelo será testada a interação entre as variáveis: Occupation e Marital_Status | Gender e City_Category

model14A = lm(formula = log(bdfactor$Purchase) ~ Age + Occupation + Gender +
                City_Category + Marital_Status + Product_Category_1 + 
                Product_Category_2 + Occupation*Marital_Status + Gender*City_Category , data = bdfactor)

summary(model14A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model14A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model14A)


###########################################
# Verificação dos pressupostos (Modelo 14A) #
###########################################

residuos_model14A = model14A$residuals

# Pressuposto 1

mean(residuos_model14A)

# Pressuposto 2

bptest(model14A)

# Pressuposto 3

bgtest(model14A)

# Pressuposto 4

jarque.bera.test(residuos_model14A)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model14A, main="Model 14A")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B, model10A, model10B,
    model11A, model11B, model12A, model12B, model13A, model13B, model14A)



#############
# Modelo 14B #
#############

# Para este modelo será testada a interação entre as variáveis: Occupation e Marital_Status | Gender e City_Category com pesos

varfunc.ols14 = lm((model14A$residuals^2) ~. , data = bdfactor)
varfunc14 = varfunc.ols14$fitted.values
peso14a = 1/sqrt((varfunc14))

model14B = lm(formula = log(bdfactor$Purchase) ~ Age + Occupation + Gender +
                City_Category + Marital_Status + Product_Category_1 + 
                Product_Category_2 + Occupation*Marital_Status + Gender*City_Category , data = bdfactor, weights = peso14a)

summary(model14B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model14B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model14B)


#############################################
# Verificação dos pressupostos (Modelo 14B) #
#############################################

residuos_model14B = model14B$residuals

# Pressuposto 1

mean(residuos_model14B)

# Pressuposto 2

bptest(model14B)

# Pressuposto 3

bgtest(model14B)

# Pressuposto 4

jarque.bera.test(residuos_model14B)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model14B, main="Model 14B")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B, model10A, model10B,
    model11A, model11B, model12A, model12B, model13A, model13B, model14A, model14B)



#############
# Modelo 15A #
#############

# Para este modelo será testada a interação entre as variáveis: Occupation e Marital_Status | Gender, City_Category e Age

model15A = lm(formula = log(bdfactor$Purchase) ~ Age + Occupation + Gender +
                City_Category + Marital_Status + Product_Category_1 + 
                Product_Category_2 + Occupation*Marital_Status + Gender*City_Category*Age , data = bdfactor)

summary(model15A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model15A)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model15A)


###########################################
# Verificação dos pressupostos (Modelo 15A) #
###########################################

residuos_model15A = model15A$residuals

# Pressuposto 1

mean(residuos_model15A)

# Pressuposto 2

bptest(model15A)

# Pressuposto 3

bgtest(model15A)

# Pressuposto 4

jarque.bera.test(residuos_model15A)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model15A, main="Model 15A")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B, model10A, model10B,
    model11A, model11B, model12A, model12B, model13A, model13B, model14A, model14B, model15A)


# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica



##############
# Modelo 15B #
##############

# Para este modelo será testada a interação entre as variáveis: Occupation e Marital_Status | Gender, City_Category e Age com pesos

varfunc.ols15 = lm((model15A$residuals^2) ~. , data = bdfactor)
varfunc15 = varfunc.ols15$fitted.values
peso15a = 1/sqrt((varfunc15))


model15B = lm(formula = log(bdfactor$Purchase) ~ Age + Occupation + Gender +
                City_Category + Marital_Status + Product_Category_1 + 
                Product_Category_2 + Occupation*Marital_Status + Gender*City_Category*Age , data = bdfactor, weights = peso15a)

summary(model15B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no AIC
# ols_step_both_aic(model15B)

# escolha do modelo de regressão que melhor se ajusta aos dados com base no p-value
# ols_step_both_p(model15B)


###########################################
# Verificação dos pressupostos (Modelo 15B) #
###########################################

residuos_model15B = model15B$residuals

# Pressuposto 1

mean(residuos_model15B)

# Pressuposto 2

bptest(model15B)

# Pressuposto 3

bgtest(model15B)

# Pressuposto 4

jarque.bera.test(residuos_model15B)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model15B, main="Model 15B")

# Comparação do AIC dos modelos
AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B, model10A, model10B,
    model11A, model11B, model12A, model12B, model13A, model13B, model14A, model14B, model15A, model15B)


# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Não se verifica
# 4º Pressuposto (Normalidade): Não se verifica



###################################################
# Previsão e avaliação da performance dos modelos #
###################################################


############
# Modelo 1 #
############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr1 = predict(model1,bdfactor)
plot(exp(pr1), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual1 = bdfactor$Purchase
prediction1 = exp(model1$fitted.values)
n1 = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE1 = (1/n1) * sum(abs((actual1 - prediction1)/actual1))
MAPE1

############
# Modelo 2 #
############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr2 = predict(model2,bdfactor)
plot(exp(pr2), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual2 = bdfactor$Purchase
prediction2 = exp(model2$fitted.values)
n2 = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE2 = (1/n2) * sum(abs((actual2 - prediction2)/actual2))
MAPE2


############
# Modelo 3 #
############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr3 = predict(model3,bdfactor)
plot(exp(pr3), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual3=bdfactor$Purchase
prediction3 = exp(model3$fitted.values)
n3=length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE3 = (1/n3) * sum(abs((actual3 - prediction3)/actual3))
MAPE3


############
# Modelo 4 #
############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr4 = predict(model4,bdfactor)
plot(exp(pr4), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual4=bdfactor$Purchase
prediction4 = exp(model4$fitted.values)
n4=length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE4 = (1/n4) * sum(abs((actual4 - prediction4)/actual4))
MAPE4


############
# Modelo 5 #
############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr5 = predict(model5,bdfactor)
plot(exp(pr5), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(log(bdfactor$Purchase), pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual5=bdfactor$Purchase
prediction5 = exp(model5$fitted.values)
n5=length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE5 = (1/n5) * sum(abs((actual5 - prediction5)/actual5))
MAPE5


############
# Modelo 6 #
############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr6 = predict(model6,bdfactor)
plot(exp(pr6), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual6=bdfactor$Purchase
prediction6 = exp(model6$fitted.values)
n6=length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE6 = (1/n6) * sum(abs((actual6 - prediction6)/actual6))
MAPE6


############
# Modelo 7 #
############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr7 = predict(model7,bdfactor)
plot(exp(pr7), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual7=bdfactor$Purchase
prediction7 = exp(model7$fitted.values)
n7=length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE7 = (1/n7) * sum(abs((actual7 - prediction7)/actual7))
MAPE7


############
# Modelo 8 #
############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr8 = predict(model8,bdfactor)
plot(exp(pr8), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual8=bdfactor$Purchase
prediction8 = exp(model8$fitted.values)
n8=length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE8 = (1/n8) * sum(abs((actual8 - prediction8)/actual8))
MAPE8


############
# Modelo 9A #
############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr9A = predict(model9A,bdfactor)
plot(exp(pr9A), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual9A=bdfactor$Purchase
prediction9A = exp(model9A$fitted.values)
n9A=length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE9A = (1/n9A) * sum(abs((actual9A - prediction9A)/actual9A))
MAPE9A


############
# Modelo 9B #
############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr9B = predict(model9B,bdfactor)
plot(exp(pr9B), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual9B=bdfactor$Purchase
prediction9B = exp(model9B$fitted.values)
n9B=length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE9B = (1/n9B) * sum(abs((actual9B - prediction9B)/actual9B))
MAPE9B



#############
# Modelo 10A #
#############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr10A = predict(model10A,bdfactor)
plot(exp(pr10A), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual10A=bdfactor$Purchase
prediction10A = exp(model10A$fitted.values)
n10A = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE10A = (1/n10A) * sum(abs((actual10A - prediction10A)/actual10A))
MAPE10A


#############
# Modelo 10B #
#############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr10B = predict(model10B,bdfactor)
plot(exp(pr10B), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual10B=bdfactor$Purchase
prediction10B = exp(model10B$fitted.values)
n10B = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE10B = (1/n10B) * sum(abs((actual10B - prediction10B)/actual10B))
MAPE10B


#############
# Modelo 11A #
#############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr11A = predict(model11A,bdfactor)
plot(exp(pr11A), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual11A=bdfactor$Purchase
prediction11A = exp(model11A$fitted.values)
n11A = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE11A = (1/n11A) * sum(abs((actual11A - prediction11A)/actual11A))
MAPE11A


#############
# Modelo 11B #
#############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr11B = predict(model11B,bdfactor)
plot(exp(pr11B), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual11B=bdfactor$Purchase
prediction11B = exp(model11B$fitted.values)
n11B = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE11B = (1/n11B) * sum(abs((actual11B - prediction11B)/actual11B))
MAPE11B



#############
# Modelo 12A #
#############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr12A = predict(model12A,bdfactor)
plot(exp(pr12A), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual12A=bdfactor$Purchase
prediction12A = exp(model12A$fitted.values)
n12A = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE12A = (1/n12A) * sum(abs((actual12A - prediction12A)/actual12A))
MAPE12A


#############
# Modelo 12B #
#############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr12B = predict(model12B,bdfactor)
plot(exp(pr12B), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual12B=bdfactor$Purchase
prediction12B = exp(model12B$fitted.values)
n12B = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE12B = (1/n12B) * sum(abs((actual12B - prediction12B)/actual12B))
MAPE12B


#############
# Modelo 13A #
#############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr13A = predict(model13A,bdfactor)
plot(exp(pr13A), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual13A=bdfactor$Purchase
prediction13A = exp(model13A$fitted.values)
n13A = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE13A = (1/n13A) * sum(abs((actual13A - prediction13A)/actual13A))
MAPE13A


#############
# Modelo 13B #
#############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr13B = predict(model13B,bdfactor)
plot(exp(pr13B), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual13B=bdfactor$Purchase
prediction13B = exp(model13B$fitted.values)
n13B = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE13B = (1/n13B) * sum(abs((actual13B - prediction13B)/actual13B))
MAPE13B


#############
# Modelo 14A #
#############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr14A = predict(model14A,bdfactor)
plot(exp(pr14A), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual14A=bdfactor$Purchase
prediction14A = exp(model14A$fitted.values)
n14A = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE14A = (1/n14A) * sum(abs((actual14A - prediction14A)/actual14A))
MAPE14A



#############
# Modelo 14B #
#############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr14B = predict(model14B,bdfactor)
plot(exp(pr14B), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual14B=bdfactor$Purchase
prediction14B = exp(model14B$fitted.values)
n14B = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE14B = (1/n14B) * sum(abs((actual14B - prediction14B)/actual14B))
MAPE14B


#############
# Modelo 15A #
#############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr15A = predict(model15A,bdfactor)
plot(exp(pr15A), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual15A=bdfactor$Purchase
prediction15A = exp(model15A$fitted.values)
n15A = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE15A = (1/n15A) * sum(abs((actual15A - prediction15A)/actual15A))
MAPE15A


#############
# Modelo 15B  #
#############

# gráfico comparativo entre o valor verdadeiro e o valor predito e MAPE 
pr15B  = predict(model15B ,bdfactor)
plot(exp(pr15B ), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000))

lines(bdfactor$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual15B =bdfactor$Purchase
prediction15B  = exp(model15B $fitted.values)
n15B  = length(bdfactor$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE15B  = (1/n15B ) * sum(abs((actual15B  - prediction15B )/actual15B ))
MAPE15B 


##########################################
# Conjunto de treino e conjunto de teste #
##########################################


set.seed(7) # É utilizada uma seed para que a separação permaneça igual cada vez que se corre o script
train = sample_frac(bdfactor, 0.9) # separação em conjunto de treino
sample_id = as.numeric(rownames(train)) # linhas correspondentes ao conjunto de treino, são posteriormente subtraídas para dar origem ao de teste
test = bdfactor[-sample_id,] # separação em conjunto de teste

# verificar o número de observações de cada conjunto
nrow(train)
nrow(test)

#############################################################
### 3 Melhores modelos para previsão (Conjunto de treino) ###
#############################################################


##############
# Modelo 12A #
##############


model12A_train = lm(formula = log(train$Purchase) ~ Age + Occupation + Gender +
                      City_Category + Marital_Status + Product_Category_1 + 
                      Product_Category_2 + Occupation*Marital_Status + Age*City_Category , data = train)

summary(model12A_train)

###########################################
# Verificação dos pressupostos (Modelo 12A) #
###########################################

residuos_model12A_train = model12A_train$residuals

# Pressuposto 1

mean(residuos_model12A_train)

# Pressuposto 2

bptest(model12A_train)

# Pressuposto 3

bgtest(model12A_train)

# Pressuposto 4

jarque.bera.test(residuos_model12A_train)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model12A_train, main="Model 12A (Train)")

# Comparação do AIC dos modelos
AIC(model12A_train)


# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Verifica-se
# 4º Pressuposto (Normalidade): Não se verifica


#############
# Modelo 13A #
#############


model13A_train = lm(formula = log(train$Purchase) ~ Age + Occupation + Gender +
                      City_Category + Marital_Status + Product_Category_1 + 
                      Product_Category_2 + Occupation*Gender + Age*City_Category , data = train)

summary(model13A_train)


###########################################
# Verificação dos pressupostos (Modelo 13A) #
###########################################

residuos_model13A_train = model13A_train$residuals

# Pressuposto 1

mean(residuos_model13A_train)

# Pressuposto 2

bptest(model13A_train)

# Pressuposto 3

bgtest(model13A_train)

# Pressuposto 4

jarque.bera.test(residuos_model13A_train)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model13A_train, main="Model 13A (Train)")

# Comparação do AIC dos modelos
AIC(model12A_train, model13A_train)


# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Verifica-se
# 4º Pressuposto (Normalidade): Não se verifica


#############
# Modelo 15A #
#############



model15A_train = lm(formula = log(train$Purchase) ~ Age + Occupation + Gender +
                      City_Category + Marital_Status + Product_Category_1 + 
                      Product_Category_2 + Occupation*Marital_Status + Gender*City_Category*Age , data = train)

summary(model15A_train)


###################################################
# Verificação dos pressupostos (Modelo 15A_train) #
###################################################

residuos_model15A_train = model15A_train$residuals

# Pressuposto 1

mean(residuos_model15A_train)

# Pressuposto 2

bptest(model15A_train)

# Pressuposto 3

bgtest(model15A_train)

# Pressuposto 4

jarque.bera.test(residuos_model15A_train)

###### gráfico dos resíduos

options(repr.plot.width=5, repr.plot.height=5)

par(mfrow=c(2,2)) 
plot(model15A_train, main="Model 15A (Train)")

# Comparação do AIC dos modelos
AIC(model12A_train, model13A_train, model15A_train)


# 1º Pressuposto (Média nula): Verifica-se
# 2º Pressuposto (Variância constante): Não se verifica
# 3º Pressuposto (Ausência de correlação): Verifica-se
# 4º Pressuposto (Normalidade): Não se verifica


######################################################################
### Previsão usando os modelos de treino sobre o conjunto de teste ###
######################################################################

##############
# Modelo 12A #
##############

pred12A_test = predict(model12A_train, test)

plot(exp(pred12A_test), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000),
     main="12A", xlim=c(0,6000))

lines(test$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual12A_test = test$Purchase
prediction12A_test = exp(pred12A_test)
n12A_test = length(test$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE12A_test = (1/n12A_test) * sum(abs((actual12A_test - prediction12A_test)/actual12A_test))
MAPE12A_test


##############
# Modelo 13A #
##############

pred13A_test = predict(model13A_train, test)

plot(exp(pred13A_test), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000),
     main="13A", xlim=c(0,6000))

lines(test$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual13A_test = test$Purchase
prediction13A_test = exp(pred13A_test)
n13A_test = length(test$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE13A_test = (1/n13A_test) * sum(abs((actual13A_test - prediction13A_test)/actual13A_test))
MAPE13A_test


#############
# Modelo 15A #
#############


pred15A_test = predict(model15A_train, test)

plot(exp(pred15A_test), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y", ylim=c(0,25000),
     main="15A", xlim=c(0,6000))

lines(test$Purchase, pch = 18, col = "blue", type = "b", lty = 2, ylim=c(0,25000))

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)

# erro de previsão

actual15A_test = test$Purchase
prediction15A_test = exp(pred15A_test)
n15A_test = length(test$Purchase)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE15A_test = (1/n15A_test) * sum(abs((actual15A_test - prediction15A_test)/actual15A_test))
MAPE15A_test



# Médias dos resíduos de todos os modelos
mediaresiduos = t(data.frame(mean(residuos_model1),mean(residuos_model2),mean(residuos_model3),mean(residuos_model4),
                             mean(residuos_model5),mean(residuos_model6),mean(residuos_model7),mean(residuos_model8),
                             mean(residuos_model9A),mean(residuos_model9B),mean(residuos_model10A), mean(residuos_model10B),
                             mean(residuos_model11A), mean(residuos_model11B), mean(residuos_model12A),mean(residuos_model12B),
                             mean(residuos_model13A),mean(residuos_model13B),mean(residuos_model14A),mean(residuos_model14B),
                             mean(residuos_model15A), mean(residuos_model15B)))

# MAPES de todos os modelos
todosmapes = t(data.frame(MAPE1, MAPE2, MAPE3, MAPE4, MAPE5, MAPE6 ,MAPE7, MAPE8 ,MAPE9A, MAPE9B, MAPE10A, MAPE10B,
                          MAPE11A, MAPE11B, MAPE12A, MAPE12B, MAPE13A, MAPE13B, MAPE14A, MAPE14B, MAPE15A, MAPE15B))

# AICs de todos os modelos
todosaic = AIC(model1, model2, model3, model4, model5, model6, model7, model8, model9A, model9B, 
               model10A, model10B, model11A, model11B, model12A, model12B, model13A, model13B, 
               model14A, model14B, model15A, model15B)

# R-quadrado ajustado de todos os modelos
summaryadjrsquared = data.frame(c(summary(model1)$adj.r.squared,summary(model2)$adj.r.squared,summary(model3)$adj.r.squared,summary(model4)$adj.r.squared,
                                  summary(model6)$adj.r.squared,summary(model7)$adj.r.squared,summary(model8)$adj.r.squared,
                                  summary(model9A)$adj.r.squared,summary(model9B)$adj.r.squared,summary(model10A)$adj.r.squared, summary(model10B)$adj.r.squared,
                                  summary(model11A)$adj.r.squared, summary(model11B)$adj.r.squared, summary(model12A)$adj.r.squared,summary(model12B)$adj.r.squared,
                                  summary(model13A)$adj.r.squared,summary(model13B)$adj.r.squared,summary(model14A)$adj.r.squared,summary(model14B)$adj.r.squared,
                                  summary(model15A)$adj.r.squared, summary(model15B)$adj.r.squared))


linhas = c("model1","model2", "model3", "model4", "model6", "model7", "model8",
           "model9A","model9B", "model10A ", "model10B ", "model11A ",
           "model11B", "model12A", "model12B", "model13A","model13B","model14A", "model14B",
           "model15A", "model15B")

rownames(summaryadjrsquared)= linhas # Alteração do nome das linhas para que a visualização seja mais prática

colnames(summaryadjrsquared)=c("R-quadrado ajustado") # Alteração do nome das colunas para que a visualização seja mais prática

# O modelo 5 não se encontra na tabela summaryadjrsquared uma vez que é utilizada a regressão linear robusta (rlm)

# Mapes de todos os modelos de treino
mapes_treino_e_teste = t(data.frame(MAPE12A_test, MAPE13A_test, MAPE15A_test))

#AICs de todos os modelos de treino
aic_treino_e_teste = AIC(model12A_train, model13A_train, model15A_train)


view(mediaresiduos)
view(todosmapes)
view(todosaic)
view(mapes_treino_e_teste)
view(summaryrsquared)
