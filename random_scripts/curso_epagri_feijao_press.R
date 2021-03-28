#### pacotes 
install.packages("ggfortify")
install.packages("lmtest")
install.packages("car")
install.packages("psych")
install.packages("ggplot2")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("multtest")
# Dados epagri
### chamada e estrutura dos dados
library(dplyr)

data <- read.table("data_feijao_CA1ES.txt", head=TRUE, sep = "\t", dec = ",", stringsAsFactors = F)
head(data)
str(data)
data$rend <- as.numeric(data$rend)
data$mmg <- as.numeric(data$mmg)
data$bloco <- as.character(data$bloco)
data <- mutate_if(data, is.character, as.factor)
str(data)

#pressuposições do modelo 
### modelo linear 
### aqui também seria possivel colocar uma interação com o ":" entre as variaveis, 
### entretanto temos sempre que ter cuidado na colocação de novos parametros no modelo pela demanda de mais coeficientes a serem estimados, diante disso temos que ter alguma razão teorica para isso.
mod<- lm(np ~ bloco + gen, data=data)
mod2 <- lm(mmg ~ bloco + gen, data=data)


## plotar a anova
Anova <- data.frame(anova(mod))
Anova
summary(mod)
Anova2 <- data.frame(anova(mod2))
Anova2
summary(mod2)
### Analise grafica dos pressupostos do modelo linear de regressão 
## pressupostos da regressão 
### importante ressaltar que as pressuposições do modelo são em relação dos residuos e não em relação a alguma das variaveis 
### grafico de residuos por valores ajustados:
#### avaliação da linearidade do modelo - Caso essa exista no grafico a linha central tera que esta aproximadamente horizontal. 
### Para avaliar aqui a homocedasticidade de variancias teriamos que ter os pontos plotados de forma contante ao longo dos valores previstos de y. È possivel percebe uma distribuição na forma de retangulo e não uma forma triangular.
library(ggplot2)
library(ggfortify)
autoplot(mod, which = 1, data = data,
         colour = "#CC662f", smooth.colour = "#003350") 
autoplot(mod2, which = 1, data = data,
         colour = "#CC662f", smooth.colour = "#003350") 
#### Avaliação se os residuos apresentam distribuição normal - Normal QQplot - Nesse caso temos os residuos padronizados e os residuos teoricos. Para essa condição ser atendida o ideal é que os pontos estejam na linha tracejada
autoplot(mod, which = 2, data = data,
         colour = "#CC662f", smooth.colour = "#003350") 
autoplot(mod2, which = 2, data = data,
         colour = "#CC662f", smooth.colour = "#003350") 
#### No grafico de scale- location podemos avaliar a homocedasticidade ou homogeneidade de variancias  - O ideal aqui é tenhamos uma linha proxima a horizontal e que os pontos plotados formem um padrão retangular
autoplot(mod, which = 3, data = data,
         colour = "#CC662f", smooth.colour = "#003350") 
autoplot(mod2, which = 3, data = data,
         colour = "#CC662f", smooth.colour = "#003350") 
#### o ultimo grafico - Residuals vs Leverage - Podemos analises os outlines ou pomtos influentesou pontos de alavancagem, isso indica um sujeito experimental tão distante que ele influencia o modelo. caso esses existam estarão plotados fora da linha tracejada vermelha. Também é esperado que os valores estejam dentro do intervalo, nesse caso, -3 e 3
autoplot(mod, which = 5, data = data,
         colour = "#CC662f", smooth.colour = "#003350") ### achei esse grafico confuso 
par(mfrow=c(1,1))
plot(mod,5) ### talvez esse seja mais facil de compreender.

autoplot(mod2, which = 5, data = data,
         colour = "#CC662f", smooth.colour = "#003350") 
par(mfrow=c(1,1))
plot(mod2,5,col = "#CC662f")
## testes
### Residuos do modelo precisam apresentar distribuição normal teste shapiro
### H0 = Distribuição dos residuo é normal se p > 0,05
### H1 = Distribuição dos residuo não é normal se p <  0,05
shapiro.test(mod$residuals)
shapiro.test(mod2$residuals)

### Independencia dos residuos 
### Usado para dados longitudinais e que seguem dritribuição normal
### medidas repetidas nas quais os n´ıveis das condi¸c˜oes de avalia¸c˜ao (ou simplesmente “condi¸c˜ao”) n˜ao podem ser aleatorizados entre si (por exemplo, o tempo ou profundidade).
### D-W statistic geralmente usado valores entre 1,5 a 2,5 para indicar que os residuos não estão correlacionados
library(car)
durbinWatsonTest(mod)
durbinWatsonTest(mod2)
### homocedasticidade das variancias 
### Esse teste tem como pressuposição a normalidade dos dados assim como D-W
### H0 = ha homecasticidade das variancias se p > 0,05
### H1 = não ha homecasticidade das variancias se p <  0,05
library(lmtest)
bptest(mod)
bptest(mod2)

# install.packages("multtest")
library(multtest)

##Outliers 
outlier <- function(resid, alpha=0.05){
  # Produce externally studentized residuals
  studresid <- resid/sd(resid, na.rm=TRUE)
  # Calculate adjusted p-values
  rawp.BHStud = 2 * (1 - pnorm(abs(studresid)))
  #Produce a Bonferroni-Holm tests for the adjusted p-values
  #The output is a list
  test.BHStud<-mt.rawp2adjp(rawp.BHStud,proc=c("Holm"),alpha = alpha)
  #Create vectors/matrices out of the list of the BH tests
  adjp = cbind(test.BHStud[[1]][,1])
  bholm = cbind(test.BHStud[[1]][,2])
  index = test.BHStud$index
  # Condition to flag outliers according to the BH test
  out_flag = ifelse(bholm<alpha, "OUTLIER ", ".")
  #Create a matrix with all the output of the BH test
  BHStud_test = cbind(adjp,bholm,index,out_flag)
  #Order the file by index
  BHStud_test2 = BHStud_test[order(index),]
  #Label colums
  names = c("rawp","bholm","index","out_flag")
  colnames(BHStud_test2) <- names
  #Create a final file, with the data and the test and the labels for the outliers
  
  # Take a look at the outliers
  outliers_BH <- as.numeric(BHStud_test2[which(BHStud_test2[,"out_flag"]!="."),"index"])
  if(length(outliers_BH)<1){ cat('No outlier detected \n')}
  if(length(outliers_BH)==1){ cat('1 outlier detected! \n')}
  if(length(outliers_BH)>1){ cat(length(outliers_BH), 'outliers detected! \n')}
  return(outliers_BH)
}

## Aplicação da função aos dados 
(outlier(mod$residuals, alpha = 0.05))
(outlier(mod2$residuals, alpha = 0.05))
### analise de Multicolinearidade
### Indica uma correlação muito alta entre as variedades independentes 
### Indice de Correlação de pearson indica que acima de 0.8 ou 0.9 a depender do autor.
### VIF(valor de inflação calculado) medida de Multicolinearidade Quando esta acima de 10 existe multicolinearidade
library(psych)
pairs.panels(data)
vif(mod)
vif(mod2)
### o grafico mostra em cada intersecção um coeficiente que podemos ver entre as variaveis dependentes 
### Dif muito proximo de 1 indica ausencia de multicolinearidade
### presença de multicolinearidade influencia direto nas estimativas do modelo 

### parametros geneticos
### variancia genetica 

vg <- (anova(mod)$'Mean Sq'[2]-anova(mod)$'Mean Sq'[3])/(2*anova(mod)$'Df'[1])
vg
vg2 <- (anova(mod2)$'Mean Sq'[2]-anova(mod2)$'Mean Sq'[3])/(2*anova(mod2)$'Df'[1])
vg2
### herdabilidade
h2 <- vg/(vg+anova(mod)$'Mean Sq'[3])
h2
h2.2 <- vg2/(vg2+anova(mod2)$'Mean Sq'[3])
h2.2