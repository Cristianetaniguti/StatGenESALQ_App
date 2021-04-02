######### transformation ############
install.packages("lsmeans")
install.packages("emmeans")
# pacotes 
library(ggplot2)
library(ggfortify)
library(lsmeans)

#data
data1 <- read.csv("data_bean.csv")
head(data1)
str(data1)
data1$local <- as.factor(data1$local)
data1$gen <-as.factor(data1$gen)
data1$block <- as.factor(data1$block)
data1$bact <- as.numeric(data1$bact)
data1$antfo <- as.numeric(data1$antfo)
data1$antva <- as.numeric(data1$antva)
data1$local <- as.factor(data1$local)
data1$gen <-as.factor(data1$gen)
data1$block <- as.factor(data1$block)
data1$bact <- as.numeric(data1$bact)
data1$antfo <- as.numeric(data1$antfo)
data1$antva <- as.numeric(data1$antva)
data1$manfo <- as.numeric(data1$manfo)
data1$np <- as.numeric(data1$np)
data1$manva <- as.numeric(data1$manva)
data1$rend <- as.numeric(data1$rend)
data1$mmg <- as.numeric(data1$mmg)
data1$def <- as.numeric(data1$def)
str(data1)
mod1 <- lm(np~block:local + gen + local + gen:local, data=data1)
mod2<- lm(log(np)~block:local + gen + local + gen:local, data=data1)
mod3<- lm(sqrt(np + 0.5)~block:local + gen + local + gen:local, data=data1)

###### para o modelo 4 a transformação de box cox requer passos adicionais
df <- data1
library(MASS)
boxcox(np~block:local + gen + local + gen:local, data=data1, plotit=F, lam=seq(-3, 3, 1/20))
bc<-boxcox(np~block:local + gen + local + gen:local, data=data1, plotit=F, lam=seq(-3, 3, 1/20))
(lambda = bc$x[which.max(bc$y)])
np_trans <- ((data1$np^lambda-1)/lambda)
data_trans <- data.frame(np_trans , local = data1$local ,gen = data1$gen , block = data1$block)

mod4 <- lm(np_trans~block:local + gen + local + gen:local, data=data_trans)
###graficos 
## pressupostos da regressão 
### importante ressaltar que as pressuposições do modelo são em relação dos residuos e não em relação a alguma das variaveis 
### grafico de residuos por valores ajustados:
#### avaliação da linearidade do modelo - Caso essa exista no grafico a linha central tera que esta aproximadamente horizontal. 
### Para avaliar aqui a homocedasticidade de variancias teriamos que ter os pontos plotados de forma contante ao longo dos valores previstos de y. È possivel percebe uma distribuição na forma de retangulo e não uma forma triangular.
autoplot(mod1, which = 1, data = data1,
         colour = "#CC662f", smooth.colour = "#003350") 
autoplot(mod2, which = 1, data = data1,
         colour = "#CC662f", smooth.colour = "#003350") 
autoplot(mod3, which = 1, data = data1,
         colour = "#CC662f", smooth.colour = "#003350") 
autoplot(mod4, which = 1, data = data1,
         colour = "#CC662f", smooth.colour = "#003350") 

#### Avaliação se os residuos apresentam distribuição normal - Normal QQplot - Nesse caso temos os residuos padronizados e os residuos teoricos. Para essa condição ser atendida o ideal é que os pontos estejam na linha tracejada
autoplot(mod, which = 2, data = data1,
         colour = "#CC662f", smooth.colour = "#003350") 
autoplot(mod2, which = 2, data = data1,
         colour = "#CC662f" , smooth.colour = "#003350") 
autoplot(mod3, which = 2, data = data1,
         colour = "#CC662f" , smooth.colour = "#003350") 
autoplot(mod4, which = 2, data = data1,
         colour = "#CC662f" , smooth.colour = "#003350") 
#### No grafico de scale- location podemos avaliar a homocedasticidade ou homogeneidade de variancias  - O ideal aqui é tenhamos uma linha proxima a horizontal e que os pontos plotados formem um padrão retangular
autoplot(mod1, which = 3, data = data,
         colour = "#CC662f", smooth.colour = "#003350") 
autoplot(mod2, which = 3, data = data,
         colour = "#CC662f", smooth.colour = "#003350") 
utoplot(mod3, which = 3, data = data,
        colour = "#CC662f", smooth.colour = "#003350") 
utoplot(mod4, which = 3, data = data,
        colour = "#CC662f", smooth.colour = "#003350") 
#### o ultimo grafico - Residuals vs Leverage - Podemos analises os outlines ou pomtos influentesou pontos de alavancagem, isso indica um sujeito experimental tão distante que ele influencia o modelo. caso esses existam estarão plotados fora da linha tracejada vermelha. Também é esperado que os valores estejam dentro do intervalo, nesse caso, -3 e 3

plot(mod1,5,col = "#CC662f",pch = 19) ## não consegui trocar a linha do centro de vermelha para azul :(
plot(mod2,5,col = "#CC662f",pch = 19)
plot(mod3,5,col = "#CC662f",pch = 19)
plot(mod4,5,col = "#CC662f",pch = 19)
### histograma dos erros do modelo, facilita a vizualização da distribuição normal 
ggplot(mod1, aes(x = mod1$residuals)) +
  geom_histogram(bins = 11, colour = "black", fill = "#CC662f", ) +
  labs(title = "Histogram of Residuals",x = "Residuals", y = "Frequency")
ggplot(mod2, aes(x = mod1$residuals)) +
  geom_histogram(bins = 11, colour = "black", fill = "#CC662f", ) +
  labs(title = "Histogram of Residuals",x = "Residuals", y = "Frequency")
ggplot(mod3, aes(x = mod1$residuals)) +
  geom_histogram(bins = 11, colour = "black", fill = "#CC662f", ) +
  labs(title = "Histogram of Residuals",x = "Residuals", y = "Frequency")
ggplot(mod4, aes(x = mod1$residuals)) +
  geom_histogram(bins = 11, colour = "black", fill = "#CC662f", ) +
  labs(title = "Histogram of Residuals",x = "Residuals", y = "Frequency")

## testes
### Residuos do modelo precisam apresentar distribuição normal teste shapiro
### H0 = Distribuição dos residuo é normal se p > 0,05
### H1 = Distribuição dos residuo não é normal se p <  0,05
shapiro.test(mod1$residuals)
shapiro.test(mod2$residuals)
shapiro.test(mod3$residuals)
shapiro.test(mod4$residuals)
### Independencia dos residuos 
### Usado para dados longitudinais e que seguem dritribuição normal
### medidas repetidas nas quais os n´ıveis das condi¸c˜oes de avalia¸c˜ao (ou simplesmente “condi¸c˜ao”) n˜ao podem ser aleatorizados entre si (por exemplo, o tempo ou profundidade).
### D-W statistic geralmente usado valores entre 1,5 a 2,5 para indicar que os residuos não estão correlacionados
library(car)
durbinWatsonTest(mod)
durbinWatsonTest(mod2)
durbinWatsonTest(mod3)
durbinWatsonTest(mod4)
### homocedasticidade das variancias 
### Esse teste tem como pressuposição a normalidade dos dados assim como D-W
### H0 = ha homecasticidade das variancias se p > 0,05
### H1 = não ha homecasticidade das variancias se p <  0,05
library(lmtest)
bptest(mod)
bptest(mod2)
bptest(mod3)
bptest(mod4)
##Outliers 
outlier <- function(resid, alpha=0.05){
  library(multtest)
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
(outlier(mod3$residuals, alpha = 0.05))
(outlier(mod4$residuals, alpha = 0.05))
### analise de Multicolinearidade
### Indica uma correlação muito alta entre as variedades independentes 
### Indice de Correlação de pearson indica que acima de 0.8 ou 0.9 a depender do autor.
### VIF(valor de inflação calculado) medida de Multicolinearidade Quando esta acima de 10 existe multicolinearidade
library(psych)
pairs.panels(data)
vif(mod)
vif(mod2)
vif(mod3)
vif(mod4)
### o grafico mostra em cada intersecção um coeficiente que podemos ver entre as variaveis dependentes 
### Dif muito proximo de 1 indica ausencia de multicolinearidade
### presença de multicolinearidade influencia direto nas estimativas do modelo 







####rend

mod5 <- lm(rend~block:local + gen + local + gen:local, data=data1)
mod6<- lm(log(rend)~block:local + gen + local + gen:local, data=data1)
mod7<- lm(sqrt(np + 0.5)~block:local + gen + local + gen:local, data=data1)
library(MASS)
boxcox(rend~block:local + gen + local + gen:local, data=data1, plotit=T, lam=seq(-3, 3, 1/20))
bc<-boxcox(rend~block:local + gen + local + gen:local, data=data1, plotit=T, lam=seq(-3, 3, 1/20))
(lambda = bc$x[which.max(bc$y)])
rend_trans <- ((data1$rend^lambda-1)/lambda)

data_trans <- data.frame(rend_trans , local = data1$local ,gen = data1$gen , block = data1$block)
mod8 <- lm(rend_trans~block:local + gen + local + gen:local, data=data_trans)

#### medias estimadas 
require(lsmeans)
require(emmeans)
(rend_np <- lsmeans(mod4,"gen"))
(rend_lsm <- lsmeans(mod8,"gen"))
