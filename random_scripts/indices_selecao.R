# install.packages("agricolae")
# install.packages("emmeans")
# library(devtools)
# install_github("reyzaguirre/st4gi")
library(agricolae)
library(emmeans)
library(st4gi)
library(metan)

setwd("StatGenEsalq_App/inst/ext/example_inputs/")
df_milho <- read.csv("data_corn.csv")

head(df_milho)
str(df_milho)

df_milho <- df_milho[,-1]
df_milho$local <- as.factor(df_milho$local)
df_milho$rep <- as.factor(df_milho$rep)
df_milho$block <- as.factor(df_milho$block)
df_milho$gen <- as.factor(df_milho$gen)

### fenotipo ap

## Modelo de alfa látice simples
# Da para fazer análise de estabilidade
# Com agricolae
model <- PBIB.test(block = df_milho$block, 
                   trt = df_milho$gen, 
                   replication = df_milho$rep, 
                   y = df_milho$ap, k = 3)
model$ANOVA
model$means$`df_milho$ap` == tapply(df_milho$ap, df_milho$gen, mean)
hist(model$means$`df_milho$ap.adj`)

# Com lm
mod <- lm(ap ~ rep/block + rep + gen, data = df_milho)
anova(mod)
emm <- emmeans(mod, specs = ~gen)
plot(emm)
test <- print(emm)
adj.mean <- test$emmean

new.df <- data.frame(gen = test$gen, adj.mean = test$emmean, local)
length(unique(df_milho$gen))
# modelo lm e do agricolae está dando diferente

## Modelo conjunto - todos os ambientes (tese chefe)
head(df_milho)
df <- df_milho
pheno <- colnames(df)[-c(1:4)]

# Modelo invidual para cada pheno
vg <- vf <- vga <- h2 <- vector()
for(i in 1:length(pheno)){
  temp <- df[,pheno[i]]
  mod <- lm(temp ~ df$gen + df$local/df$rep/df$block + 
               df$local/df$rep + df$local + df$gen*df$local)
  # médias ajustadas
  emm <- print(emmeans(mod, specs = ~ gen))
  emm_vec <- emm$emmean
  if(i == 1) df_emm <- data.frame(gen = emm$gen, emm_vec) else df_emm <- cbind(df_emm, emm_vec)

  mod_anova <- anova(mod)
  n_rep <- length(unique(df$rep))
  n_local <- length(unique(df$local))
  n_block <- length(unique(df$block))
  
  # variância genetica
  vg[i] <- (mod_anova$`Mean Sq`[1] - mod_anova$`Mean Sq`[4])/n_rep*n_local
  
  # variancia da interação genotipo x ambiente
  vga[i] <- (mod_anova$'Mean Sq'[4]-mod_anova$'Mean Sq'[6])/n_rep*n_local
  
  # variância fenotipica
  vf[i] <- mod_anova$`Mean Sq`[1]/n_rep*n_local

  # herdabilidade
  h2[i] <- vg[i]/vf[i]
}

names(vg) <- pheno

# Modelos pheno dois a dois
combin <- expand.grid(pheno,pheno)

cvg <- cvf <- v2g <- v2f <- vector()
for(i in 1:dim(combin)[1]){
  temp1 <- df[,as.character(combin[i,1])] 
  temp2 <- df[,as.character(combin[i,2])]
  temp <- temp1 + temp2
  mod <- lm(temp ~ df$gen + df$local/df$rep/df$block + 
              df$local/df$rep + df$local + df$gen*df$local)
  
  mod_anova <- anova(mod)
  n_rep <- length(unique(df$rep))
  n_local <- length(unique(df$local))
  
  # variância genetica
  v2g[i] <- (mod_anova$`Mean Sq`[1] - mod_anova$`Mean Sq`[4])/n_rep*n_local
  cvg[i] <- (v2g[i] - vg[combin[i,1]] - vg[combin[i,2]])/2
  
  # variância fenotipica
  v2f[i] <- mod_anova$`Mean Sq`[1]/n_rep*n_local
  cvf[i] <- (v2f[i] - vf[combin[i,1]] - vf[combin[i,2]])/2
}

cvg <- matrix(cvg, nrow = length(pheno))
cvf <- matrix(cvf, nrow = length(pheno))

colnames(cvg) <- rownames(cvg) <- pheno
colnames(cvf) <- rownames(cvf) <- pheno

# replacing diagonal with variances from individual models
# Não precisa, pq da o mesmo resultado
# for(i in 1:length(pheno)){
#   for(j in 1:length(pheno)){
#     if(i == j){
#       cvg[i,j] <- vg[i]
#       cvf[i,j] <- vf[i]
#     }
#   }
# }

colnames(df_emm) <- c("gen", pheno)
head(df_emm)

# DL conferidos na tese do chefe
anova(mod)
summary(mod)

# programas de seleção recorrente

## indice multiplicativo
# Elston 1963
# não necessita estabelecer pesos
# necessidade de transformar para que os fenotipos tenham variancias semelhantes
# sem necessidade de estimar valores genéticos e fenotípicos
df_emm_elston <- df_emm
df_emm_elston[,"acam"] <- df_emm[,"acam"]*-1
df_emm_elston[,"dec"] <- df_emm[,"dec"]*-1
hist(df_emm_elston[,2])
hist(df_emm_elston[,3])
hist(df_emm_elston[,4])
hist(df_emm_elston[,5])
hist(df_emm_elston[,6])
hist(df_emm_elston[,7])

index_df <- elston(c("ap", "ae", "acam", "prolif", "dec", "prod"), geno = "gen", dfr = df_emm_elston)

# Indice de soma de classificação
# Mulamba e Mock
# também não há necessidade de calcular parâmetros genéticos

##' Mulamba and Mock (1978) selection index
##' 
##' @param df data.frame with first column called 'gen' with genotypes identification and follow columns with adjusted mean for each phenotype. 
##' @param increasing vector with column names of phenotypes that low value is advantageous
##' @param weights vector of economic weight for each phenotype. If df has 8 columns ('gen' + 7 phenotypes), this vector should have length 7.
mulamba_index <- function(df, increasing = NULL, weights = NULL){
  pheno <- colnames(df)[-1]
  if(is.null(weights)) weights <- rep(1,length(pheno))
  
  decreasing <- pheno[!pheno %in% increasing]
  
  # 1 receives best value (higher value)
  for(i in decreasing)
    df[order(df[,i], decreasing = T),i] <- 1:dim(df)[1]
  
  if(!is.null(increasing)){
    # 1 receives best value (low value)
    for(i in increasing)
      df[order(df[,i], decreasing = F),i] <- 1:dim(df)[1]
  }
  
  ranks <- t(t(df[,-1])*weights)
  mulamba_sum <- ranks %>% as.data.frame %>% mutate_if(is.character, as.numeric) %>% rowSums 
  df_mulamba <- data.frame(gen=as.character(df[,1]),sum = mulamba_sum)
  df_mulamba <- df_mulamba[order(df_mulamba$sum, decreasing = T),]
  
  return(df_mulamba)
}

increasing <- c("acam", "dec")
weights <- rep(1, length(pheno))
mulamba_index(df_emm, increasing = increasing)

## dando pau
smith <- Smith_Hazel(df_emm[,-1], use_data = "pheno", pcov = round(cvf,3), gcov = round(cvg,3), weights = rep(1, length(pheno)))

## ganho de seleção