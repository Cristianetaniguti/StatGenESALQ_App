# install.packages("agricolae")
# install.packages("emmeans")
library(agricolae)
library(emmeans)
# library(devtools)
# install_github("reyzaguirre/st4gi")
library(st4gi)

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

## Modelo conjunto (tese chefe)
head(df_milho)
df <- df_milho
pheno <- colnames(df)[-c(1:4)]


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
  vg <- (mod_anova$`Mean Sq`[1] - mod_anova$`Mean Sq`[4])/n_rep*n_local
  
  # variancia da interação genotipo x ambiente
  vga <- (mod_anova$'Mean Sq'[4]-mod_anova$'Mean Sq'[6])/n_rep*n_local
  
  # variância fenotipica
  vf <- mod_anova$`Mean Sq`[1]/n_rep*n_local

  # herdabilidade
  h2 <- vg/vf
}

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

### parametros geneticos
### variancia genetica 

vg <- (anova(mod)$'Mean Sq'[2]-anova(mod)$'Mean Sq'[3])/(2*anova(mod)$'Df'[1])
vg


### herdabilidade
h2 <- vg/(vg+anova(mod)$'Mean Sq'[3])
h2


