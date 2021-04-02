# Aqui faremos os modelos para obtenção das variâncias e covariancias fenotipicas 
# e genotípicas e da herdabilidade das caracteríticas avaliadas em dois experimento: 
# i) De milho em látice quadrado 12x12; 
# ii) De feijão em blocos completos casualizados.
# Os modelos são construídos individualmente por local e conjuntamente
# Após a obtenção das matrizes e das médias ajustadas, as utilizamos para cálculo
# dos índices de seleção: 1) multiplicativo (Elston, 1963)(Subandi et al., 1973);
# 2) com base em soma de postos ou “ranks” (Mulamba e Mock, 1978); 
# 3) clássico de Seleção (Smith (1936) e Hazel (1943)).
# Por fim, calculamos o Ganho de seleção conforme intensidade definida
#
# autores: Cristiane Taniguti e Wellingson Araújo

## Pacotes
# install.packages("agricolae")
# install.packages("emmeans")
# library(devtools)
# install_github("reyzaguirre/st4gi")
library(agricolae)
library(emmeans)
library(metan)

# Dados
setwd("StatGenEsalq_App/inst/ext/example_inputs/")
df_milho <- read.csv("data_corn.csv")
df_feijao <- read.csv("data_bean.csv")

str(df_milho)
str(df_feijao)

# tipo de vetores
df_milho <- df_milho[,-1]
df_milho$local <- as.factor(df_milho$local)
df_milho$rep <- as.factor(df_milho$rep)
df_milho$block <- as.factor(df_milho$block)
df_milho$gen <- as.factor(df_milho$gen)

df_feijao <- df_feijao %>% mutate(local = as.factor(local),
                                  gen = as.factor(gen),
                                  block = as.factor(block))

str(df_feijao)

# inspect

metan::inspect(df_milho, plot = T, threshold = 144)
metan::inspect(df_feijao, plot = T, threshold = 100)

### fenotipo ap

## Modelo de alfa látice simples
# Da para fazer análise de estabilidade
# Com agricolae
model <- PBIB.test(block = df_milho$block, 
                   trt = df_milho$gen, 
                   replication = df_milho$rep, 
                   y = df_milho$ap, k = 12)

model$means$`df_milho$ap` == tapply(df_milho$ap, df_milho$gen, mean)
hist(model$means$`df_milho$ap.adj`)

str(df_milho) # desaninhar -- atenção!

unique(df_milho$block)

x <- c("a", "b", "a", "c", "a", "b", "b")
n <- c(apple="a", bananna="b", cherry="c")

n <- c(c(1:12),c(13:24), c(25:36))
new <- c(c(1:12), c(1:12), c(1:12))
names(n) <- new

df_milho$block <- factor(names(n)[match(as.character(df_milho$block), n)])
str(df_milho)

# Com lm
df_milho <- df_milho %>% filter(local == 1)

mod <- lm(ap ~ rep/block + gen, data = df_milho)
anova(mod)
emm <- emmeans(mod, specs = ~gen)
plot(emm)
test <- print(emm)
adj.mean <- test$emmean

# modelo lm e do agricolae está dando diferente
# Modelo locais individuais e invidual para cada pheno
##' Calculates covariance/variance matrix for DBC and alpha lattice design in
##' single environment
##' 
##' @param design character defining "DBC" do "lattice"
##' @param df data.frame
##' @param multi_env logica TRUE/FALSE indicating multienvironment analysis
##' 
analysis_fixed_effects <- function(df, design, multi_env){
  if(design == "DBC"){
    pheno <- colnames(df)[-c(1:3)]
  } else {
    pheno <- colnames(df)[-c(1:4)]
  }
  vg <- vf <- vga <- h2 <- CV <- vector()
  for(i in 1:length(pheno)){
    temp <- df[,pheno[i]]
    mod <- run_models(temp, df, design, multi_env)

    # médias ajustadas
    emm <- print(emmeans(mod, specs = ~ gen))
    emm_vec <- emm$emmean
    if(i == 1) df_emm <- data.frame(gen = emm$gen, emm_vec) else df_emm <- cbind(df_emm, emm_vec)
    
    mod_anova <- anova(mod)
    
    if(design == "DBC"){
      n_rep <- length(unique(df$block))
    } else {
      n_rep <- length(unique(df$rep))
    }
    
    if(multi_env){
      # variância genetica
      vg[i] <- (mod_anova["df$gen","Mean Sq"] - mod_anova["df$gen:df$local","Mean Sq"])/n_rep*n_local
      
      # variancia da interação genotipo x ambiente
      vga[i] <- (mod_anova["df$gen:df$local",'Mean Sq'] - mod_anova["Residuals",'Mean Sq'])/n_rep*n_local
      
      # variância fenotipica
      vf[i] <- mod_anova["df$gen","Mean Sq"]/n_rep*n_local
      
    } else {
      # variância genetica
      vg[i] <- (mod_anova["df$gen","Mean Sq"] - mod_anova["Residual","Mean Sq"])/n_rep
      
      # variância fenotipica
      vf[i] <- mod_anova["df$gen","Mean Sq"]/n_rep
    }
    
    # not normal distribuition
    if(vg[i] < 0) {
      warning("Negative variances in phenotype ", pheno[which(vg < 0)], ". It can indicat lack of normality in its distribution.")
      vf[i] <- NA
      vg[i] <- NA
      h2[i] <- NA
      CV[i] <- NA
    } else {
      # herdabilidade
      h2[i] <- vg[i]/(vg[i] + ((mod_anova["Residuals",'Mean Sq'])/n_rep))
      
      # CV
      # sqrt(qme)/med*100
      CV[i] <- (sqrt(mod_anova["Residuals",'Mean Sq'])/mean(emm_vec))*100 
    }
  }
  
  # Modelos pheno dois a dois
  combin <- expand.grid(pheno,pheno)
  cvg <- cvf <- v2g <- v2f <- corrg <-  corrf <- vector()
  for(i in 1:dim(combin)[1]){
    temp1 <- df[,as.character(combin[i,1])] 
    temp2 <- df[,as.character(combin[i,2])]
    temp <- temp1 + temp2
    
    mod <- run_models(temp, df, design, multi_env)
    
    mod_anova <- anova(mod)
    if(design == "DBC"){
      n_rep <- length(unique(df$block))
    } else {
      n_rep <- length(unique(df$rep))
    }
    
    if(multi_env){
      # Genetic variance 
      v2g[i] <- (mod_anova["df$gen","Mean Sq"] - mod_anova["df$gen:df$local","Mean Sq"])/n_rep*n_local
      
      # Phenotypic variance
      v2f[i] <- mod_anova["df$gen","Mean Sq"]/n_rep*n_local
      
    } else {
      # variância genetica
      v2g[i] <- (mod_anova["df$gen","Mean Sq"] - mod_anova["Residual","Mean Sq"])/n_rep

      # variância fenotipica
      v2f[i] <- mod_anova["df$gen","Mean Sq"]/n_rep
    }
    
    # Genetic Covariance 
    cvg[i] <- (v2g[i] - vg[combin[i,1]] - vg[combin[i,2]])/2
    
    # Genetic correlation
    corrg[i] <- cvg[i]/sqrt(sqrt(vg[combin[i,1]]*vg[combin[i,2]])^2)
    
    # Phenotypic covariance
    cvf[i] <- (v2f[i] - vf[combin[i,1]] - vf[combin[i,2]])/2
    
    # Phenotypic correlation
    corrf[i] <- cvf[i]/sqrt(vf[combin[i,1]]*vf[combin[i,2]])
  }
  
  cvg <- matrix(cvg, nrow = length(pheno))
  cvf <- matrix(cvf, nrow = length(pheno))
  corrg <- matrix(corrg, nrow = length(pheno))
  corrf <- matrix(corrf, nrow = length(pheno))
  
  colnames(cvg) <- rownames(cvg) <- rownames(corrg) <- rownames(corrf) <-  pheno
  colnames(cvf) <- rownames(cvf) <- colnames(corrg) <- colnames(corrf) <- pheno
  
  # Results
  if(multi_env){
    param <- data.frame(pheno, var_g = vg, var_pheno = vf, h2 = h2, cv = CV, var_gen_env = vga)
  } else {
    param <- data.frame(pheno, var_g = vg, var_pheno = vf, h2 = h2, cv = CV)
  }
  colnames(df_emm) <- c("gen", pheno)
  results <- list(genetic_parameters = param, 
                  adjusted_means = df_emm, 
                  genetic_covariance = cvg, 
                  phenotypic_covariance = cvf, 
                  genetic_correlation = corrg, 
                  phenotypic_correlation = corrf)
  
  return(results)
}


##' Utilities
run_models <-function(pheno, df, design, multi_env){
  if(design == "DBC"){
    if(multi_env){
      mod <- lm(pheno ~ df$gen + df$local/df$block + df$local + df$gen*df$local)
    } else {
      mod <- lm(pheno ~ df$gen + df$block)
    }
  } else {
    if(multi_env){
      mod <- lm(pheno ~ df$gen + df$local/df$rep/df$block + 
                  df$local/df$rep + df$local + df$gen*df$local)
    } else {
      mod <- lm(pheno ~ df$gen + df$rep/df$block)
    }
  }
  return(mod)
}

#y = L + G + e
# f = /n_local
# g = /rep

# programas de seleção recorrente

## indice multiplicativo
# Elston 1963
# não necessita estabelecer pesos
# necessidade de transformar para que os fenotipos tenham variancias semelhantes
# sem necessidade de estimar valores genéticos e fenotípicos

##' Elston selection index
##' 
##' @param df data.frame with first column called 'gen' with genotypes identification and follow columns with adjusted mean for each phenotype. 
##' @param k vector with minimum or maximum value for each phenotype evaluated. If NULL the minimum/maximum value is considered.
##' @param increasing vector with column names of phenotypes that low value is advantageous
##' 
elston_index <- function(df, k=NULL, increasing=NULL){
  gen <- df[,1]
  df <- df[,-1]
  pheno <- colnames(df)
  
  # define k
  if(is.null(k)){
    for(i in 1:length(pheno)){
      if(pheno[i] %in% increasing){
        k[i] <- max(df[,i]) # worse value
      } else {
        k[i] <- min(df[,i])
      }
    }
  }
  
  df_elson <- vector()
  for(i in 1:length(pheno)){
    if(pheno[i] %in% increasing){
      df_elson <- c(df_elson, k[i] - df[,i]) 
    } else {
      df_elson <- c(df_elson, df[,i] - k[i])
    }
  }
  
  df_elson[which(df_elson < 0)] <- 0
  df_elson <- matrix(df_elson, ncol = length(pheno), byrow = F)
  colnames(df_elson) <- pheno
  
  df_index <- data.frame(gen, elson_idx = apply(df_elson, 1, prod)) 
  
  return(df_index)
}

elston <- elston_index(df_emm, increasing = c("acam", "dec"))


# Indice de soma de classificação
# Mulamba e Mock
# também não há necessidade de calcular parâmetros genéticos

##' Mulamba and Mock (1978) selection index
##' 
##' @param df data.frame with first column called 'gen' with genotypes identification and follow columns with adjusted mean for each phenotype. 
##' @param increasing vector with column names of phenotypes that low value is advantageous
##' @param weights vector of economic weight for each phenotype. If df has 8 columns ('gen' + 7 phenotypes), this vector should have length 7.
mulamba_index <- function(df, increasing = NULL, weights = NULL){
  
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
mulamba_index(df_emm, increasing = increasing)

##' Smith Hazel index
##' 
##' Is = wYik + wYij
##' 
##' @param adj.means data.frame with first column as the genotypes 
##' names and following columns the adjusted means for each phenotype
##' 
##' @param cvf phenotypic covariance matrix
##' @param cvg genotypic covariance matrix
##' @param weights vector with weight of each phenotype
##' 
smith_hazel <- function(adj.means, cvf, cvg, weights){
  if(is.null(weights)) weights <- rep(1,dim(adj.means)[2]-1) 
  w <- solve(cvf)%*%cvg%*%weights
  Is = as.matrix(adj.means[,-1])%*%w
  smith_df <- data.frame(gen = adj.means[,1], Is)
  head(smith_df)
}

##' Calculates selection gain
##' 
##' @param herdability number
##' @param pheno data.frame with first column as the genotypes 
##' names and following columns the adjusted means for the evaluated phenotype
##' @param selected_ind individuals selected
##' 
selection_gain <- function(pheno, selected_ind, herdability){
  selected <- pheno[which(pheno[,1] %in% selected_ind),]
  SD <- mean(selected[,2]) - mean(pheno[,2])
  SG <- herdability*SD
}
