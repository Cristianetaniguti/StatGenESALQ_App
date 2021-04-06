

# MET Analysis

# Library
library(dplyr)
library(metan)
library(emmeans)
library(ggplot2)

# Load data
dat <- read.csv("./data/feijao.csv", header = TRUE, sep = ",")
str(dat)

# Defining Factors 
dat$R <- dat$bloco %>% as.factor        # Rep
dat$L <- dat$local %>% as.factor        # Environment
dat$G <- dat$gen %>% as.factor          # Genotype
str(dat)

# NA
sum(is.na(dat$rend))
tapply(dat$rend, dat$G, mean, na.rm = TRUE)
table(dat$G, dat$L)

# Plot
ge_plot(dat, gen = G, env = L, resp = rend, type = 1)
ge_plot(dat, gen = G, env = L, resp = rend, type = 2)

# "Eskridge's Safety-first index"
# Get adjusted means per location
trait <- parse(text = "rend") # Select trait

# @dat 
# @trait Random variable 
# @model DBC or lattice
safety_first <- function(dat, trait, model, alpha = 0.95) {
  Envs <- levels(dat$L)
  results <- vector("list", length(Envs))
  # Loops Envs
  for (i in Envs) {
    Edat <- droplevels(subset(dat, L == i))
    if (model == "DBC") {
      # Model
      mod <- aov(eval(trait) ~ G + R,
                 data = Edat)
    } else {
      mod <- aov(eval(trait) ~ G + R + B,
                 data = Edat)
    }
    temp = data.frame(lsmeans(mod, ~ G))
    results[[i]] <- data.frame(
      G = temp$G,
      trait = temp$lsmean,
      L = factor(levels(Edat$L)),
      L_mean = mean(temp$lsmean)
    )
    rm(Edat, mod, temp)
  }
  # Adj. means
  StageI <- do.call(rbind, results)
  colnames(StageI)[2] <- trait
  StageI_H <-
    matrix(StageI$rend, nlevels(dat$G), nlevels(dat$L)) # n_g x n_l
  # Yi. - Z(1 - \alpha)(Vi)^0.5
  Vii <- apply(StageI_H, 1, sd)
  Yi. <- apply(StageI_H, 1, mean)
  # Z tab
  Z <- qnorm(p = alpha)
  # Safety-first index
  Risk_F <- round(Yi. - (Z * Vii), 2)
  Risk_F <- data.frame(Risk_F, ID = levels(dat$G))
  Risk_F <- Risk_F[order(Risk_F$Risk_F, decreasing = TRUE),]
  return(Risk_F)
}
Risk_F <- safety_first(dat, trait, model = "DBC")

# Changing order of the levels
Risk_F$ID = factor(Risk_F$ID, levels = Risk_F$ID)
x_lab <- "Linhagens"
y_lab <- "Safety-first index"

# Plot 
(P1 = ggplot(Risk_F, aes(x = ID, y = Risk_F)) +
    geom_segment( aes(x = ID, xend = ID, y = 0, yend = Risk_F), size = 1) +
    geom_point(size = 2, color = "red", fill = alpha("orange", 0.3), 
               alpha = 0.7, shape = 21, stroke = 0.4) +
    theme_bw() +
    theme(axis.title.x = element_text(size = 11, face = "bold", color = "black"),
          axis.title.y = element_text(size = 11, face = "bold", color = "black"), 
          axis.text.x = element_text(size = 6, face = "bold", color = "black", angle = 90),
          axis.text.y = element_text(size = 7, face = "bold", color = "black")) + 
    theme(plot.title = element_text(size = 11, hjust = 0.5, face = "bold")) +
    labs(x = x_lab, y = y_lab))


# Annicchiarico
Ann <- Annicchiarico(dat,
                     env = L,
                     gen = G,
                     rep = R,
                     resp = rend,
                     prob = 0.25)

# Favorable Environments
Ann$rend$environments

# Mean per Env
sort(tapply(dat$rend, dat$L, mean))
Index = round(tapply(dat$rend, dat$L, mean) - mean(dat$rend), 2)
Index

# General
Ann$rend$general[order(Ann$rend$general$Wi, decreasing = TRUE),]

# Pij = Yij/Y.j X 100
envs <- levels(dat$L)
Pij <- vector("list", length(envs))
# loop
for(i in 1:length(envs)){
  edat = droplevels(subset(dat, L == envs[i]))
  mean = mean(edat$rend)
  Pij[[i]] = round((tapply(edat$rend, edat$G, mean)/mean)*100,0)
}

# Pij
Pij = t(do.call(rbind,Pij))
sort(round(rowMeans(Pij), 1) , decreasing = TRUE)[1:5]
Ann$rend$general[order(Ann$rend$general$Mean_rp, decreasing = TRUE),][1:5,] 

# SD
sort(apply(Pij, 1, sd), decreasing = TRUE)

# Reliability index 
# Ii = pi. - ZxSi  
Z = qnorm(p = 0.75)
(I = round(apply(Pij, 1, mean) - (apply(Pij, 1, sd)*Z), 0))

# Shukla
Shuk <- Shukla(dat,
               env = L,
               gen = G,
               rep = R,
               resp = rend)
Shuk$rend

# Joint regression analysis - Eberhart and Russell (1966)
jra <- ge_reg(dat, env = L, gen = G, rep = R, resp = rend)
plot(jra, type = 1)






