
# Mixed models

# Library
library(sommer)
library(dplyr)

install.packages("doBy")
install.packages("regress")
install.packages("igraph")
install.packages("qtl")
BiocManager::install("snpStats")
install.packages("LDheatmap")

install.packages("synbreed",repos="http://r-forge.r-project.org", dependencies = T)
library(synbreed)

library(AGHmatrix)
library(emmeans)

# Load data
dat <- read.table("./data/dados_feijao.txt", header = TRUE, sep = "\t", dec = ",")
head(dat)

# Defining Factors 
dat$R <- dat$bloco %>% as.factor        # Rep
dat$L <- dat$local %>% as.factor        # Environment
dat$G <- dat$gen %>% as.factor          # Genotype
str(dat)

# Compound simmetry (CS) model
ans1 <- mmer(rend ~ L,
             random = ~ G + L:R + L:G,
             rcov = ~ units,
             data = dat)
summary(ans1)$varcomp

# Het R
ans1b <- mmer(rend ~ L,
              random = ~ G + L:R + L:G,
              rcov = ~ vs(ds(L),units),
              data = dat)

summary(ans1b)$varcomp

# Compound simmetry (CS) + Diagonal (DIAG) model
ans2 <- mmer(rend ~ L,
             random = ~ G + L:R + vs(ds(L), G),
             #rcov = ~ units,
             rcov= ~ vs(ds(L),units),
             data = dat)
summary(ans2)$varcomp

# unstructured variance
ans3 <- mmer(rend ~ L,
             random = ~ L:R + vs(us(L), G),
             rcov = ~ units,
             data = dat)
summary(ans3)$varcomp

# AIC
ans1$AIC
ans1b$AIC
ans2$AIC
ans3$AIC

# Get BLUPS
names(ans1)
ans1$U$G


# Simular Pedigree
ped <- simul.pedigree(gener = 4, ids = c(3,4,6,7))
par(mfrow = c(1,1))
plot(ped)

# A Matrix
A <- Amatrix(data = ped[,1:3], ploidy = 2)/2
hist(A)
rownames(A) <- levels(dat$G)
colnames(A) <- levels(dat$G)
A[1:4, 1:4]

# Model plus A Matrix
ans1_G <- mmer(rend ~ L,
               random = ~ vs(G, Gu = A) + L:R,
               rcov = ~ units,
               data = dat)

summary(ans1_G)$varcomp
summary(ans1)$varcomp

# AIC
ans1$AIC
ans1_G$AIC


# Exercício fazer BLUP e GS
levels(dat$L)
edat <- droplevels(subset(dat, L == "CA1ES"))

# Model Fixed
ans1_F <- aov(rend ~ G + R,
              data = edat)
anova(ans1_F)

Adjs <- data.frame(lsmeans(ans1_F, ~ G))
Adjs[order(Adjs$lsmean, decreasing = TRUE),][1,c(1,2)]

ds <- Adjs[order(Adjs$lsmean, decreasing = TRUE),][1,2] - mean(Adjs$lsmean, na.rm = TRUE) 
ds

Vg <- (anova(ans1_F)["G","Mean Sq"] - anova(ans1_F)["Residuals","Mean Sq"])/4
Ve <- anova(ans1_F)["Residuals","Mean Sq"]
h2 <- Vg/ (Vg + Ve/4)
h2

# Ganho
Adjs[order(Adjs$lsmean, decreasing = TRUE),][1,c(1,2)]
ds*h2

# Model Random
ans1_R <- mmer(rend ~ R,
               random = ~ G,
               rcov = ~ units,
               data = edat)

# BLUP
ans1_R$U$G$rend


