
# MET Feijão

# Library
library(dplyr)
library(metan)

# Load data
dat <- read.table("./dados/dados_feijao.txt", header = TRUE, sep = "\t", dec = ",")
head(dat)

# Defining Factors 
dat$R <- dat$bloco %>% as.factor        # Rep
dat$L <- dat$local %>% as.factor        # Environment
dat$G <- dat$gen %>% as.factor          # Genotype
str(dat)

# NA
sum(is.na(dat$rend))
tapply(dat$rend, dat$G, mean, na.rm = TRUE)
table(dat$G, dat$L)

# Remove 2 locations
dat <- droplevels(dat[-c(which(dat$L %in% c("PS1ES", "UR2ES"))),])

# Plot
ge_plot(dat, gen = G, env = L, resp = rend, type = 1)
ge_plot(dat, gen = G, env = L, resp = rend, type = 2)

# Annicchiarico
Ann <- Annicchiarico(dat,
                     env = L,
                     gen = G,
                     rep = R,
                     resp = rend,
                     prob = 0.25)

Ann$rend$environments
Ann$rend$general
Ann$rend$favorable
Ann$rend$unfavorable

# Shukla
Shuk <- Shukla(dat,
              env = L,
              gen = G,
              rep = R,
              resp = rend)
Shuk$rend$ShuklaVar
Shuk$rend$GEN
Shuk$rend$rMean
Shuk$rend$rShukaVar
  
# Joint regression analysis - Eberhart and Russell (1966)
jra <- ge_reg(dat, env = L, gen = G, rep = R, resp = rend)
plot(jra, type = 1)

# AMMI
Ammi <- performs_ammi(dat, env = L, gen = G, rep = R, resp = rend)

# PC1 x PC2 (variable GY)
p1 <- plot_scores(Ammi)
p1

# PC1 x PC2 (variable HM)
plot_scores(Ammi,
            var = 1, # or "HM"
            type = 2)

# ecovalence (Wricke, 1965)
eco <- ecovalence(dat,
                  env = L,
                  gen = G,
                  rep = R,
                  resp = rend)
eco


