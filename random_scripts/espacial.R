
# Análise Espacial
# Wheat data from Australia (From Fred van Eeuwijk Course in Brazil, 2019)

# Load
library(SpATS)
library(sommer)

# Load data
dat <- read.table("./data/dataWheat.txt", h = TRUE, sep = "\t")

# Factor
dat$G <- as.factor(dat$geno)
dat$B <- as.factor(dat$rep)

# add row and columns as random factors:
dat$R <- as.factor(dat$row)
dat$C <- as.factor(dat$col)
str(dat)

# Specify the number of segments for the spatial model
nrow <- max(dat$row)
ncol <- max(dat$col)
nseg.row <- nrow/2
nseg.col <- ncol/2

# SpATS analyis, with genotype random:
M1 <- SpATS(response = "yield", 
            genotype = "G", 
            spatial = ~ PSANOVA(col, row, nseg = c(nseg.col, nseg.row)), 
            genotype.as.random = TRUE,
            random = ~ R + C, 
            data = dat,
            control = list(monitoring = 0))
# Plot 
names(M1)
plot(M1)

# Summary
summary(M1)

# Generalized heritability  
getHeritability(M1)

# BLUPS
Blups <- predict(M1, which = "G")
head(Blups)

# Sommer 
S1 <- mmer(yield ~ B,
           random = ~ G + R + C,
           rcov = ~ units,
           data = dat)
summary(S1)$varcomp
h2.fun(S1, data = dat, gTerm = "G")


