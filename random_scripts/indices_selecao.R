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
library(StatGenESALQ)

# Dados
setwd("StatGenEsalq_App/inst/ext/example_inputs/")
df_milho <- read.csv("data_corn.csv")
df_feijao <- read.csv("data_bean.csv")

str(df_milho)
str(df_feijao)

# tipo de vetores
df_milho$local <- as.factor(df_milho$local)
df_milho$rep <- as.factor(df_milho$rep)
df_milho$block <- as.factor(df_milho$block)
df_milho$gen <- as.factor(df_milho$gen)

df_feijao <- df_feijao %>% mutate(local = as.factor(local),
                                  gen = as.factor(gen),
                                  block = as.factor(block))

str(df_feijao)

### fenotipo ap

# Com agricolae
## Modelo de alfa látice simples
# Da para fazer análise de estabilidade
model <- PBIB.test(block = df_milho$block, 
                   trt = df_milho$gen, 
                   replication = df_milho$rep, 
                   y = df_milho$ap, k = 12)

model$means$`df_milho$ap` == tapply(df_milho$ap, df_milho$gen, mean)
hist(model$means$`df_milho$ap.adj`)

# desaninhar -- atenção!
str(df_milho) 

unique(df_milho$block)

x <- c("a", "b", "a", "c", "a", "b", "b")
n <- c(apple="a", bananna="b", cherry="c")

n <- c(c(1:12),c(13:24), c(25:36))
new <- c(c(1:12), c(1:12), c(1:12))
names(n) <- new

df_milho$block <- factor(names(n)[match(as.character(df_milho$block), n)])
str(df_milho)

# Individual por ambiente

df <- df_milho %>% filter(local %in% c(1,2))%>% select(c(1:4,5))
head(df)

results_milho <-analysis_fixed_effects(df, design = "lattice", multi_env = T)

df <- df_feijao %>% filter(local %in% c("Local1","Local2"))%>% select(c(1:3,"rend"))
head(df)

results_feijao <-analysis_fixed_effects(df, design = "DBC", multi_env = T)

# Com médias ajustadas
#y = L + G + e
# f = /n_local
# g = /rep

# programas de seleção recorrente

## indice multiplicativo
# Elston 1963
# não necessita estabelecer pesos
# necessidade de transformar para que os fenotipos tenham variancias semelhantes
# sem necessidade de estimar valores genéticos e fenotípicos

increasing <- c("acam", "dec")

elston_index(df = results_milho$adjusted_means, increasing = increasing)

mulamba_index(df_emm, increasing = increasing)

smith_hazel(results_milho$adjusted_means, 
            cvf = results_milho$phenotypic_covariance, 
            cvg = results_milho$genetic_covariance)

elston_index(df = results_feijao$adjusted_means)


