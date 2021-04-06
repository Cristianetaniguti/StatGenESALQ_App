df <- read.csv("data_bean.csv")

head(df)
dim(df)

df <- df %>% select(c(1:3,6,8,4)) %>% 
  filter(local %in% c("Local3", "Local2", "Local6")) 

old <- c("Local3", "Local2", "Local6")
new <- c("Local1", "Local2", "Local3")
names(old) <- new

df$local <- factor(names(old)[match(as.character(df$local), old)])

old <- sample(unique(df$gen))
names(old) <- paste0("Gen",1:length(old))

df$gen <- factor(names(old)[match(as.character(df$gen), old)])

colnames(df)[c(4,5,6)] <- c("phen1", "phen2", "phen3")

df <- df[order(df$local, df$gen),]

df$phen1 <- round(df$phen1 + 2 + rnorm(length(df$phen1), mean = 0.05, sd = 0.2),2)
df$phen2 <- round(df$phen2 + 1 + rnorm(length(df$phen2), mean = 0.05, sd = 0.1),2)
df$phen3 <- round(df$phen3 + 6 + rnorm(length(df$phen3), mean = 0.05, sd = 0.4),2)

str(df)
unique(df$local)

write.csv(df, file = "example_blocks.csv", row.names = F)


#################

df <- read.csv("data_corn.csv")

length(unique(df$local))

df <- df %>% select(c(1:4,6,8,7)) 
head(df)
str(df)
unique(df$local)
old <- sample(unique(df$gen))
names(old) <- paste0("Gen",1:length(old))

df$gen <- factor(names(old)[match(as.character(df$gen), old)])


colnames(df)[c(5,6,7)] <- c("phen1", "phen2", "phen3")

df <- df[order(df$local, df$gen),]

df$phen1 <- round(df$phen1 + 11 + rnorm(length(df$phen1), mean = 0.05, sd = 0.2),2)
df$phen2 <- round(df$phen2 + 5 + rnorm(length(df$phen2), mean = 0.05, sd = 0.1),2)
df$phen3 <- round(df$phen3 + 3 + rnorm(length(df$phen3), mean = 0.05, sd = 0.4),2)

write.csv(df, file = "example_lattice.csv", row.names = F)

