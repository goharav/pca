consumption <- read.csv('consumption.csv')
str(consumption)
summary(consumption)
data.frame(mean=sapply(consumption, mean), sd=sapply(consumption, sd),
           min=sapply(consumption, min), max=sapply(consumption, max),
           median=sapply(consumption, median))

## rice and macaroni are correlated
library(ggplot2)
ggplot(consumption, aes(x=bread_3_rice.kg, y=bread_5_macaroni.kg))+
  geom_point(color='blue', size=1.5) + geom_smooth(method='lm', se=F, size=1.5, col='red')+
  ggtitle('First Principal Component')

cor(consumption$bread_3_rice.kg, consumption$bread_5_macaroni.kg)

p_comp <- prcomp(consumption[,c('bread_3_rice.kg', 'bread_5_macaroni.kg')])
names(p_comp)
head(p_comp$x)
summary(p_comp)

ggplot(as.data.frame(p_comp$x), aes(x=PC1, y=PC2)) + geom_point(color='blue')+
  xlim(-5, 10) + ylim(-5, 5)+
  theme(axis.title = element_text(size=20))
d1 <- data.frame(consumption[,c("bread_3_rice.kg", "bread_5_macaroni.kg")], p_comp$x)
head(d1)
summary(d1)
# PC1 and PC2 means are zero, because of scalling
#new
# rotation
p_comp$rotation
head(d1)
## correlation matrix
cor(d1)
cov(d1)

## pca for all bread products
colnames(consumption)
p_comp <- prcomp(consumption, scale=T)
summary(p_comp)
## taking first 2 components
screeplot(p_comp, type='l')
## matrici sepakan arjeqnery, lamdanery
p_comp$sdev^2
## First 3 components together explain the biggest part of the variance (67%), we can keep
##them, however eigenvalues suggest that 2 components are enough (eigenvalues > 1)

# percentage of explained variances
install.packages('factoextra')
library(factoextra)
fviz_eig(p_comp)
df <- data.frame(consumption, p_comp$x[, 1:2])
cor_mat <- cor(df)
cor_mat <- cor_mat[! rownames(cor_mat) %in% c('PC1', 'PC2'),
                   colnames(cor_mat) %in% c('PC1', 'PC2')]
print(cor_mat)
## factor mapping
fviz_pca_var(p_comp, repel = TRUE) ## avoid text overlapping  ## ankyunnerov

#Contributions of variables on PC1
fviz_contrib(p_comp, choice='var', axes=1)
fviz_contrib(p_comp, choice='var', axes=2)
fviz_contrib(p_comp, choice='var', axes=1:2)
fviz_pca_var(p_comp, col.var = 'contrib')

#constructing latent variable 
df <- read.csv("Countries.csv")
str(df)
df1 <- data.frame(Country = df[,2], df[,grepl(".Freedom", colnames(df))]) 
head(df1)
df[,grepl(".Freedom", colnames(df))]
df[,2]
df1 <- df1[complete.cases(df1)]
df1           
rownames(df1) <- df1$Country 
head(df1)
str(df1)
str(df1[,-1])
p_comp <- prcomp(df1[,-1], scale. = T)
summary(p_comp)
# First principal component explain 60% of total variation
screeplot(p_comp, type = 'l')
fviz_pca_ind(p_comp)
# extract only first component
p_comp <- prcomp(df1[,-1], scale. = T, rank. = 1)
p_comp <- prcomp(df1[,-1], scale. = T)
p_comp
df1$Score <- as.numeric(p_comp$x)
df1$FreedomIndex <- (df1$Score - min(df1$Score))/(max(df1$Score)-min(df1$Score))
ggplot(df1, aes(x = FreedomIndex)) + geom_histogram()
cor <- cor(df1[,c(colnames(df1)[grepl(".Freedom", colnames(df1))], "FreedomIndex")])
cor
data.frame(cor = cor[-nrow(cor), "FreedomIndex"]) 

# ranking
df1$FreedomIndexRank <- rank(df1$FreedomIndex)
df1$FreedomIndexRank
df1$FreedomIndexRank <- nrow(df1) - df1$FreedomIndexRank +1
df1$FreedomIndexRank
df1 <- df1[order(df1$FreedomIndexRank), ]
rownames(df1) <- NULL
head(df1[, c("Country", "Score", "FreedomIndex", "FreedomIndexRank")], 10) 



## tsne
install.packages('Rtsne')
library(Rtsne)
library(ggplot2)
set.seed(1)
country <- read.csv('Countries.csv')
df0 <- country[complete.cases(country),]

df1 <- df0[-c(1:4)]
df1
tsne <- Rtsne(df1, dims=2)
tsne$Y
df2 <- data.frame(Countries=df0$Country.Name, tsne$Y)
df2
ggplot(df2, aes(x = X1, y = X2, label = Countries)) +
  geom_point() + geom_text(aes(label = Countries), hjust = 0, vjust = 0) +
  xlab("tSNE dimension 1") + ylab("tSNE dimension 2")
