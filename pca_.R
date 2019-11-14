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
