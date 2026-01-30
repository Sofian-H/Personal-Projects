library(FactoMineR)

X<-load("..\\alon.rda")

res.pca<-PCA(x,scale.unit=TRUE,ncp=61,graph=TRUE)

#Computing the loadings with a command found on the following link :
#https://groups.google.com/g/factominer-users/c/BRN8jRm-_EM?pli=1

loadings<-sweep(res.pca$var$coord,2,sqrt(res.pca$eig[,1]),FUN="/")

#Now computing the matrix of principal components

T<-(x-colMeans(x))%*%loadings

#Exporting T and the loadings as csv files

write.csv(T,file="principal_components.csv")
write.csv(loadings,file="loadings.csv")

#Retrieving individual variance first and then cumulative variance

indiv_var<-res.pca$eig[,2]
cum_var<-res.pca$eig[,3]

#Moving on to plotting

library(ggplot2)

r<-c(1:61)

df<-data.frame(r,indiv_var,cum_var)

g_ind<-ggplot(df) + 
  geom_line(aes(r,indiv_var),size=1,color="red") +
  xlab("Principal components") +
  ylab("Variance")

g_cum<-ggplot(df) +
  geom_line(aes(x,cum_var),size=1,color="blue") +
  xlab("Principal components") +
  ylab("Cumulatives variance")

#Projecting onto the first 2 principal components

P<-T[,1:2]

cl<-c(1:62)*0

for (i in 1:61) {
  
  if (y[i]=="Tumour")
    cl[i]<-1
  
}

p<-ggplot(as.data.frame(T)) + geom_point(aes(x=P[,1], y=P[,2],color=cl))
p<-p+xlab("PC1")+ylab("PC2")
