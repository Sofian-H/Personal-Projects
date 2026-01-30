X<-iris

set.seed(123) #For reproducibility

sample_size = floor(0.8*nrow(X))
set.seed(123)

# randomly split data in r
picked = sample(seq_len(nrow(X)),size = sample_size)
X_train = X[picked,]
X_test = X[-picked,]

#Estimating the parameters

setosa_mean<-as.numeric(colMeans(X_train[X_train$Species=="setosa",1:4]))
versicolor_mean<-as.numeric(colMeans(X_train[X_train$Species=="versicolor",1:4]))
virginica_mean<-as.numeric(colMeans(X_train[X_train$Species=="virginica",1:4]))

setosa_cov<-cov(X_train[X_train$Species=="setosa",1:4])
versicolor_cov<-cov(X_train[X_train$Species=="versicolor",1:4])
virginica_cov<-cov(X_train[X_train$Species=="virginica",1:4])

#Displyaing the params in the output

setosa_mean
versicolor_mean
virginica_mean

setosa_cov
versicolor_cov
virginica_cov

################################################################################
#On to question b

library(e1071)

model<-naiveBayes(Species ~.,data=X_train)
predictions<-predict(model,X_test[,1:4])

#Question c

library(ipred)

mod<-naiveBayes(Species ~.,data=X_train)
mypred<-function(object,newdata)
  predict(object,newdata=newdata)

err<-errorest(formula=Species ~.,data=X_train,model=naiveBayes,
              estimator="cv",est.para=control.errorest(k=5,predictions=TRUE),
              predict=mypred)

#Confidence intervals

n_ve=length(X_train$Species=="versicolor")
n_vi=length(X_train$Species=="virginica")

f_ve=mean(err$predictions[X_train$Species=="versicolor"]=="virginica")
f_vi=mean(err$predictions[X_train$Species=="virginica"]=="versicolor")

inf_ve=f_ve-1/sqrt(n_ve)
sup_ve=f_ve+1/sqrt(n_ve)

inf_vi=f_vi-1/sqrt(n_vi)
sup_vi=f_ve+1/sqrt(n_vi)

paste("Error rate 95% CI for versicolor [",inf_ve,',',sup_ve,"]")
paste("Error rate 95% CI for virginica [",inf_vi,',',sup_vi,"]")

#Question d

#Question e
for (i in 1:dim(X_train)[1]) {
  
  print(paste(X_train[i,5]," ",err$predictions[i]))
}

#Calculating the posteriors for the 16th element in X_train
#I'll proceed here to calculate the posteriors for each class evaluated at
#that point. However, I will not bother calculating the denominator. In fact
#we only need to largest value for the posterior, and the denominator will
#be the same everytime.

x=X_train[16,1:4]
x=as.matrix(x)

#All the means and variances were drawn from mod$tables
post_se<-mod$apriori[1]*dnorm(x[1],mean=4.985,sd=0.3605907)*dnorm(x[2],3.417500,0.3928120)*
  dnorm(x[3],1.470000,0.1828549)*dnorm(x[4],0.255000,0.1131144)

post_ve<-mod$apriori[2]*dnorm(x[1],5.928571,0.4985694)*dnorm(x[2],2.768571,0.3215509)*
  dnorm(x[3],4.271429,0.4566962)*dnorm(x[4],1.328571,0.2037464)

post_vi<-mod$apriori[3]*dnorm(x[1],6.562222, 0.6579199)*dnorm(x[2],2.966667,0.3350712)*
  dnorm(x[3],5.531111,0.5743859)*dnorm(x[4],2.013333,0.2817155)

#Question g

#Randomly selecting 16 rows in setosa
X_se=iris[1:50,]
picked_se = sample(seq_len(nrow(X_se)),size = 16)
X_se = X_se[picked_se,]

#moving on to versicolor and virginica applying the same process as above
X_ve=iris[51:100,]
picked_ve=sample(seq_len(nrow(X_ve)),size = 48)
X_ve=X_ve[picked_ve,]

X_vi=iris[101:150,]
picked_vi=sample(seq_len(nrow(X_vi)),size = 16)
X_vi=X_vi[picked_vi,]

#Assembling the 3 newly created subsets into the new training set
X_new<-rbind.data.frame(X_se,X_ve,X_vi)

size_new<-0.8*nrow(X_new)
picked_new=sample(seq_len(nrow(X_new)),size=size_new)
X_train_new<-X_new[picked_new,]
X_test_new<-X_new[-picked_new,]

#Refitting the model
new_mod<-naiveBayes(Species ~.,data=X_train_new)

new_pred<-predict(new_mod,X_test_new[,1:4])

for (i in 1:dim(X_test_new)[1]) {
  
  print(paste(i," Actual class : ",X_test_new[i,5],' | predicted class : ',new_pred[i]))
  
}