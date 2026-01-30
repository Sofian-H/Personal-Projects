library(RWeka)
X<-iris

set.seed(123) #For reproducibility

sample_size = floor(0.8*nrow(X))
set.seed(123)

# randomly split data in r
picked = sample(seq_len(nrow(X)),size = sample_size)
X_train = X[picked,]
X_test = X[-picked,]

#Building the tree with J48 command, meaning it will use the C4.5 algorithm
dec_tree <- J48(Species ~ ., data = X_train)
predictions<-predict(dec_tree,newdata=X_test)

#Error rates estimates
mean(predictions[X_train$Species=="setosa"]=="versicolor")

mean(predictions[X_train$Species=="setosa"]=="virginica")

mean(predictions[X_train$Species=="versicolor"]=="setosa")

mean(predictions[X_train$Species=="versicolor"]=="virginica")

mean(predictions[X_train$Species=="virginica"]=="versicolor")

mean(predictions[X_train$Species=="virginica"]=="setosa")

#Question c

library(ipred)

mypred<-function(object,newdata)
  predict(object,newdata=newdata)

err<-errorest(formula=Species ~.,data=X_train,model=J48,
              estimator="cv",est.para=control.errorest(k=5,predictions=TRUE),
              predict=mypred)

#error rate estimates for the CV
mean(err$predictions[X_train$Species=="setosa"]=="versicolor")

mean(err$predictions[X_train$Species=="setosa"]=="virginica")

mean(err$predictions[X_train$Species=="versicolor"]=="setosa")

mean(err$predictions[X_train$Species=="versicolor"]=="virginica")

mean(err$predictions[X_train$Species=="virginica"]=="versicolor")

mean(err$predictions[X_train$Species=="virginica"]=="setosa")

#95% CIs

n_s=length(X_train$Species=="setosa")
n_ve=length(X_train$Species=="versicolor")
n_vi=length(X_train$Species=="virginica")

f_s=mean(err$predictions[X_train$Species=="setosa"]!="setosa")
f_ve=mean(err$predictions[X_train$Species=="versicolor"]!="versicolor")
f_vi=mean(err$predictions[X_train$Species=="virginica"]!="virginica")

inf_s=f_s-1/sqrt(n_s)
sup_s=f_s+1/sqrt(n_s)

inf_ve=f_ve-1/sqrt(n_ve)
sup_ve=f_ve+1/sqrt(n_ve)

inf_vi=f_vi-1/sqrt(n_vi)
sup_vi=f_ve+1/sqrt(n_vi)

paste("Error rate 95% CI for setosa [",inf_s,',',sup_s,"]")
paste("Error rate 95% CI for versicolor [",inf_ve,',',sup_ve,"]")
paste("Error rate 95% CI for virginica [",inf_vi,',',sup_vi,"]")

for (i in 1:dim(X_train)[1]) {
  
  print(paste(X_train[i,5]," ",err$predictions[i]))
}

for (i in 1:dim(X_test)[1]) {
  
  print(paste(X_test[i,5]," ",predictions[i],',',i))
}

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
X_test_new<-X_new[-picked_new,]

#Refitting the model
new_dec_tree<-J48(Species ~ ., data = X_train_new)

new_pred<-predict(new_dec_tree,X_test_new[,1:4])

for (i in 1:dim(X_test_new)[1]) {
  
  print(paste(i," Actual class : ",X_test_new[i,5],' | predicted class : ',new_pred[i]))
  
}