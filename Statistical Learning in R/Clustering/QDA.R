#Part of the code below was copied from the following video :
# https://www.youtube.com/watch?v=0Jp4gsfOLMs

pca<-prcomp(train[,1:784],retx=TRUE,tol=0.01)

pca.var<-pca$sdev^2
pca.var.per<-round(pca.var/sum(pca.var)*100)

#We project our dataset onto the 37 first PCs

proj_train<-pca$x[,1:37]
proj_train<-cbind(proj_train,train$y)
#note that the newly added column has name V38 rather than y
proj_train<-as.data.frame(proj_train)
#For some reason all values in V38 were incremented by 1 compared to train$y
#I'm therefore just correcting that here
proj_train$V38<-proj_train$V38-1

#projecting the training set on the PCs
proj_test<-as.matrix(test[,1:784])%*%pca$rotation[,1:37]
proj_test<-cbind(proj_test,test$y)
proj_test<-as.data.frame(proj_test)
#Same thing here, all values were somehow incremented by 1
proj_test$V38<-proj_test$V38-1

library(MASS)

model<-qda(V38~.,proj_train)
predictions<-predict(model,proj_test[,1:37])

#On to error rates
overall_err<-0
#The counts variable will store the number of occurrences of each class
counts<-c(0:9)*0
cls_err<-c(0:9)*0

#counting the nb of occurrences of each class
for (i in 1:dim(proj_test)[1]){
  
  counts[proj_test$V38[i]+1]<-counts[proj_test$V38[i]+1]+1
  
}

#Calculating the errors
for (i in 1:dim(proj_test)[1]) {
  
  if (proj_test$V38[i]!=predictions$class[i]) {
    cls_err[proj_test$V38[i]+1]<-cls_err[proj_test$V38[i]+1]+1
    overall_err<-overall_err+1
  }
}
for (i in 1:10) {
  
  cls_err[i]<-cls_err[i]/counts[i]
  
}

overall_err<-overall_err/10000

#printing the error rates

for (i in 1:10) {
  
  p<-cls_err[i]
  n<-counts[i]
  print(paste("95% CI for class",i-1,"error rate : [",p-1.96*sqrt(p*(1-p)/n),",",p+1.96*sqrt(p*(1-p)/n),"]"))
  
}

print(paste("95% CI for the overall error rate : [",overall_err-1.96*sqrt(overall_err*(1-overall_err)/10000),","
            ,overall_err+1.96*sqrt(overall_err*(1-overall_err)/10000),"]"))

#Question c

new_preds<-predict(model,proj_train[,1:37])

#On to error rates
new_overall_err<-0
#The counts variable will store the number of occurrences of each class
new_counts<-c(0:9)*0
new_cls_err<-c(0:9)*0

#counting the nb of occurrences of each class
for (i in 1:dim(proj_train)[1]){
  
  new_counts[proj_train$V38[i]+1]<-new_counts[proj_train$V38[i]+1]+1
  
}

#Calculating the errors
for (i in 1:dim(proj_train)[1]) {
  
  if (proj_train$V38[i]!=new_preds$class[i]) {
    new_cls_err[proj_train$V38[i]+1]<-new_cls_err[proj_train$V38[i]+1]+1
    new_overall_err<-new_overall_err+1
  }
}
for (i in 1:10) {
  
  new_cls_err[i]<-new_cls_err[i]/new_counts[i]
  
}

new_overall_err<-new_overall_err/60000

#printing the error rates

for (i in 1:10) {
  
  p<-new_cls_err[i]
  n<-new_counts[i]
  print(paste("95% CI for class",i-1,"error rate : [",p-1.96*sqrt(p*(1-p)/n),",",p+1.96*sqrt(p*(1-p)/n),"]"))
  
}

print(paste("95% CI for the overall error rate : [",new_overall_err-1.96*sqrt(new_overall_err*(1-new_overall_err)/60000),","
            ,new_overall_err+1.96*sqrt(new_overall_err*(1-new_overall_err)/60000),"]"))

#######################################################################################################################

#Question e

class_mins<-c(0:9)*0
class_mins<-class_mins+1.000000e+30 #setting it to a value which I know can't be outgrown
class_indices<-c(0:9)*0

for (i in 1:60000) {
  for (j in 0:9) {
    if ((proj_train$V38[i]==new_preds$class[i]) && new_preds$class[i]==j) {
      n<-norm(as.matrix(colMeans(proj_train[proj_train$V38==j,1:37]))-
                proj_train[i,1:37],"2")
      
      if (class_mins[j+1]>n) {
        class_mins[j+1]<-n
        class_indices[j+1]<-i
      }
    }
  }
}

for (i in 0:9) {
  
  #For obvious purposes, we're this time selecting off the original training set
  show_digit(train[class_indices[i+1],1:784])
  show_digit(colMeans(as.matrix(train[train$y==i,1:784])))
  
}

########################################################################################################################

#Question f
#The code is almost the same as in e but in the first "if" block, the first == becomes a !=

class_maxs<-(c(0:9)*0)+1.0e30 #setting it to a value which I know can't be outgrown
class_indices<-c(0:9)*0

for (i in 1:60000) {
  for (j in 0:9) {
    if ((proj_train$V38[i]!=new_preds$class[i]) && new_preds$class[i]==j) {
      n<-norm(as.matrix(colMeans(proj_train[proj_train$V38==j,1:37]))
              -proj_train[i,1:37],"2")
      
      if (class_maxs[j+1]>n) {
        class_maxs[j+1]<-n
        class_indices[j+1]<-i
      }
    }
  }
}

for (i in 0:9) {
  
  show_digit(train[class_indices[i+1],1:784])
  show_digit(colMeans(as.matrix(train[train$y==new_preds$class[class_indices[i+1]],1:784])))
 
  print(paste(train$y[class_indices[i+1]],",",new_preds$class[class_indices[i+1]])) 
}

#########################################################################################################################

#Question g

one_mean<-c(1:784)*0
seven_mean<-c(1:784)*0

for (i in 1:60000) {
  
  if (train$y[i]==1 && new_preds$class[i]==1)
    one_mean<-one_mean+train[i,1:784]
  
  if (train$y[i]==7 && new_preds$class[i]==7)
    seven_mean<-seven_mean+train[i,1:784]
  
}

one_mean<-one_mean/60000
seven_mean<-seven_mean/60000

show_digit(one_mean)
show_digit(seven_mean)
show_digit(seven_mean-one_mean)