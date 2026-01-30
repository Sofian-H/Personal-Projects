library(class)

knn_model<-knn(train=proj_train[,1:37],test=proj_test[,1:37],
               cl=as.character(proj_train$V38),k=245)

#On to error rates
knn_overall_err<-0
#The counts variable will store the number of occurrences of each class
knn_counts<-c(0:9)*0
knn_cls_err<-c(0:9)*0

#counting the nb of occurrences of each class
for (i in 1:dim(proj_test)[1]){
  
  knn_counts[proj_test$V38[i]+1]<-knn_counts[proj_test$V38[i]+1]+1
  
}

#Calculating the errors
for (i in 1:dim(proj_test)[1]) {
  
  if (proj_test$V38[i]!=knn_model[i]) {
    knn_cls_err[proj_test$V38[i]+1]<-knn_cls_err[proj_test$V38[i]+1]+1
    knn_overall_err<-knn_overall_err+1
  }
}
for (i in 1:10) {
  
  knn_cls_err[i]<-knn_cls_err[i]/knn_counts[i]
  
}

knn_overall_err<-knn_overall_err/10000

#printing the error rates

for (i in 1:10) {
  
  p<-knn_cls_err[i]
  n<-knn_counts[i]
  print(paste("95% CI for class",i-1,"error rate : [",p-1.96*sqrt(p*(1-p)/n),",",p+1.96*sqrt(p*(1-p)/n),"]"))
  
}

print(paste("95% CI for the overall error rate : [",knn_overall_err-1.96*sqrt(knn_overall_err*(1-knn_overall_err)/10000),","
            ,knn_overall_err+1.96*sqrt(knn_overall_err*(1-knn_overall_err)/10000),"]"))

#########################################################################################################################

#Question c

knn_new_model<-knn(train=proj_train[,1:37],test=proj_train[,1:37],
                   cl=as.character(proj_train$V38),k=245)

#On to error rates
knn_new_overall_err<-0
#The counts variable will store the number of occurrences of each class
knn_new_counts<-c(0:9)*0
knn_new_cls_err<-c(0:9)*0

#counting the nb of occurrences of each class
for (i in 1:dim(proj_train)[1]){
  
  knn_new_counts[proj_train$V38[i]+1]<-knn_new_counts[proj_train$V38[i]+1]+1
  
}

#Calculating the errors
for (i in 1:dim(proj_train)[1]) {
  
  if (proj_train$V38[i]!=knn_new_model[i]) {
    knn_new_cls_err[proj_train$V38[i]+1]<-knn_new_cls_err[proj_train$V38[i]+1]+1
    knn_new_overall_err<-knn_new_overall_err+1
  }
}
for (i in 1:10) {
  
  knn_new_cls_err[i]<-knn_new_cls_err[i]/knn_new_counts[i]
  
}

knn_new_overall_err<-knn_new_overall_err/60000

#Question e

class_mins<-c(0:9)*0
class_mins<-class_mins+1.000000e+30 #setting it to a value which I know can't be outgrown
class_indices<-c(0:9)*0

for (i in 1:10000) {
  for (j in 0:9) {
    if ((proj_test$V38[i]==knn_model[i]) && knn_model[i]==j) {
      n<-norm(as.matrix(colMeans(proj_test[proj_test$V38==j,1:37]))
              -proj_test[i,1:37],"2")
      
      if (class_mins[j+1]>n) {
        class_mins[j+1]<-n
        class_indices[j+1]<-i
      }
    }
  }
}

for (i in 0:9) {
  
  #For obvious purposes, we're this time selecting off the original training set
  show_digit(test[class_indices[i+1],1:784])
  show_digit(colMeans(as.matrix(test[test$y==i,1:784])))
  
}

########################################################################################################################

#Question f
#The code is almost the same as in e but in the first "if" block, the first == becomes a !=

class_maxs<-(c(0:9)*0)+1.0e30 #setting it to a value which I know can't be outgrown
class_indices<-c(0:9)*0

for (i in 1:10000) {
  for (j in 0:9) {
    if ((proj_test$V38[i]!=knn_model[i]) && knn_model[i]==j) {
      n<-norm(as.matrix(colMeans(proj_test[proj_test$V38==j,1:37]))
              -proj_test[i,37],"2")
      
      if (class_maxs[j+1]>n) {
        class_maxs[j+1]<-n
        class_indices[j+1]<-i
      }
    }
  }
}

for (i in 0:9) {
  
  show_digit(test[class_indices[i+1],1:784])
  show_digit(colMeans(as.matrix(test[test$y==knn_model[class_indices[i+1]],1:784])))
  
  print(paste(test$y[class_indices[i+1]],",",knn_model[class_indices[i+1]])) 
}

#########################################################################################################################

#Question g

one_mean<-c(1:784)*0
seven_mean<-c(1:784)*0

for (i in 1:10000) {
  
  if (test$y[i]==1 && knn_model[i]==1)
    one_mean<-one_mean+test[i,1:784]
  
  
  if (test$y[i]==7 && knn_model[i]==7)
    seven_mean<-seven_mean+test[i,1:784]
  
}

one_mean<-one_mean/10000
seven_mean<-seven_mean/10000

show_digit(one_mean)
show_digit(seven_mean)
show_digit(abs(seven_mean-one_mean))