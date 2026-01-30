train_counts<-c(1:9)*0
test_counts<-c(1:9)*0

#for (i in 1:dim(train)[1]) {
  
#  for (j in 1:9) {
    
#    if (train[i,]$y==j)
#      train_counts[j]<-train_counts[j]+1
    
#  }
  
#}

#for (i in 1:dim(test)[1]) {
  
#  for (j in 1:9) {
    
#    if (test[i,]$y==j)
#      test_counts[j]<-test_counts[j]+1
    
#  }
  
#}

mean_image<-colMeans(train[,1:784])
