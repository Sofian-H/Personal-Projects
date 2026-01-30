X<-load("..\\alon.rda")

library(MASS)

dat<-vector()

for (i in 1:2000) {
  
  dat<-cbind(dat,x[,i])
  
}

dat<-cbind(dat,y)
dat<-as.data.frame(dat)

C<-cov(x)

lin_discr<-lda(formula=y~x)

write.csv(lin_discr$means,file="LDA means.csv")
write.csv(C,file="cov.csv")

library(glmnet)

set.seed(7)

lasso<-cv.glmnet(x,y,alpha=1,family="binomial",nfolds = 62)

log_reg<-glmnet(x,y,alpha=1,family="binomial",lambda=lasso$lambda.min)

#To estimate CV errors

library(ipred)

# force predict to return class labels only
mypredict.lda <- function(object, newdata)
  predict(object, newdata = newdata)$class

err_lda<-errorest(data=dat,y~x,model=lda,estimator="cv",predict=mypredict.lda,
                  est.para=control.errorest(k=62))

mypredict.logreg <- function(object, newdata)
  predict.glmnet(object,newdata = newdata)$class

err_lda<-errorest(data=dat,y~x,model=lda,estimator="cv",predict=mypredict.lda,
                 est.para=control.errorest(,predictions=TRUE))

#moving on to logistic regression, code inspired by link below

overall_error<-0
tumour_error<-0
normal_error<-0
folds<-c(1:62)
predictions <- c(1:62)*0

for (k in 1:62) {
  x_train<-dat[-folds[k],1:2000]
  x_test <-dat[folds[k],1:2000]
  y_train<-dat[-folds[k],2001]
  y_test<-dat[folds[k],2001]
  fit<-glmnet(as.matrix(x_train),as.matrix(y_train),alpha=1,family="binomial",lambda=lasso$lambda.min)
  pred<-predict(fit,as.matrix(x_test),s=lasso$lambda.min,type="class")
  
  if (k < 41) {
    if (pred!=dat$y[k]) {
      tumour_error<-tumour_error+1
      overall_error<-overall_error+1
    }
  }
  else {
    if (pred!=dat$y[k]) {
      normal_error<-normal_error+1
      overall_error<-overall_error+1
    }
  }
}

overall_error<-overall_error/62
tumour_error<-tumour_error/40
normal_error<-normal_error/22

#Using nested cv to determine lambda

library(TANDEM)

upstream<-vector(length=2000)
for (i in 1:2000) {
  upstream[i]<-TRUE
}
cv_glmnet = nested.cv(x, dat$y, upstream=upstream, method="glmnet", alpha=1,family="binomial")
