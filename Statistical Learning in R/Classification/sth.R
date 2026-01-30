library(MASS)
scale=10
offset=3.5
x<-seq(1,10)
c1 <- cbind(x,x)
c2 <- cbind(x+offset,x)
call<-rbind(c1,c2)
eqscplot(call,pch='',xlab="x1",ylab="x2")
points(c1)
points(c2,pch='x')
cs1 <- cbind(x*scale,x)
cs2 <- cbind((x+offset)*scale,x)
calls<-rbind(cs1,cs2)
eqscplot(calls,pch='',xlab="x1",ylab="x2")
10
points(cs1)
points(cs2,pch='x')
##
### New version below including predicted classes
##
## scaled
library(class)
minx = min(cs1[,1],cs2[,1])
maxx = max(cs1[,1],cs2[,1])
miny = min(cs1[,2],cs2[,2])-10
maxy = max(cs1[,2],cs2[,2])+10
xs <- seq(minx,maxx)
ys <- seq(miny,maxy)
lenxs = length(xs)
lenys = length(ys)
classes = factor(c(rep(1,10), rep(0,10)))
z<-matrix(0,nrow=lenxs,ncol=lenys)
for (i in 1:lenxs) {
  for (j in 1:lenys) {
    z[i,j] = knn(calls,c(xs[i],ys[j]),cl=classes,k=1)
  }
}
image(xs,ys,z,asp=1)
points(cs1)
points(cs2,pch='x')
## unscaled
minu = min(c1[,1],c2[,1])
maxu = max(c1[,1],c2[,1])
minv = min(c1[,2],c2[,2])
maxv = max(c1[,2],c2[,2])
101
us <- seq(minu,maxu,0.02)
vs <- seq(minv,maxv,0.02)
lenus = length(us)
lenvs = length(vs)
classes = factor(c(rep(1,10), rep(0,10)))
w<-matrix(0,nrow=lenus,ncol=lenvs)
for (i in 1:lenus) {
  for (j in 1:lenvs) {
    w[i,j] = knn(call,c(us[i],vs[j]),cl=classes,k=1)
  }
}
image(us,vs,w,asp=1)
points(c1)
points(c2,pch='x')
