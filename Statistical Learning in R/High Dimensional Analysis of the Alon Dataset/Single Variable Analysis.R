X<-load("..\\alon.rda")

library(stats)
library(rstatix)

fdr<-0.01

dat<-as.data.frame(cbind(x,y))

gene_order<-c(1:2000)
orig_p_vals<-c(1:2000)*0
adj_p_vals<-c(1:2000)*0
  
for (i in 1:2000) {
  
  temp<-data.frame(x=dat[,i],y=dat$y)
  orig_p_vals[i]<-t_test(data=temp,formula=x~y,var.equal=FALSE,conf.level=1-fdr,
         p.adjust.method = "BH")$p
  
}

p_vals<-data.frame(order=gene_order,p_value=orig_p_vals)
p_vals<-p_vals[order(p_vals$p_value),]

i<-1
threshold<-0
while (p_vals$p_value[i] < i*(fdr/2000)) {
  
  threshold<-i*(fdr/2000)
  i<-i+1
  
}

write.csv(p_vals[1:i,],file="Genes_declared_significant.csv")

library(ggplot2)

p<-ggplot(p_vals[1:i,])+geom_point(aes(sort(p_vals$order[1:i]),p_vals$p_value[1:i])) +
  geom_vline(xintercept = i,linetype="dashed")+
  xlab("Genes ordered by p-value")+ ylab("p-values")
