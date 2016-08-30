d = data.frame(P=rep(NA,1000),R=rep(NA,1000));
for(i in c(1:100)){
  for(j in c(1:10)){
    d[(i-1)*10+j,c('P')]=j;
    d[(i-1)*10+j,c('R')]=sample(c("A","B"), size=1, replace=TRUE, prob=c(1/j,(1-1/j)));
  }
}

d$R = factor(d$R)
glmfit = glm(R=='A'~P, data=d, family=binomial)
print(summary(glmfit))
print(predict(glmfit, newdata = data.frame(P=c(1:10)), type="response"))
# plot distribution of response (i.e. 'A' and 'B') 
# among predictor values in a glm 'link' scale 
# (from https://www.r-bloggers.com/learn-logistic-regression-and-beyond/)
densityplot(predict(glmfit,type='link'),groups=d$R=='A', auto.key=T)