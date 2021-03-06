#From http://rpubs.com/hughes/116374
#Returns predicted and boostraped confidence intervals for lm, glm, lmer and glmer models
#parameters:
#@m : the fitted lm, glm or merMod object (need to be provided)
#@focal_var: a character, the name of variable of interest that will be plotted on the x axis, ie the varying variable (need to be provided)
#@inter_var: a character or character vector, the name of variable interacting with the focal variable, ie categorical variables from which prediction will be drawn for each level across the focal_var gradient
#@RE: if giving a merMod object give as character or character vector of the name of the random effects variable (so far I only tried with one RE)
#@n: a numeric, the number of data point that will form the gradient of the focal variable
#@n_core: the number of core used to compute the bootstrapped CI for GLMM models

plot_fit<-function(m,focal_var,inter_var=NULL,RE=NULL,n=20,n_core=4){
  require(arm)  
  dat<-model.frame(m)
  #turn all character variable to factor
  dat<-as.data.frame(lapply(dat,function(x){
    if(is.character(x)){
      as.factor(x)
    }
    else{x}
  }))
  #make a sequence from the focal variable
  x1<-list(seq(min(dat[,focal_var]),max(dat[,focal_var]),length=n))
  #grab the names and unique values of the interacting variables
  isInter<-which(names(dat)%in%inter_var)
  if(length(isInter)==1){
    x2<-list(unique(dat[,isInter]))
    names(x2)<-inter_var
  }
  if(length(isInter)>1){
    x2<-lapply(dat[,isInter],unique)
  }
  if(length(isInter)==0){
    x2<-NULL
  }
  #all_var<-x1
  #add the focal variable to this list
  all_var<-c(x1,x2)
  #expand.grid on it
  names(all_var)[1]<-focal_var
  all_var<-expand.grid(all_var)
  
  #remove varying variables and non-predictors
  dat_red<-dat[,-c(1,which(names(dat)%in%c(focal_var,inter_var,RE,"X.weights."))),drop=FALSE]
  if(dim(dat_red)[2]==0){
    new_dat<-all_var
  }
  else{
    fixed<-lapply(dat_red,function(x) if(is.numeric(x)) mean(x) else factor(levels(x)[1],levels = levels(x)))
    #the number of rows in the new_dat frame
    fixed<-lapply(fixed,rep,dim(all_var)[1])
    #create the new_dat frame starting with the varying focal variable and potential interactions
    new_dat<-cbind(all_var,as.data.frame(fixed)) 
    #get the name of the variable to average over, debug for conditions where no variables are to be avergaed over
    name_f<-names(dat_red)[sapply(dat_red,function(x) ifelse(is.factor(x),TRUE,FALSE))]
  }  
  
  
  #get the predicted values
  cl<-class(m)[1]
  if(cl=="lm"){
    pred<-predict(m,newdata = new_dat,se.fit=TRUE)
  }
  
  if(cl=="glm" | cl=="negbin"){
    #predicted values on the link scale
    pred<-predict(m,newdata=new_dat,type="link",se.fit=TRUE)
  }
  if(cl=="glmerMod" | cl=="lmerMod"){
    pred<-list(fit=predict(m,newdata=new_dat,type="link",re.form=~0))
    #for bootstrapped CI
    new_dat<-cbind(new_dat,rep(0,dim(new_dat)[1]))
    names(new_dat)[dim(new_dat)[2]]<-as.character(formula(m)[[2]])
    mm<-model.matrix(formula(m,fixed.only=TRUE),new_dat)
  }
  #average over potential categorical variables  
  if(length(name_f)>0){
    if(cl=="glmerMod" | cl=="lmerMod"){
      coef_f<-lapply(name_f,function(x) fixef(m)[grep(paste0("^",x,"\\w+$"),names(fixef(m)))])
    }
    else{
      coef_f<-lapply(name_f,function(x) coef(m)[grep(paste0("^",x,"\\w+$"),names(coef(m)))])
    }    
    pred$fit<-pred$fit+sum(unlist(lapply(coef_f,function(x) mean(c(0,x)))))
  }
  #to get the back-transform values get the inverse link function
  linkinv<-family(m)$linkinv
  
  #get the back transformed prediction together with the 95% CI for LM and GLM
  if(cl=="glm" | cl=="lm"){
    pred$pred<-linkinv(pred$fit)
    pred$LC<-linkinv(pred$fit-1.96*pred$se.fit)
    pred$UC<-linkinv(pred$fit+1.96*pred$se.fit)
  }
  
  #for GLMM need to use bootstrapped CI, see ?predict.merMod
  if(cl=="glmerMod" | cl=="lmerMod"){
    pred$pred<-linkinv(pred$fit)
    predFun<-function(.) mm%*%fixef(.)
    bb<-bootMer(m,FUN=predFun,nsim=200,parallel="multicore",ncpus=n_core) #do this 200 times
    bb$t<-apply(bb$t,1,function(x) linkinv(x))
    #as we did this 200 times the 95% CI will be bordered by the 5th and 195th value
    bb_se<-apply(bb$t,1,function(x) x[order(x)][c(5,195)])
    pred$LC<-bb_se[1,]
    pred$UC<-bb_se[2,] 
  }
  
  #the output
  out<-as.data.frame(cbind(new_dat[,1:(length(inter_var)+1)],pred$LC,pred$pred,pred$UC))
  names(out)<-c(names(new_dat)[1:(length(inter_var)+1)],"LC","Pred","UC")
  return(out)
}