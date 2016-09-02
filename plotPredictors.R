plotPredictors = function(respname,data){
  # arguments type check
  data = as.data.frame(data);
  if(!is.character(respname)){
    print('respname must be a string');
    return(FALSE)
  }

  par(mfrow=c(1,length(colnames(data))-1)); 
  for(predname in colnames(data)){
    if(predname == respname) next();
    print(plot(data[,c(respname)], data[,c(predname)], xlab=respname, ylab=predname))
  }
  
  # default par
  par(mfrow=c(1,1));
}