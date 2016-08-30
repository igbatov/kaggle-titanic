plotPredictors = function(respname,data){
  # arguments type check
  data = as.data.frame(data);
  if(!is.character(respname)){
    print('respname must be a string');
    return(FALSE)
  }

  # try to determine which column has response variable
  respindex = grep(respname, colnames(data))
  if(!is.numeric(respindex)){
    print(paste('Cannot find ',respname,' in data'))
    return(FALSE);
  }
  
  # troubles with ggpairs because I cannot easily output single row :(
  pm = ggpairs(data);
  
  par(mfrow=c(1,length(colnames(data)))); 
  for(i in c(1:length(colnames(data)))) print(getPlot(pm, respindex, i))
}