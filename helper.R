cmpHist = function(v1, v2=F, breaks=F){
  
  if(class(v1) == 'factor'){
    df1=data.frame(var=v1, type='arg1')
    if(length(v2)){
      df2=data.frame(var=v2, type='arg2')
      dff = rbind(df1, df2)
    }else{
      dff=df1
    }
    ggplot(dff, aes(var, fill=type)) + geom_bar(position="dodge")
  }else{
    # Histogram Colored (blue and red)
    if(breaks == T){
      hist(v1, col=rgb(1,0,0,0.5), breaks=breaks, main="arg1 - blue, arg2 - red", xlab="")
      if(v2 != F) hist(v2, col=rgb(0,0,1,0.5), breaks=breaks, add=T)    
    }else{
      hist(v1, col=rgb(1,0,0,0.5), main="arg1 - blue, arg2 - red", xlab="")
      if(length(v2)) hist(v2, col=rgb(0,0,1,0.5), add=T)    
    }
  }
}