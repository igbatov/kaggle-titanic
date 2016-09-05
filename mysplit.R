library(caret)

mysplit = function(data,response){
  inTrain = createDataPartition(data[,c(response)], p=0.7, list=FALSE);
  return(list(train=data[inTrain,], test=data[-inTrain,]));
}

s=mysplit(Auto,'mpg')
lmfit = lm(mpg~horsepower+I(horsepower^2),s$train)
pr = predict(lmfit,newdata = s$test)
mse1 = mean((s$test$mpg-pr)^2)
lmfit = lm(mpg~horsepower+I(horsepower^2)+I(horsepower^3)+I(horsepower^4)+I(horsepower^5)+I(horsepower^6)+I(horsepower^7)+I(horsepower^8)+I(horsepower^9)+I(horsepower^10),s$train)
pr = predict(lmfit,newdata = s$test)
mse2 = mean((s$test$mpg-pr)^2)
