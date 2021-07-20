TASK 1:
  
the.data = as.matrix(read.table('./Energy20.txt'))
  
my.data<-the.data[sample(1:671,320),c(1:6)]
  
library(e1071)
  
Scatter Plot
plot(my.data[,6],  my.data[,1], type = "p", 
       xlab = "Energy use of appliances in Wh", 
       ylab = "Temperature in kitchen (Celcius)", 
       col = "red", pch = 4 )
cor(my.data[,1],my.data[,6])
  
plot(my.data[,6], my.data[,2], type = "p",
       xlab = "Energy use of appliances in Wh",
       ylab = "Humidity in kitchen (percentage)",
       col = "red", pch = 4 )
  cor(my.data[,2],my.data[,6])
  
plot(my.data[,6], my.data[,3], type = "p",
       xlab = "Energy use of appliances in Wh",
       ylab = "Temperature outside of kitchen (Celcius)",
       col = "red", pch = 4)
cor(my.data[,3],my.data[,6])
  
plot(my.data[,6],my.data[,4], type = "p",
       xlab = "Energy use of appliances in Wh",
       ylab = "Humidity outside",
       col = "red", pch = 4 )
cor(my.data[,4],my.data[,6])
  
plot( my.data[,6], my.data[,5], type = "p",
        xlab = "Energy use of appliances in Wh",
        ylab = "Visibility",
        col = "red", pch = 4 )
cor(my.data[,5],my.data[,6])
  
Histograms
hist(my.data[,1], main = "Energy prediction of domestic appliances",
       xlab = "Temperature in kitchen (Celcius)",
       col = "blue")
  skewness(my.data[,1])
  
hist(my.data[,2], main = "Energy prediction of domestic appliances",
       xlab = "Humidity in kitchen (in Percentage)",
       col = "blue")
  skewness(my.data[,2])
  
  hist(my.data[,3], main = "Energy prediction of domestic appliances",
       xlab = "Temperature outside",
       col = "blue")
  skewness(my.data[,3])
  
  hist(my.data[,4], main = "Energy prediction of domestic appliances",
       xlab = "Humidity outside(in Percentage)",
       col = "blue")
  skewness(my.data[,4])
  
  hist(my.data[,5], main = "Energy prediction of domestic appliances",
       xlab = "Visibility (in Kms)",
       col = "blue")
  skewness(my.data[,5])
  
  hist(my.data[,6], main = "Energy prediction of domestic appliances",
       xlab = "Energy use",
       col = "blue")
  skewness(my.data[,6])
  
  TASK 2:
    
selected.data=my.data[,-5]
  
  Linear Feature Scaling
  selected.data[,1]=(selected.data[,1]-min(selected.data[,1]))/(max(selected.data[,1])-min(selected.data[,1]))
  View(selected.data[,1])
  
  skewness(selected.data[,1])
  
  Standardization
  selected.data[,2]=(selected.data[,2]-mean(selected.data[,2]))/sd(selected.data[,2])
  View(selected.data[,2])

  skewness(selected.data[,2])
  
  Linear Feature Scaling
  selected.data[,3]=(selected.data[,3]-min(selected.data[,3]))/(max(selected.data[,3])-min(selected.data[,3]))
  View(selected.data[,3])
  
  skewness(selected.data[,3])
  
  Standardization
  selected.data[,4]=(selected.data[,4]-mean(selected.data[,4]))/sd(selected.data[,4])
  View(selected.data[,4])
  
  skewness(selected.data[,4])
  
  Standardization
  selected.data[,5]=(selected.data[,5]-mean(selected.data[,5]))/sd(selected.data[,5])
  View(selected.data[,5])

  skewness(selected.data[,5])
  
  write.table(selected.data,"Sanket-trasnformed.txt")
  
  TASK 3:
    
    source("AggWaFit718.R")
  
  sorted.data = as.matrix(read.table('./Sanket-transformed.txt'))
  View(sorted.data)
  
  colnames(sorted.data)<-c("X1","X2","X3","X4","Y")
  
  library(lpSolve)
  
  WEIGHTED ARITHMETIC MEAN:
    fit.QAM(sorted.data[,c(1:5)])
  
  WEIGHTED POWER MEAN: p=0.1 
  fit.QAM(sorted.data[,c(1:5)], 'WPM1.output.txt','WPM1.stats.txt', g=PM05, g.inv=invPM05)
  
  WEIGHTED POWER MEAN: p=10
  fit.QAM(sorted.data[,c(1:5)], 'WPM2.output.txt','WPM2.stats.txt', g=QM, g.inv = invQM)
  
  ORDERED WEIGHTED AVERAGING  (OWA):
    fit.OWA(sorted.data[,c(1:5)])
  
  CHOQUET
  fit.choquet(sorted.data[,c(1:5)])
  
  TASK 4:
    
    task4.x=c(16,38,4,77) # not including X5
  task4.x[1] = (task4.x[1]-min(my.data[,1]))/(max(my.data[,1])-min(my.data[,1]))
  task4.x[2] = (task4.x[2]-min(my.data[,2]))/(max(my.data[,2])-min(my.data[,2]))
  task4.x[3] = (task4.x[3]-min(my.data[,3]))/(max(my.data[,3])-min(my.data[,3]))
  task4.x[4] = (task4.x[4]-min(my.data[,4]))/(max(my.data[,4])-min(my.data[,4]))
  View(task4.x)
  
  choquet.weights = c(0, 0, 0, 0.0338410735054895, 0.634710909563876, 0.0338410735054895,
                      0.678200397540872, 0, 0, 0, 0, 0.0338410735054895,
                      0.634710909563876, 0.0338410735077015, 0.999999999991591)
  
  task4_ans = choquet(task4.x, choquet.weights)
  View(task4_ans)
  
  #Reversing the applied linear regression on both data sets
  
  Poly.inverse.Y = (task4_ans)^(5/2) 
  true.value= Poly.inverse.Y*(max(my.data[,6])-min(my.data[,6]))+min(my.data[,6])
  test.value = choquet(sorted.data[1,c(1,2,3,4)], choquet.weights)
  Polyinv.Actual.Y = (test.value)^(5/2)
  Correct.Y.value = Polyinv.Actual.Y*(max(my.data[,6])-min(my.data[,6]))+min(my.data[,6])
  Correct_Y_test
  TASK 5:
    
    sorted.data.dataframe=as.data.frame(sorted.data)
  
  fit.linear.model = lm(Y~X1+X2+X3+X4, data = sorted.data.dataframe)
  
  summary(fit.linear.model)
  
  prediction.320.lm = predict(fit.linear.model, sorted.data.dataframe[,1:4])
  
  plot(sorted.data.dataframe$Y, prediction.320.lm, 
       ylab = "Predicted Values of Y", 
       xlab = "True Values of Y ",
       main = "Relationship between True Value and Predicted Value of Y")
  
  prediction.Y.lm = -2.07298 +1.32997*(task4.x[1])+(-0.09217)*(task4.x[2])+(3.82065)*(task4.x[3])+ 0.35565*(task4.x[4])
  
  Actual.Y.lm=((prediction.Y.lm)^(5/2))*(max(my.data[,6])-min(my.data[,6]))+min(my.data[,6])
  