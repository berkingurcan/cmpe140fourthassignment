plot_car_data <- function(filename){
  mpg <- read.table(filename)

  par(mfrow = c(2,3))
  
  plot(mpg$V4,mpg$V1, xlab = "Engine power", ylab="Miles Per Gallon")
  plot(mpg$V6,mpg$V1, xlab = "Acceleration", ylab="Miles Per Gallon")
  plot(mpg$V5,mpg$V1, xlab = "Weight", ylab="Miles Per Gallon")
  plot(mpg$V2,mpg$V1, xlab = "Number of cyclinders", ylab="Miles Per Gallon")
  plot(mpg$V3,mpg$V1, xlab = "Displacement", ylab="Miles Per Gallon")
  
}
predict_mpg <- function(p,a,w){
  mpg <- read.table("auto-mpg.data")
  
  mpg[] <- lapply(mpg, function(x) as.numeric(as.character(x)))
  
  y <- mpg$V1[2:length(mpg$V1)]
  
  horsepower <- mpg$V4[2:length(mpg$V1)]
  weight <- mpg$V5[2:length(mpg$V5)]
  acceleration <- mpg$V6[2:length(mpg$V6)]
  
  linerfiting <- lm(y ~ horsepower+acceleration+weight, data = mpg)   
  toto <- predict.lm(linerfiting, data.frame(horsepower = p,acceleration=a,weight=w))
  return(toto)
}

first_model <- function(filename){
  dataxy <- read.csv(filename, sep=' ')
  
  y <- dataxy$y
  dataxy$x <- cos(dataxy$x)
  cosx <- dataxy$x
  
  fitto <- lm(y~cosx, data = dataxy)
  return(summary(fitto)$coefficients[,1])
}

second_model <- function(filename){
  dataxy <- read.csv(filename, sep=' ')
  y <- dataxy$y
  
  cosx <- cos(dataxy$x)
  
  cos3x <- cos(dataxy$x * 3)
  
  fitto <- lm(y~cosx+cos3x, data = dataxy)
  return(summary(fitto)$coefficients[,1])
  
  
}


third_model <- function(filename){
  dataxy <- read.csv(filename, sep=' ')
  y <- dataxy$y
  
  cosx <- cos(dataxy$x)
  
  cos3x <- cos(dataxy$x * 3)
  
  cos5x <- cos(dataxy$x * 5)
  
  fitto <- lm(y~cosx+cos3x+cos5x, data = dataxy)
  return(summary(fitto)$coefficients[,1])
}


plotmodels <- function(filename){
  dataxy <- read.csv(filename, sep=' ')
  
  y <- dataxy$y
  cosx <- cos(dataxy$x)
  
  cos3x <- cos(dataxy$x * 3)
  
  cos5x <- cos(dataxy$x * 5)
  
  fitto1 <- lm(y~cosx, data = dataxy)
  pre1 <- predict.lm(fitto1, data=dataxy)
  fitto2 <- lm(y~cosx+cos3x, data = dataxy)
  pre2 <- predict.lm(fitto2, data=dataxy)
  fitto3 <- lm(y~cosx+cos3x+cos5x, data = dataxy)
  pre3 <- predict.lm(fitto3, data=dataxy)
  
  par(mfrow=c(2,2))
  
  plot(dataxy$x,dataxy$y,xlab="x",ylab="y", pch = 20,main="Data")
  
  plot(dataxy$x,dataxy$y,xlab="x",ylab="y", pch = 20,main="Data and first model")
  lines(dataxy$x, pre1, col="green")
  plot(dataxy$x,dataxy$y,xlab="x",ylab="y", pch = 20,main = "Data and second model")
  lines(dataxy$x,pre2,col="red")
  plot(dataxy$x,dataxy$y,xlab="x",ylab="y", pch = 20, main="Data and third model")
  lines(dataxy$x,pre3,col="blue")

}


fenwick_workforce <- function(years, initial, rates){
  M <- rates
  init <- initial
  
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  
  df <- rbind(df, init)
  
  for (i in 1:years) {
    x <- round(M %*% as.numeric(df[i,]))
    df <- rbind(df,x[,1])
  }
  
  names <- c("Agro", "Inds", "Serv")
  colnames(df) <- names
  return(df)
}

plot_workforce_time <- function(years, initial, rates){
  par(mfrow = c(1,1))
  df <- fenwick_workforce(years,initial,rates)
  plot(df$Agro, type = "l", col="red", xlab="years", ylab="number",ylim = c(min(df,na.rm = TRUE),max(df,na.rm = TRUE)) )
  lines(df$Inds, col = "blue")
  lines(df$Serv, col= "green")
  legend("topright",inset=.05, legend = c("Agro","Inds","Serv"), col=c("red","blue","green"),lty = 1)
}







