#Hello World!

wine.red <- read.csv("winequality-red.csv" , sep = ";", header = TRUE)
wine.white <- read.csv("winequality-white.csv" , sep = ";", header = TRUE)

wine.red$red <- TRUE

#wine.red <- cbind(wine.red, color = rep(TRUE, length(wine.red[,1])))
#wine.white <- cbind(wine.white, color = rep(FALSE, length(wine.white[,1])))

#wine.red$red = TRUE
#wine.white$red = FALSE

#wine <- rbind(wine.red, wine.white)


#wine.red <- scale(wine.red ,center = TRUE)