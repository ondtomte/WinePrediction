# Mål: Separera bra viner från dåliga, splittar skalan vid 5!


# Ta bort icke-skalad LM!

wine.red <- read.csv("winequality-red.csv" , sep = ";", header = TRUE)
wine.white <- read.csv("winequality-white.csv" , sep = ";", header = TRUE)

wine.red$red <- TRUE
wine.white$red <- FALSE

# kollade NA

# struntar i rött!
# wine <- rbind(wine.red, wine.white)
wine <- wine.white

# gör om snyggare!

set.seed(23)

random <- sample(1:length(wine[,1]), 1500)
test.ind <- sample(1:2000, 500)

train <- wine[-random,]
valid <- wine[random[-test.ind],]
test <- wine[random[test.ind],]

##

lin.wine <- lm(quality ~ ., data = train)
lin.wine.mse <- mean((predict(lin.wine, newdata = valid[,-12]) - valid[,12])^2)
baseline.error <- mean((valid[,12] - mean(train$quality))^2)

wine2 <- scale(wine[,-c(12,13)] ,center = TRUE)
wine2 <- as.data.frame(wine2)
wine2$quality <- wine$quality
wine2$red <- wine$red

set.seed(23)

train2 <- wine2[-random,]
valid2 <- wine2[random[-test.ind],]
test2 <- wine2[random[test.ind],]

lin.wine2 <- lm(quality ~ ., data = train2)
lin.wine2.mse <- mean((predict(lin.wine2, newdata = valid2[,-12]) - valid2[,12])^2)
baseline.error2 <- mean((valid2[,12] - mean(train2$quality))^2)

#prcomp$x = ny data roterad

PCA <- prcomp(train2[,-12], data = train2)
pca.train <- princomp(train2[,-12], data = train2)$scores[,1:10]
pca.valid <- data.frame(princomp(valid2[,-12], data = valid2)$scores[,1:10])
pca.train <- cbind(as.data.frame(pca.train), quality = train2$quality)

pca.lm <- lm(quality ~ ., data = pca.train)
lin.wine.pca.mse <- mean((predict(pca.lm, newdata = pca.valid) - valid2[,12])^2)

#m <- matrix(c(princomp(train2[,-12], data = train2)$scores[,1], princomp(train2[,-12], data = train2)$scores[,2], train2$quality), ncol = 3)
#plot3d(m[,1], m[,2], m[,3])

# vi körde PCA, alla attribut är relevanta så vi behåller alla! Ursprunglig LM -> Alla attribut signfikanta, behåller alla!
# PCA-graf avslöjar inget...

# AIC, osv?

require(MASS)
require(lars)
require(rgl)

# ridge.models <- lm.ridge(quality ~ ., lambda = seq(0, 20), data = train2)
# plot(ridge.models) - verkar inte vara så relevant modell?

lasso.models <-lars(x = as.matrix(train2[,-12]), y = as.vector(train2[,12]), type = "lasso")
plot(lasso.models)

lasso.predict <- rep(0,13)
for (i in 1:13){
  lasso.predict[i]<-mean((predict(lasso.models, as.matrix(valid2[,-12]))$fit[,i] - valid2[,12])^2)}

# vanlig lin reg är bäst

#poly.lm <- lm(quality ~ poly(fixed.acidity + volatile.acidity, degree = 4, raw = TRUE), data = train2)

binary.train2 <- train2
binary.train2$quality <- train2$quality > 6
binary.train2$quality[which(binary.train2$quality)] <- as.factor(1)
#binary.train2$quality <- as.factor(binary.train2$quality)

binary.valid2 <- valid2
binary.valid2$quality <- valid2$quality > 6
binary.valid2$quality[which(binary.valid2$quality)] <- as.factor(1)
#binary.valid2$quality <- as.factor(binary.valid2$quality)

valid.mces <- as.data.frame(matrix(nrow = 4, ncol = 5))

require(e1071)

svm.fct <- function(x){

for (j in c("linear", "polynomial", "radial", "sigmoid")){
  
  if (j == "linear"){k <- 1}
  if (j == "polynomial"){k <- 2}
  if (j == "radial"){k <- 3}
  if (j == "sigmoid"){k <- 4}
  
  for (i in c(0.01, 0.3, 3, 20, 30)){
    
    if (i == 0.01){l <- 1}
    if (i == 0.3){l <- 2}
    if (i == 3){l <- 3}
    if (i == 20){l <- 4}
    if (i == 30){l <- 5}
    
    if (x == 0){model <- svm(quality ~ ., data = binary.train2, kernel = j, cost = i)}
    if (x == 1){
      cw <- c("0" = 1, "1" = length(which(binary.train2$quality == 0))/length(which(binary.train2$quality == 1)))
      model <- svm(quality ~ ., data = binary.train2, kernel = j, cost = i, class.weights = cw)}
    pred <- predict(model, newdata = binary.valid2[,-12])
    valid.mces[k,l] <- length(which(binary.valid2[,12] != pred))/length(binary.valid2[,12])}}

  return(list(valid.mces))}