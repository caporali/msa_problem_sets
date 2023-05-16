###########################################
#######   KNN classifier  #################
###########################################

set.seed(2023)
m <- 9
n <- ceiling(m/2)
train_err_m <- matrix(rep(0, n*44), ncol = 44)
test_err_m <-matrix(rep(0, n*44), ncol = 44)
for(j in 1:44){
    train <- pendigits[-which(group_cv ==j),]
    test <- pendigits[-which(group_cv !=j),]
    x_train <- select(train, -digit)
    x_test <- select(test, -digit)
    y_train <- train$digit
    y_test <- test$digit
    for(i in 1:n){
        train_pred <- knn(x_train, x_train, y_train, 2*i-1)
        train_err_m[i,j] <- 1 - mean(train$digit == train_pred)
        test_pred <- knn(x_train, x_test, y_train, 2*i-1)
        test_err_m[i,j] <- 1 - mean(test$digit == test_pred)
    }
}
train_err <- rowMeans(train_err_m)
test_err <- rowMeans(test_err_m)
train_err
test_err
plot(x = seq(1,m,2), y= train_err,type = "b", pch=16, col = "blue", xlab = "Number of nieghboors", ylab = "Error rate", lwd = 0.5, ylim = c(0, max(max(test_err),max(train_err))*1.1))
lines(x = seq(1,m,2), y=test_err, lwd = 0.5,type = "b", pch=16, col = "red")
# Plot them? Show them as dataframe?

## Now I would like to plot the knn boundaries in the first two linear discriminant basis




#############################################
#########   QDA classifier   ################
#############################################

# Imputating for as y8 (which is constantly 0) random sample from uniform[0,1]
count_4 <- dim(filter(pendigits, digit == 4))[1]
dig_4 <- which(pendigits$digit == 4)
pendigits$y8[dig_4] <- runif(count_4)
qda_base <- qda(digit~.,pendigits)
pred <- predict(qda_base, pendigits)
train_err <- 1 - mean(pred$class == pendigits$digit)
paste0(round(mean(train_err),4)*100, '%')

test_err <- rep(0, 44)
for(i in 1:44){
    train <- filter(pendigits, group_cv != i)
    test <- filter(pendigits, group_cv == i)
    model <- qda(digit~.,train)
    pred <- predict(model, test)
    test_err[i] <- 1 - mean(pred$class == test$digit)
}
paste0(round(mean(test_err),4)*100, '%')

#################################################
#####   Regularized QDA   #######################
#################################################

# library(caret)
# x <- select(pendigits, -digit)
# y <- pendigits$digit
# qda_reg <- train(x, y, method="qda")

# Rem: alpha = 0 is LDA
library(rda)
x <- t(as.matrix(select(pendigits, -digit)))
y <- pendigits$digit
qda_reg <- rda(x, y, alpha = seq(0, 0.99, len = 5), delta = 0)
names(qda_reg)

#################################################
######  SVM   ###################################
#################################################

library(e1071)
model <- svm(digit ~ ., data = pendigits, kernel = "radial", cost = 1, gamma = 0.1)
pred <- predict(model, newdata = select(pendigits,-digit))
train_err <- 1 - mean(as.numeric(pendigits$digit) == round(pred))
train_err