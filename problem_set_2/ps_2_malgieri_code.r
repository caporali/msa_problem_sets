setwd("C:/Users/utente/Dropbox/PC/Documents/Master/Multivariate/Problem sets/problemset_2")
library(MASS)
library(cvms)
library(dplyr)
rm(list=ls())
pendigits<-read.table("data/pendigits.txt", sep=",",head=F)
names(pendigits)<-c(paste0(rep(c("x","y"),8),rep(1:8,each=2)),"digit")
lookup<-c("darkgreen",  "brown", "lightblue",  "magenta", "purple", 
                     "blue", "red", "lightgreen", "orange", "cyan")
names(lookup)<-as.character(0:9)
digit.col<-lookup[as.character(pendigits$digit)]
prop.dig = as.vector(table(pendigits$digit)) / sum(as.vector(table(pendigits$digit)))

##################### Point 1  #####################################################
lda.fit <- lda(digit ~.,data = pendigits, prior = prop.dig)
pred <- predict(lda.fit)
ftld <- as.matrix(pred$x)[,1:2] #creating a dataset with the first two linear discriminant variables
plot(LD2~LD1, data=ftld, asp=1, pch=16,col=digit.col)
# The most difficult to discriminate are brown = 1, blue = 5, and cyan = 9 
conf_mat <- confusion_matrix(targets = pendigits$digit, predictions = pred$class)
plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]], class_order = as.character(9:0),
                      add_normalized = FALSE, palette = 'Green')
# Indeed it is conspicuous that the worst performance are indeed in classifying ones and fives. 
# Also nines are classified quite poorly. Surprisingly, also eights are classified quite badly.
# In particular they are classified even worsly than nines. 

lda.CV <- lda(digit ~.,data = pendigits, CV =T)
conf_mat_cv <- confusion_matrix(targets = pendigits$digit, predictions = lda.CV$class)
conf_mat$`Overall Accuracy` - conf_mat_cv$`Overall Accuracy`
# Indeed accuracy is slightly higher in train missclassification error. But only by 0.1%, this is surprising

plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]], class_order = as.character(9:0), add_normalized = FALSE, palette = 'Green')

##############à digression: checking orthogonality of ld vars ##########
ld = as.matrix(pendigits[,-17])%*% as.matrix(lda.fit$scaling)
round(var(ld), 12)
########################################################################

################ Point 2 ##################################

groupCV<-rep(1:44, each=250)
groupCV<-groupCV[1:length(pendigits$digit)]

start_time <- proc.time()
pendigits$groupCV = groupCV
cv = c()
for(k in 1:10){
    i <- 1
    train <- filter(pendigits, groupCV != i)
    test <- filter(pendigits, groupCV == i)
    model <- lda(digit~.,train, prior = prop.dig)
    pred <- predict(model,test, dimen = k)
    data = cbind(test$digit,pred$class)
    for(i in 2:44){
        train <- filter(pendigits, groupCV != i)
        test <- filter(pendigits, groupCV == i)
        model <- lda(digit~.,train, prior = prop.dig)
        pred <- predict(model,test, dimen = k)
        data = rbind(data, cbind(test$digit,pred$class)) 
    }
    cvk = 1 - mean(test$digit == pred$class)
    cv = c(cv, cvk)
}
end_time <- proc.time()
elapsed_time <- end_time - start_time
print(elapsed_time)  
plot(1:length(cv), cv, xlab = "Discriminant variables used", ylab = "CV test error rate", pch = 21)
# The error is minimized with 8 variables. However these estimates potentially don't expolit all the information in the sample (loocv could be more accurate), and on top of that the sample information is limited. In synthesis as the margin of improvement of 8 is by a small amount, it might as well be preferable by parsimony principle (?) to only use 6 discriminant variables in order to avoid overfitting.
# This procedure run in approximately 30 seconds. This means that loocv which fits 250 times more models would take approximately 2 hours, which is not prohibitive. I could launch it during a break.   


######################## LOOCV alternative. Don't launch it by mistake! #############

# pendigits$groupCV <- NULL
# n = nrow(pendigits)
# cv = c()
# for(k in 1:10){
#     cvk = 0
#     for(i in 1:n){
#         train <- pendigits[-i,]
#         test <- pendigits[i,]
#         model <- lda(digit~.,train, prior = prop.dig)
#         pred <- predict(model,test, dimen = k)
#         cvk = cvk + as.integer(pred$class == test$digit)
#     }
#     cvk = 1 - cvk/n
#     cv = c(cv, cvk)
# }
# cv

#########################################################################################






######################################################################
# Little digression for personal study: we can get two equivalent derivations for LDS (apparently)
# The first one is pretty straightforward. Consider the the posterio P(G = g|X =x) where priors are estimated by sample frequencies of classes
# and X|G=g is normal with mean in the centroid of class g and pooled variance matrix equal for all g's. We will choose one over another if it has 
# higher posterior probability, i.e. if the ratio against alternative is > 1, i.e. if log of odds is positive. Therefore we can use as discriminant functions
# \delta_g the log of this posterior discounted of the additive constant that does not depend on g.

# Another equivalent result is given by Fisher derivation. He also assumes that Var(X | G = g) is constant, however he does not assume specifically data to be
# normally distributed. Groups have different means. (If data were normal, moreover, as variance is in common, such mean would completely identify the distribution)
# Therefore in some sense such means (the centroids) identify the distribution (completely under normality, up to second order in general). We thus care about 
# determining the correlation structure of such centroids, i.e. the matrix B. We want to determine a subspace (so a projection vector a), which discriminates maximally 
# this "between variance". helding constant the "within variance". 

#########################################################################
