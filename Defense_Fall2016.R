#######################################
# Data Science Final Exam - Fall 2016 #
#######################################
library(class)
library(boot)
library(tree)
library(randomForest)
library(car)
library(ROCR)
library(leaps)
library(MASS)
library(glmnet)
library(elasticnet)
library(caret)
library(leaps)
library("e1071")
library(knitr)
library(ggplot2)
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(animation)
library(dplyr)
library(viridis)
library(plotly)
library(reshape2)
library(d3heatmap)
library(lars)
library(mclust)

# Loading In #

Data <- read.csv(file.choose())
dim(Data)
summary(Data)
ls.str(Data)
pairs(modDat)
###########Splitting
modDat <- Data[complete.cases(Data),]
testData <- Data[!complete.cases(Data),]

################
######Data Preparation
###############

#Initial cleaning:
Data$game_id<- NULL
Data$game_event_id<- NULL
Data$team_id<- NULL
Data$shot_id<- NULL

Data$team_name<- NULL
Data$game_date<-NULL
Data$action_type<-NULL

Data$total_seconds_remaining <- (Data$minutes_remaining*(60) + Data$seconds_remaining)
Data$minutes_remaining<-NULL
Data$seconds_remaining<- NULL
Data$lat<-NULL
Data$lon<-NULL
Data$opponent<-NULL
Data$playoffs<- factor(Data$playoffs)
Data$shot_made_flag<- factor(Data$shot_made_flag)

#################
####Data Visualization
################
########combined_shot_type
settings <- list(par.main.text = list(font = 1, just = "left", x = grid::unit(5, "mm"))
                 , par.sub.text = list(font = 2, just = "left", x = grid::unit(5, "mm")))
barchart(sort(table(Data$combined_shot_type))
         , col = "#0080ff"
         , border = "transparent"
         , xlim = c(0, 27000)
         , xlab = "Number of shots"
         , main = "Number of shots by shot type"
         , par.settings = settings
         , panel = function(...){
           panel.abline(v = seq(0, 26000, 1000), col = "gray90")
           args <- list(...)
           panel.text(
             args$x, args$y, paste0(args$x, " (", round(prop.table(args$x), 3)*100, "%)")
             , pos = 4)
           panel.barchart(...)})
# by location
#combined_shot_type vs shot_made_flag
data=modDat
ggplot(data=data,aes(data$lon,data$lat)) + 
  geom_point(aes(color=data$combined_shot_type),alpha=1.0,size=2) + 
  ylim(33.6,34.2) + ggtitle("Combined Shot Type Distribution")+
  theme(plot.title = element_text(size="25",colour="black"))

#########lat and lon vs  loc_x  and loc_y
#lat and lon vs shot_made_flag
Data %>% ggplot(aes(lon, lat, color=shot_made_flag), main='lon and lat') + geom_point()

#loc_x and loc_y vs shot_made_flag
Data %>% ggplot(aes(loc_x, loc_y, color=shot_made_flag)) + geom_point()

##########Period

devel2 <- as.data.frame(xtabs(~ period + combined_shot_type, modDat))

devel2$combined_shot_type <- factor(
  devel2$combined_shot_type, levels(devel2$combined_shot_type)[c(1, 3, 6, 2, 5, 4)])

levels(devel2$period) <-
  c("1 period", "2 period", "3 period", "4 period", "1 overtime", "2 overtime", "3 overtime")

levels(devel2$period) <-
  paste0(levels(devel2$period), ": "
         , table(modDat$period), " (", round(prop.table(table(modDat$period)), 4)*100, "%)")

barchart(combined_shot_type ~ Freq | period
         , scales = list(x = "free")
         , strip = strip.custom(bg = "white")
         , xlab = "Number of shots"
         , main = "Number of shots by period"
         , par.settings = settings
         , col = "#0080ff"
         , border = "transparent"
         , xlim = list(
           c(0, 11000), c(0, 8000), c(0, 11000), c(0, 10000), c(0, 400), c(0, 50), c(0, 8))
         , layout = c(4, 2)
         , devel2
         , panel = function(...){
           args <- list(...)
           panel.text(
             args$x, args$y, paste0(args$x, " (", round(prop.table(args$x), 3)*100, "%)")
             , pos = 4)
           panel.barchart(...)})

##########Playoffs

devel1 <- as.data.frame(xtabs(~ playoffs + combined_shot_type, modDat))

devel1$combined_shot_type <- factor(
  devel1$combined_shot_type, levels(devel1$combined_shot_type)[c(1, 3, 6, 2, 5, 4)])

levels(devel1$playoffs) <- c("Regular season", "Playoffs")

levels(devel1$playoffs) <-
  paste0(levels(devel1$playoffs), ": "
         , table(modDat$playoffs), " (", round(prop.table(table(modDat$playoffs)), 4)*100, "%)")

barchart(combined_shot_type ~ Freq | playoffs
         , col = "#0080ff"
         , border = "transparent"
         , scales = list(x = "free")
         , xlim = list(c(0, 27000), c(0, 4500))
         , strip = strip.custom(bg = "white")
         , xlab = "Number of shots"
         , main = "Number of shots by regular season and playoffs"
         , par.settings = settings
         , devel1
         , panel = function(...){
           args <- list(...)
           panel.text(
             args$x, args$y, paste0(args$x, " (", round(prop.table(args$x), 3)*100, "%)")
             , pos = 4)
           panel.barchart(...)})

######Season
devel3 <- modDat

devel3$season <- factor(devel3$season, levels(devel3$season)[20:1])

barchart(devel3$season
         , col = "#0080ff"
         , border = "transparent"
         , xlim = c(0, 3000)
         , xlab = "Number of shots"
         , main = "Number of shots by season"
         , par.settings = settings
         , panel = function(...){
           panel.abline(v = seq(0, 3000, 100), col = "gray90")
           args <- list(...)
           panel.text(
             args$x, args$y, paste0(args$x, " (", round(prop.table(args$x), 3)*100, "%)")
             , pos = 4)
           panel.barchart(...)})

#####Shot_zone_area

t1 <- modDat %>%
  group_by(opponent, shot_zone_area) %>%
  summarise(hit.rate = round(sum(shot_made_flag=="1")/n(),2))

ggplot(data=t1, aes(y=opponent, x=shot_zone_area)) + 
  geom_tile(col="white", aes(fill=hit.rate)) + scale_fill_viridis() + 
  theme_classic()

####Shot_zone_basic

t2 <- modDat %>%
  group_by(opponent, shot_zone_basic) %>%
  summarise(hit.rate = round(sum(shot_made_flag=="1")/n(),2))

ggplot(data=t2, aes(y=opponent, x=shot_zone_basic)) + 
  geom_tile(col="white", aes(fill=hit.rate)) + scale_fill_viridis() + 
  theme_classic()

####Shot_zone_range

t3 <- modDat %>%
  group_by(shot_distance, shot_distance) %>%
  summarise(hit.rate = round(sum(shot_made_flag=="1")/n(),2))

ggplot(data=t3, aes(y=shot_made_flag, x=shot_distance)) + 
  geom_tile(col="white", aes(fill=hit.rate)) + scale_fill_viridis() + 
  theme_classic()

####Shot_type
xyplot(loc_y ~ loc_x, groups = shot_type
       , type = c("p", "g")
       , pch = 19
       , col = c("#0080ff", "gray")
       , xlab = "Longitude"
       , ylab = "Latitude"
       , main = "Shots"
       , key = list(text = list(c("2PT Field Goal", "3PT Field Goal")), points = TRUE
                    , col = c("#0080ff", "gray") , pch = 19, columns = 2)
       , modDat)


#combined_shot_type vs shot_distance
#loc_x and loc_y vs shot_made_flag
Data %>% ggplot(aes(loc_x, loc_y, color=shot_distance)) + geom_point()


#############
# Splitting #
#############
Data$lat<-NULL
Data$lon<-NULL
Data$opponent<-NULL
Data$playoffs<-factor(Data$playoffs)
# Split into complete and incomplete cases
modDat <- Data[complete.cases(Data),]
testData <- Data[!complete.cases(Data),]
set.seed(702)
train_rows <- sample(1:nrow(modDat), nrow(modDat)*.75)
train <- modDat[train_rows,]
test <- modDat[-train_rows,]

#####################
#####Variable Selection
#####################
#  1. Step 0.2298441

f= shot_made_flag~combined_shot_type+loc_x+loc_y+period+playoffs+season+shot_distance+shot_type+shot_zone_area+
  shot_zone_basic+shot_zone_range+matchup+total_seconds_remaining
set.seed(702)
modfull <- glm(f, data=train, family=binomial)
summary(modfull)

step <- stepAIC(modfull, direction="both")
#Step:  AIC=24994.01
step_formula<-shot_made_flag ~ combined_shot_type + loc_x + loc_y + period + 
              season + shot_distance + shot_zone_basic + shot_zone_range + 
               total_seconds_remaining + shot_distance:shot_zone_range

smodfull <- glm(step_formula, data=modDat, family=binomial)
summary(modfull)
cv.err <- cv.glm(modDat, modfull, K = 10)
summary(cv.err)
cv.err$delta[2] # 0.2298441

###2. LASSO
a<-with(c(modDat), data.frame(shot_made_flag,combined_shot_type,loc_x,loc_y,period,playoffs,season,shot_distance,shot_type,shot_zone_area,
                                shot_zone_basic,shot_zone_range,matchup,total_seconds_remaining,period:total_seconds_remaining,
                                shot_distance:shot_zone_range,loc_y:period,combined_shot_type:period))
glm.data<-model.matrix(f,modDat)

y<-data.matrix(modDat$shot_made_flag)
x<-data.matrix(modDat[ ,!(colnames(train) == "shot_made_flag")])

x<- glm.data[ ,!(colnames(train) == "shot_made_flag")]
y<- glm.data$shot_made_flag
set.seed(702)
train_rows <- sample(1:nrow(glm.data), nrow(glm.data)*.75)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]
fit.lasso <- glmnet(x.train, y.train,family="gaussian", alpha=1)
lbs_fun <- function(fit, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
  legend('topright', legend=labs, col=1:length(labs), lty=1) # <<< ADDED BY ME
}
coef(fit.lasso)

par(mfrow=c(1,1))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda",col=1:dim(coef(fit.lasso))[1])
lbs_fun(fit.lasso)
lasso_formula= shot_made_flag~combined_shot_type+shot_zone_range+shot_distance+
               period+total_seconds_remaining+shot_distance:shot_zone_range

###  Cross-Validation for LASSO   0.2318258

glm.data<-model.matrix(lasso_formula,modDat)

y<-data.matrix(modDat$shot_made_flag)

x<- glm.data[ ,!(colnames(train) == "shot_made_flag")]
y<- glm.data$shot_made_flag


cv.lasso = cv.glmnet(x, y,alpha=1)
plot(cv.lasso)
coef(cv.lasso)
mean(cv.lasso$cvm)



#### 3. regsubsets

regsubsets.out <-
  regsubsets(f,
             data = train,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = 17,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "forward")


reg.summary = summary(regsubsets.out)
names(reg.summary)
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp")
which.min(reg.summary$cp)
points(18, reg.summary$cp[18], pch = 20, col = "red")
coef(regsubsets.out, 18)


regsubset.formula<- shot_made_flag~combined_shot_type:period+ shot_distance:shot_zone_range+matchup+season+
  combined_shot_type+shot_zone_area+shot_zone_basic+ shot_zone_range+shot_distance

#Calculate AIC for the selected variables

regsubsets.subset <-
  regsubsets(regsubset.formula,
             data = train,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = 9,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "forward")

reg.summary = summary(regsubsets.subset)
names(reg.summary)
# [1] "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"  
bic<- reg.summary$bic
n <- length(train$shot_made_flag)
p <- apply(regsubsets.subset$which, 1, sum)

result1 <- with(train,leaps(cbind(shot_zone_area,shot_zone_basic, shot_zone_range,shot_distance,
                                  combined_shot_type:period, shot_distance:shot_zone_range,matchup,season,
                                  combined_shot_type),shot_made_flag,
                            method="r2", nbest=3)) 
result1$r2
n <- nrow(train)
SSE <- (1-result1$r2)*var(train$shot_made_flag)*(n-1)
AIC <- mean(n*log(SSE) -n*log(n) +2*result1$size) 
AIC
# AIC [1] -27991.78
#http://www.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/chapter09/Rforch09.pdf



################
##Final set of variables
##############
set.seed(702)
logit <- glm(step_formula, data = train, family = "binomial")
summary(logit)

f= as.factor(shot_made_flag)~ combined_shot_type+loc_y+period+season+total_seconds_remaining+shot_distance:shot_zone_range

f1= shot_made_flag~ combined_shot_type+loc_y+period+season+total_seconds_remaining+shot_distance:shot_zone_range

trainnull <- glm(shot_made_flag~1, data=train, family=binomial)
summary(modnull)

trainfull <- glm(f, data=train, family=binomial)
summary(modfull)

selectmod.train <- step(trainnull, scope=list(lower=formula(trainnull),upper=formula(trainfull)), direction="both")
summary(selectmod.train)


################################################################################################################

##########################################Model Selection and Building  ########################################

##################################################################################################################

#        1.Logistic regression
set.seed(702)
logit.fit <- glm(f, data=train, family=binomial)
summary(logit.fit)

log.testprob <- predict(logit.fit, newdata=test, type="response")
log.testclass <- rep("No", nrow(test))
log.testclass[log.testprob > 0.5] <- "Yes"
t=table(log.testclass, test$shot_made_flag)
## MCR = 0.3934, TP = 0.3143, TN = 0.8474
1-sum(diag(t))/sum(t)
#Logloss 0.6532015
predicted<-log.testprob
actual<- test$shot_made_flag

LogLoss<-function(actual, predicted)
{
  result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
  return(result)
}
LogLoss(actual=actual, predicted=predicted)

# Cross Validation
set.seed(702)
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
logit.fit <- glm(f, data=train, family=binomial)
cv.err <- cv.glm(train, logit.fit, cost, K=10)
cv.err$delta # MCR = 0.3828



# #####################        2. LDA
set.seed(702)
lda.fit<- lda(f, data=train)
summary(lda.fit)
lda.fit
frst.pred <- predict(lda.fit, newdata=test, type="response")
t=table(test$shot_made_flag, frst.pred$class)
## MCR = 0.3961, TP = 0.6265, TN = 0.5983
1-sum(diag(t))/sum(t) 
#Log loss 0.6548
LogLoss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}
LogLoss(test$shot_made_flag,frst.pred$posterior[,2])

# Cross validation #0.3852
vlda = function(v,formula,data,cl){
  require(MASS)
  grps = cut(1:nrow(data),v,labels=FALSE)[sample(1:nrow(data))]
  pred = lapply(1:v,function(i,formula,data){
    omit = which(grps == i)
    z = lda(formula,data=data[-omit,])
    predict(z,data[omit,])
  },formula,data)
  
  wh = unlist(lapply(pred,function(pp)pp$class))
  table(wh,cl[order(grps)])
}

tt=vlda(10,f,modDat,modDat$shot_made_flag) 
error = sum(tt[row(tt) != col(tt)]) / sum(tt)
error

################### 3. SVM
set.seed(702)
svm_model <- svm(f, data=train,kernel='radial')
summary(svm_model)

tune.out = tune(svm, f,
                data = train, kernel = "radial",
                ranges = list(cost = 10^seq(-2, 1, by = 0.25),
                              gamma = c(.5,1,2)))
summary(tune.out)
tuneResult <- tune(svm, f,  data = train,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 1
                   )
)
svm_model_after_tune <- svm(f, data=train, kernel="radial", cost=1, gamma=0.3823685,probability=TRUE)

pred <- predict(svm_model_after_tune, newdata=test, decision.values = TRUE)
pred1=attr(pred, "decision.values")[,1]
LogLoss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}
LogLoss(test$shot_made_flag,pred1)


svm.fit <- svm(f, data = train, kernel = "linear", cost = 0.01, probability = TRUE)
pred.svm <- predict(svm.fit, newdata = test, type = "response", probability = TRUE)
probs.svm = attr(pred.svm, "probabilities")[,1]

hpred.svm2 = attr(frst.pred,"probabilities")
#Then this gets the column predicting '1'
pred.svm3 = pred.svm2[,1]

frst.pred <- predict(svm_model_after_tune, newdata=test, type="response")
# Logloss 1.116707
LogLoss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}
LogLoss(test$shot_made_flag,pred1)

log.testclass <- rep("No", nrow(test))
log.testclass[frst.pred > 0.5] <- "Yes"
t=table(log.testclass, test$shot_made_flag)
## MCR = 0.3937, TP = 0.3147383, TN = 0.8466
1-sum(diag(t))/sum(t)

tune.out1 = tune(svm, f,
                data = modDat, kernel = "radial",
                ranges = list(cost = 10^seq(-2, 1, by = 0.25),
                              gamma = c(.5,1,2))) # cross- validation - 0.3893

##################  4. RandomForest
# Algorithm Tune (tuneRF)
train1<- train[c(1,3,4,6,15)]
#data.frame(c(1,3,4,6,15),train)
trainx<- train1[ ,!(colnames(train1) == "shot_made_flag")]
trainy<- train$shot_made_flag
set.seed(702)
bestmtry <- tuneRF(trainx, trainy, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry) #Best mtry =1, OOBERROR = 0.2303

set.seed(702)
frst.one <- randomForest(f1, data=train, mtry=1, importance=TRUE)
frst.one #OOB 38.36

frst.pred <- predict(frst.one, newdata=test, type="class")
log.testclass <- rep("No", nrow(test))
log.testclass[frst.pred > 0.5] <- "Yes"
t=table(frst.pred, test$shot_made_flag)
## MCR = 0.394085, TP = 0.3185, TN = 0.8429
1-sum(diag(t))/sum(t)
LogLoss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}
LogLoss(test$shot_made_flag,frst.pred)


rf<-rfcv(train1,as.factor(train$shot_made_flag),cv.fold=10)
rf$error.cv #0.3864

##################  5. Bagging

set.seed(702)
bag.mod <- randomForest(f1, data=train, mtry=6, importance=TRUE)
bag.mod

bag.pred <- predict(bag.mod, newdata=test, type="response")

LogLoss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}
LogLoss(test$shot_made_flag,bag.pred)

log.testclass <- rep("No", nrow(test))
log.testclass[bag.pred > 0.5] <- "Yes"
t=table(bag.pred, test$shot_made_flag)
## MCR = 0.4275, TP = 0.3997, TN = 0.714
1-sum(diag(t))/sum(t)

#cv 0.4086
rf<-rfcv(train1,as.factor(train$shot_made_flag),cv.fold=10)
rf$error.cv ##cv 0.4086 for all the predictors

##################################
# Logistic regression submission #
##################################

log.mod <- glm(shot_made_flag ~ f, data=modDat, family=binomial)
NA.pred <- predict(logit.fit, newdata=testData, type="response")
NA.pred <- data.frame(shot_id=row.names(testData), shot_made_flag=NA.pred)
write.csv(NA.pred, file="C:/Users/Asus/Documents/MEGA/Data Science/Defense/logpreds1.csv", row.names=FALSE)
