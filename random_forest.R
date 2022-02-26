if(!require(randomForest)) install.packages("randomForest") 
if(!require(colorspace)) install.packages("colorspace") 
if(!require(reshape)) install.packages("reshape") 
if(!require(ggplot2)) install.packages("ggplot2")

library(randomForest)
library(colorspace) 
library(reshape) 
library(ggplot2)
library(ggRandomForests)
library(randomForestSRC) 

data(pbc) 
summary(pbc)

# Transform variable values: years to days; 0-1 to T-F
pbc1<-pbc 
pbc1$Years<-pbc$days/365 
pbc1$age<-pbc$age/365 
pbc1$Status <- NULL 
pbc1$status

# Get transformed data
pbc2<-pbc1[,2:20] 
head(pbc2)

# Reshape continuous variable data for exploratory analysis
dtb1<- melt(pbc2, id.vars=c("age","Years","status")) 
dtb2<- melt(pbc2, id.vars=c("bili","Years","status")) 
dtb3<- melt(pbc2, id.vars=c("albumin","Years","status")) 
dtb4<- melt(pbc2, id.vars=c("alk","Years","status")) 
dtb5<- melt(pbc2, id.vars=c("sgot","Years","status")) 
dtb6<- melt(pbc2, id.vars=c("prothrombin","Years","status")) 
dtb7<- melt(pbc2, id.vars=c("chol","Years","status")) 
dtb8<- melt(pbc2, id.vars=c("copper","Years","status")) 
dtb9<- melt(pbc2, id.vars=c("trig","Years","status")) 
dtb10<- melt(pbc2, id.vars=c("platelet","Years","status"))

# Plot contuinuous variables
gg1<-ggplot(data=dtb1, aes(x=Years, y=age)) +
  geom_point(aes(x=Years, color=status)) +
  scale_fill_brewer(type="seq", palette = "Set1") 
gg2<-ggplot(data=dtb2, aes(x=Years, y=bili)) +
  geom_point(aes(x=Years, color=status)) +
  scale_fill_brewer(type="seq", palette = "Set1") 
gg3<-ggplot(data=dtb3, aes(x=Years, y=albumin)) +
  geom_point(aes(x=Years, color=status)) +
  scale_fill_brewer(type="seq", palette = "Set1") 
gg4<-ggplot(data=dtb4, aes(x=Years, y=alk)) +
  geom_point(aes(x=Years, color=status)) +
  scale_fill_brewer(type="seq", palette = "Set1") 
gg5<-ggplot(data=dtb5, aes(x=Years, y=sgot)) +
  geom_point(aes(x=Years, color=status)) +
  scale_fill_brewer(type="seq", palette = "Set1") 
gg6<-ggplot(data=dtb6, aes(x=Years, y=prothrombin)) +
  geom_point(aes(x=Years, color=status)) +
  scale_fill_brewer(type="seq", palette = "Set1") 
gg7<-ggplot(data=dtb7, aes(x=Years, y=chol)) +
  geom_point(aes(x=Years, color=status)) +
  scale_fill_brewer(type="seq", palette = "Set1") 
gg8<-ggplot(data=dtb8, aes(x=Years, y=copper)) +
  geom_point(aes(x=Years, color=status)) +
  scale_fill_brewer(type="seq", palette = "Set1") 
gg9<-ggplot(data=dtb9, aes(x=Years, y=trig)) +
  geom_point(aes(x=Years, color=status)) +
  scale_fill_brewer(type="seq", palette = "Set1") 
gg10<-ggplot(data=dtb10, aes(x=Years, y=platelet)) +
  geom_point(aes(x=Years, color=status)) +
  scale_fill_brewer(type="seq", palette = "Set1")

# Show multiple pots in a window
if(require(!gridExtra)) install.packages("gridExtra")
library(gridExtra)
grid.arrange(gg1,gg2,gg3,gg4,gg5,gg6,nrow=3)

# Include only the randomized patients.
pbc.trial <- pbc2[-which(is.na(pbc2$treatment)),]

# Create a test set from the remaining patients
pbc.test <- pbc2[which(is.na(pbc2$treatment)),] 
head(pbc.trial)

# Create a test set from the remaining patients
# Create the gg_survival object
gg_dta <- gg_survival(interval = "Years", 
                      censor = "status", 
                      by = "treatment", 
                      data = pbc.trial, 
                      conf.int = .95)

#Plot the survival probability function
plot(gg_dta, lwd = 1.25) + labs(y = "Survival Probability", 
                    x = "Observation Time (Years)", 
                    color = "Treatment", 
                    fill = "Treatment") + 
  theme(legend.position = c(.2,.2)) + 
  coord_cartesian(y = c(0,1.01))

# Plot the cumulative hazard function
plot(gg_dta, type="cum_haz", lwd=1.25) + 
  labs(y = "Cumulative Hazard", 
       x = "Observation Time (Years)", 
       color = "Treatment", fill = "Treatment") + 
  theme(legend.position = c(.2,.8))

# Duplicate the trial data
pbc.bili <- pbc.trial

# Group by bilirubin values
pbc.bili$bili_grp <- cut(pbc.trial$bili, 
                         breaks = c(0, .8, 1.3, 3.4, 
                                    max(pbc.trial$bili)))

# Plot the gg_survival object directly
plot(gg_survival(interval = "Years",censor = "status", 
                 by = "bili_grp", data = pbc.bili), 
     lwd = 1.25, error = "none") + 
  labs(y = "Survival Probability", 
       x = "Observation Time (Years)", 
       color = "Bilirubin")
if(!require(shape2)) install.packages(shape2) 
library(reshape2)
dta <- melt(pbc2, id.vars=c("bili","Years")) 
dtb <- melt(pbc2, id.vars=c("Years","status")) 
head(dtb)

# Using shiny GUI for colorspace
choose_palette("tcltk")

# Analog to: choose_palette(gui = “shiny”)
ggplot(dta, aes(x=bili, y=Years, color=variable)) + 
  geom_point(alpha=.4) + 
  geom_rug(data=dta) + 
  labs(y="", x="bili") + 
  scale_fill_gradientn(colours = 
                         colorspace::rainbow_hcl(17)) + 
  facet_wrap(~variable, scales="free_y", ncol=3)

ggplot(dta, aes(x=bili, y=Years, color=value)) + 
  geom_point(alpha=.4) + 
  geom_rug(data=dta) + 
  labs(y="", x="bili") + 
  scale_fill_brewer (type = "seq", palette = "Set2") + 
  facet_wrap(~variable, scales="free_y", ncol=3)

# Grow and store the random survival forest
rfsrc_pbc <- rfsrc(Surv(Years, status) ~ ., 
                   data = pbc.trial)

# Use random splitting (nsplit = 10) and impute missing values (na.action = “na.impute”)
rfsrc_pbc2 <- rfsrc(Surv(Years, status) ~ ., 
                    data = pbc.trial, 
                    nsplit = 10, 
                    na.action = "na.impute")

# Print the forest summary
rfsrc_pbc
rfsrc_pbc2

# Predict survival for 106 patients not in randomized trial
pbc.test$status<-ifelse(pbc.test$status == "T",1,0) 
head(pbc.test)
rfsrc_pbc_test <- predict(rfsrc_pbc, 
                          newdata = pbc.test, 
                          na.action = "na.impute")

# Print prediction summary
rfsrc_pbc_test

# Print prediction summary
rfsrc_pbc_test2 <- predict(rfsrc_pbc2, 
                           newdata = pbc.test, 
                           na.action = "na.impute")
# Print prediction summary
rfsrc_pbc_test2

# Extract VIMP measures for each of the variables used to grow the forest.
gg_variable(rfsrc_pbc) 
gg_dta<-gg_vimp(rfsrc_pbc)
gg_dta
plot(gg_dta)

gg_dta_10<-gg_vimp(rfsrc_pbc, nvar=10)
gg_dta_10
plot(gg_dta_10)

plot(rfsrc_pbc, lbls = st.labs) + 
  theme(legend.position = c(0.8,0.2)) + 
  labs(fill = "VIMP > 0") + 
  scale_fill_brewer(palette = "Set1")

# Return an object with both minimal depth and vimp measures
varsel_pbc <- var.select(rfsrc_pbc)
ggMindepth <- gg_minimal_depth(varsel_pbc, lbls = Years) 
print(ggMindepth)
plot(ggMindepth)

# Both minimal depth and VIMP
plot(gg_minimal_vimp(ggMindepth))

# Get the minimal depth selected variables
xvar <- varsel_pbc$topvars 
xvar

# Data generation
ggrf <- gg_variable(rfsrc_pbc, time = c(1, 3), 
                    time.labels = c("1 Year", "3 Years"))

# Plot the bilirubin variable dependence plot
plot(ggrf, xvar = "bili", se = .95, alpha = .3) + 
  labs(y = "Survival", x = "bili") + 
  theme(legend.position = "none")

# Pull the categorical variables
xvar.cat <- c("edema", "stage") 
xvar <- xvar[-which(xvar %in% xvar.cat)]

# plot the next 5 continuous variable dependence plots.
plot(ggrf, xvar = xvar[2:6], panel = TRUE, 
     se = FALSE, alpha = .3, 
     method = "glm", formula = y~poly(x,2)) + 
  labs(y = "Survival") + 
  theme(legend.position = "none") #optional

# Variable dependence plots for categorical variables are constructed using boxplots to show the distribution of the predictions within each category.
plot(ggrf, xvar = xvar.cat, panel = TRUE, notch = TRUE, alpha = .3) + 
  labs(y = "Survival") + 
  scale_fill_gradientn(colours = colorspace::rainbow_hcl(17))

# Calculate the 1 and 3 year partial dependence
partial_age <- plot.variable(rfsrc_pbc, xvar.names = "age", 
                             partial=TRUE)
partial_alk <- plot.variable(rfsrc_pbc, xvar.names = "alk", 
                             partial=TRUE)
partial_alb <- plot.variable(rfsrc_pbc, xvar.names = "albumin",
                             partial=TRUE)
partial_bili <- plot.variable(rfsrc_pbc, xvar.names = "bili", 
                              partial=TRUE)
partial_chol <- plot.variable(rfsrc_pbc, xvar.names = "chol", 
                              partial=TRUE)
partial_copp <- plot.variable(rfsrc_pbc, xvar.names = "copper", 
                              partial=TRUE)
partial_trig <- plot.variable(rfsrc_pbc, xvar.names = "trig", 
                              partial=TRUE)
partial_sgot <- plot.variable(rfsrc_pbc, xvar.names = "sgot", 
                              partial=TRUE)
partial_plat <- plot.variable(rfsrc_pbc, xvar.names = "platelet", 
                              partial=TRUE)
partial_pro <- plot.variable(rfsrc_pbc, xvar.names = "prothrombin", 
                             partial=TRUE)
partial_asci <- plot.variable(rfsrc_pbc, xvar.names = "ascites",
                              partial=TRUE)
partial_edema<- plot.variable(rfsrc_pbc, xvar.names = "edema", 
                              partial=TRUE)
xvar <- ggMindepth$topvars 
plot(ggrf, xvar=xvar, panel=TRUE, alpha=.4) + labs(y="age", x="")

# Calculate the 1, 3 and 5 year partial dependence
partial_pbc <- lapply(c(1,3,5), function(tm){ 
  plot.variable(rfsrc_pbc, surv.type = "surv", 
                time = "Years", 
                xvar.names = xvar, 
                partial = TRUE, 
                show.plots = TRUE) 
})	
plot.variable(rfsrc_pbc, surv.type = "surv", 
              time = "Years", 
              xvar.names = xvar, 
              partial = TRUE, 
              show.plots = TRUE)

# Convert all partial plots to gg_partial objects
gg_dta <- lapply(partial_pbc, gg_partial)

# Combine the objects to get multiple time curves along variables on a single figure.
gg_dta <- lapply(partial_pbc, gg_partial) 
pbc_ggpart <- combine.gg_partial(gg_dta[[1]], gg_dta[[2]], lbls = c("1 Year", "3 Years")) 
#pbc_ggpart2 <- combine.gg_partial(ggRandomForests::gg_partial(gg_dta[[1]], gg_dta[[2]]), lbls = c("1 Year", "3 Years")) 
plot(pbc_ggpart[["bili"]], lwd = 3) + 
  theme(legend.position = c(.9, .85)) + 
  labs(y = "Survival", 
       x = "bili", 
       color = "Time", shape = "Time") + 
  scale_color_brewer(palette = "Set2")

# Create a temporary holder and remove the stage and edema data
ggpart <- pbc_ggpart 
ggpart$edema <- ggpart$stage <- NULL 
ggpart$bili <- ggpart$sgot <- ggpart$chol <- NULL 
ggpart$platelet <- ggpart$trig <- ggpart$alk <- NULL 

# Panel plot the remainder. 
plot(ggpart, panel = TRUE) + 
  labs(x = "", y = "Survival", color = "Time", shape = "Time") + 
  scale_color_brewer(palette = "Set2") + 
  theme(legend.position = c(.9, .15))

ggpart <- pbc_ggpart 
head(ggpart$edema)
ggpart$stage
names(ggpart) <- c("edema", "stage") 
class(ggpart) <- c("gg_partial_list", class(ggpart)) 
plot(ggpart$edema, panel=TRUE, notch = TRUE, alpha = .3, lwd = 3) + 
  labs(x = "", y = "Survival (%)", color="Time", shape="Time") + 
  scale_color_brewer(palette = "Set1") + 
  theme(legend.position = c(.89, .85))
# The gg_interaction function wraps the find.interaction matrix for use with the provided S3 plot and print functions.
interaction_pbc <- find.interaction(rfsrc_pbc)
ggint <- gg_interaction(interaction_pbc) 
plot(ggint, xvar = xvar) + 
  labs(y = "Interactive Minimal Depth") + 
  theme(legend.position = "none")

# Conditional dependence plots
ggvar <- gg_variable(rfsrc_pbc, time = 1) 
ggvar$stage <- paste("stage = ", ggvar$stage, sep = "") 
var_dep <- plot(ggvar, xvar = "bili", 
                bg = "lightblue", method = "glm", alpha = .5) + 
  labs(y = "Survival", x = "bili") + 
  theme(legend.position = c(.35, .15)) + 
  scale_color_brewer(palette = "Set2") + 
  scale_shape(solid=TRUE) + 
  coord_cartesian(y = c(-.01,1.01))
var_dep

# dependence of survival against bilirubin, versus other categorical covariates, say edema and stage.
var_dep + facet_grid(edema~stage)

# Find intervals with similar number of observations.
copper_cts <-quantile_pts(ggvar$copper, groups = 6, intervals = TRUE)

# Create the conditional groups and add to the gg_variable object
copper_grp <- cut(ggvar$copper, breaks = copper_cts) 
ggvar$copper_grp <- copper_grp

# Adjust naming for facets
levels(ggvar$copper_grp) <- paste("copper = ",levels(copper_grp), sep = "")

# plot.gg_variable
var_dep <- plot(ggvar, xvar = "bili", 
                method = "glm", alpha = .5) + 
  labs(y = "copper", x = "bili") + 
  theme(legend.position = c(.35, .15)) + 
  scale_color_brewer(palette = "Set1") + 
  scale_shape(solid=TRUE) + 
  coord_cartesian(y = c(-.01,1.01))
var_dep
chol_cts <-quantile_pts(ggvar$chol, groups = 6, intervals = TRUE) 
chol_grp <- cut(ggvar$chol, breaks = chol_cts) 
ggvar$chol_grp <- chol_grp

# Adjust naming for facets
levels(ggvar$chol_grp) <- paste("chol = ",levels(chol_grp), sep = "")

# Plot.gg_variable
var_dep <- plot(ggvar, xvar = "bili", 
                method = "glm", alpha = .5, se = FALSE) + 
  labs(y = "cholesterol", x = "bili") + 
  theme(legend.position = c(.35, .1)) + 
  scale_color_brewer(palette = "Set2") + 
  scale_shape(solid=TRUE) + 
  coord_cartesian(y = c(-.01,1.01))
var_dep

# Partial dependence coplots
data(rfsrc_pbc, package="ggRandomForests")

# Create the variable plot.
ggvar <- gg_variable(rfsrc_pbc, time = 1)

# Find intervals with similar number of observations.
copper_cts <-quantile_pts(ggvar$copper, groups = 6, intervals = TRUE)

# Create the conditional groups and add to the gg_variable object
copper_grp <- cut(ggvar$copper, breaks = copper_cts) 
partial_coplot_pbc <- gg_partial_coplot(rfsrc_pbc, 
                                        xvar = "bili", 
                                        groups = copper_grp, 
                                        surv_type = "surv", 
                                        time = 1, 
                                        show.plots = FALSE)

# so load the cached set
data(partial_coplot_pbc, package="ggRandomForests")

# Partial coplot
plot(partial_coplot_pbc) + 
  labs(x = "serum bilirubin (mg/dl)", 
       y = "Survival at 1 year (%)")
partial_coplot_pbc <- gg_partial_coplot(rfsrc_pbc, 
                                        xvar = "copper", 
                                        groups = copper_grp, 
                                        surv_type = "surv", 
                                        time = 1, 
                                        show.plots = FALSE) 

data(partial_coplot_pbc, package = "ggRandomForests")
plot(partial_coplot_pbc) + 
  labs(x = "urine copper", 
       y = "Survival at 1 year (%)", 
       color = "Bilirubin", 
       shape = "Bilirubin") + 
  scale_color_brewer(palette = "Set2")

# Read and Explore data
bank<-read.csv(file="./Banking.csv",header = T) 
bank<-data.frame(bank)
str(bank)
names(bank)
head(bank)

# Input dataset has 20 independent variables and a target variable. The target variable y is binary.
table(bank$RESP)/nrow(bank)

# Now, we will split the data sample into development and validation samples.
sample.ind <- sample(2, nrow(bank), replace = T,prob = c(0.6,0.4))
                     
#sample.ind <- sample(2, 10000, replace=F)
bank.train <- bank[sample.ind==1,]
bank.test <- bank[sample.ind==2,]
table(bank.train$RESP)/nrow(bank.train)
table(bank.test$RESP)/nrow(bank.test)
library(forcats)
bank.train$RESP=as_factor(bank.train$RESP)
bank.test$RESP=as_factor(bank.test$RESP)
bank.test$RESP

# If target variable is factor, classification decision tree is built. We can check the type of response variable.
class(bank.train$RESP)
class(bank.test$RESP)

# Make Formula
varNames <- names(bank.train)

# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("RESP")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("RESP", varNames1, sep = " ~ "))

# Building Random Forest using R
# Now, we have a sample data and formula for building Random Forest model. Let's build 500 decision trees using Random Forest.
bank.rf <- randomForest(rf.form, bank.train, ntree=500,importance=T) 
par(mfrow = c(1, 1))
plot(bank.rf, lwd = 3)

# Variable Importance Plot
varImpPlot(bank.rf, sort = T, 
           main="Variable Importance", n.var = 6,
           lwd = 2, col = "blue")
# Variable Importance Table
var.imp <- data.frame(importance(bank.rf, type=2))
# Make row names as columns
var.imp$Variables <- row.names(var.imp) 
var.imp[order(var.imp,decreasing = T),]
# Predict Response Variable Value using Random Forest
# Generic predict function can be used for predicting response variable using Random Forest object.
# Predicting response variable
bank.train$predicted.response <- predict(bank.rf, bank.train)

# Confusion Matrix
# confusionMatrix() function from caret package can be used for creating confusion matrix based on actual response variable and predicted value.

# Load Library or packages
#if(!require(e1071)) install.packages("e1071") 
#if(!require(caret)) install.packages("caret")
library(e1071)
library(caret)
# Create Confusion Matrix
confusionMatrix(bank.train$predicted.response, bank.train$RESP)
# Predicting response variable
truth <- bank.test$RESP
pred <- predict(bank.rf, bank.test)

library(ModelMetrics)
mse(pred, truth)

# Create Confusion Matrix
confusionMatrix(pred, truth)

library(tidyr)

plotdf <- pivot_longer(data.frame(ntrees=1:nrow(bank.rf$err.rate),bank.rf$err.rate),-ntrees)
ggplot(plotdf,aes(x=ntrees,y=value,col=name)) + 
  geom_line(lwd=1.5) + ggtitle("Bank Randon Forest Error Rate") + theme_bw()

# X0 and X1 are lines for error in prediction for that specific label, and OOB (your first column) is simply the average of the two. As the number of trees increase, your OOB error gets lower because you get a better prediction from more trees.

# Fit a Random Forest to the fgl data and compare with SVM
# The fgl data frame has 214 rows and 10 columns. It was collected by B. German on fragments of glass collected in forensic work.
#if(!require(MASS)) install.packages("MASS") 
library(MASS) 
data(fgl) 
set.seed(17) 
fgl.rf <- randomForest(type ~ ., data = fgl, mtry = 2, 
                       importance = TRUE, do.trace = 100)
print(fgl.rf)
# We can compare random forests with support vector machines by doing ten repetitions of 10-fold cross-validation, using the errorest functions in the ipred package. #errorest performs resampling based estimates of prediction error: misclassification error, root mean squared error or Brier score for survival data.
#if(!require(ipred)) install.packages("ipred") 
library(ipred) 
set.seed(131) 
error.RF <- numeric(10) 
for(i in 1:10) 
  error.RF[i] <- errorest(type ~ ., data = fgl, 
                          model = randomForest, mtry = 2)$error 
summary(error.RF)
# SVM
set.seed(563) 
error.SVM <- numeric(10) 
for (i in 1:10) 
  error.SVM[i] <- errorest(type ~ ., data = fgl, 
                           model = svm, cost = 10, gamma = 1.5)$error 
summary(error.SVM)
# We see that the random forest compares quite favorably with SVM. We have found that the variable importance measures produced by random forests can sometimes be useful for model reduction
par(mfrow = c(2, 2)) 
for (i in 1:4) 
  plot(sort(fgl.rf$importance[,i], dec = TRUE), 
       type = "h", lwd = 3, col = "blue", 
       main = paste("Measure", i))

plotdf <- pivot_longer(data.frame(ntrees=1:nrow(fgl.rf$err.rate),fgl.rf$err.rate),-ntrees)
ggplot(plotdf,aes(x=ntrees,y=value,col=name)) + 
  geom_line(lwd=1) + ggtitle("FLG Rand Forest Error Rate") + theme_bw()

# X0 and X1 are lines for error in prediction for that specific label, and OOB (your first column) is simply the average of the two. As the number of trees increase, your OOB error gets lower because you get a better prediction from more trees.

# Random Forest in R example with Low Birth Weight
lwt<-read.csv(file="C:/Users/jeff/Documents/VIT_Course_Material/Data_Analytics_2018/data/lowbwt.csv",header = T) 
lwt$LOW=as.factor(lwt$LOW)
class(lwt$LOW)
lwt.rf<- randomForest(LOW ~ BIRTH+AGE+RACE+LWT+SMOKE+BWT, 
                      data = lwt, ntree=60, mtry = 2, 
                      importance = TRUE, do.trace = 100)
print(lwt.rf)
lwt.rf$importance
varImpPlot(lwt.rf)


par(mfrow = c(1, 2))
plot(lwt.rf$err.rate, main = "Error Rate", lwd = 2, col = "blue")
plot(lwt.rf$votes, main = "Votes", lwd = 2, col = "blue")

library(tidyr)

plotdf <- pivot_longer(data.frame(ntrees=1:nrow(lwt.rf$err.rate),lwt.rf$err.rate),-ntrees)
ggplot(plotdf,aes(x=ntrees,y=value,col=name)) + 
  geom_line(lwd=1.5) + ggtitle("Low Birth Weight Rand Forrest Error Rate") + theme_bw()

# X0 and X1 are lines for error in prediction for that specific label, and OOB (your first column) is simply the average of the two. As the number of trees increase, your OOB error gets lower because you get a better prediction from more trees.

# set.seed(131) error.RF <- numeric(10) for(i in 1:10) error.RF[i] <- errorest(LOW ~ BIRTH+AGE+RACE+LWT+SMOKE+BWT, data = lwt, model = randomForest, mtry = 2)$error
summary(error.RF)
## Min. 1st Qu. Median Mean 3rd Qu. Max. ## 0.04250 0.04310 0.04383 0.04385 0.04473 0.04507
set.seed(563) 
error.SVM <- numeric(10) 
for (i in 1:10) 
  error.SVM[i] <- errorest(LOW ~ BIRTH+AGE+RACE+LWT+SMOKE+BWT, 
                           data = lwt, model = svm, cost = 10, gamma = 1.5)$error 
summary(error.SVM)
# Random Forest in R example with IRIS Data
# Split iris data to Training data and testing data
ind <- sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3)) 
trainData <- iris[ind==1,] 
testData <- iris[ind==2,]
# Generate Random Forest learning treee
iris_rf <- randomForest(Species~.,data=trainData,ntree=100,proximity=TRUE) 
table(predict(iris_rf),trainData$Species)
# Try to print Random Forest model and see the importance features
print(iris_rf)
plot(iris_rf, lwd=3)
# Plot of chunk unnamed-chunk-4
importance(iris_rf)
varImpPlot(iris_rf, col = "blue", lwd = 2)
# Plot of chunk unnamed-chunk-4
# Try to build random forest for testing data
irisPred<-predict(iris_rf,newdata=testData) 
table(irisPred, testData$Species)
# Try to see the margin, positive or negative, if positif it means correct classification
plot(margin(testData$Species))
# Plot of chunk unnamed-chunk-6
# Try to tune Random Forest
tune.rf <- tuneRF(iris[,-5],iris[,5], stepFactor=0.5)
# Plot of chunk unnamed-chunk-7
print(tune.rf)
# This script trains a Random Forest model based on the data, saves a sample submission, and plots the relative importance of the variables in making predictions
# Download 1_random_forest_r_submission.csv through https://www.kaggle.com/c/titanic-gettingStarted/submissions/attach
set.seed(1) 
train <- read.csv("C:/Users/jeff/Documents/VIT_Course_Material/Data_Analytics_2018/data/titanic_train.csv", stringsAsFactors=FALSE) 
test <- read.csv("C:/Users/jeff/Documents/VIT_Course_Material/Data_Analytics_2018/data/titanic_test.csv", stringsAsFactors=FALSE)
extractFeatures <- function(data) { 
  features <- c("Pclass", 
                "Age", 
                "Sex", 
                "Parch",
                "SibSp", 
                "Fare", 
                "Embarked") 
  fea <- data[,features] 
  fea$Age[is.na(fea$Age)] <- -1 
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE) 
  fea$Embarked[fea$Embarked==""] = "S" 
  fea$Sex <- as.factor(fea$Sex) 
  fea$Embarked <- as.factor(fea$Embarked) 
  return(fea) 
}
rf <- randomForest(extractFeatures(train), 
                   as.factor(train$Survived), 
                   ntree = 100, importance=TRUE)
submission <- data.frame(PassengerId = test$PassengerId) 
submission$Survived <- predict(rf, extractFeatures(test))
imp <- importance(rf, type = 1) 
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1]) 
p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), 
                                   y = Importance)) + 
  geom_bar(stat = "identity", fill = "#53cfff") + 
  coord_flip() + theme_light(base_size = 20) + 
  xlab("") + 
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") + 
  theme(plot.title=element_text(size = 18)) 
p

