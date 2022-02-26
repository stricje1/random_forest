---
title: "Random Forest"
author: "Jeffrey Strickland"
date: "2/24/2022"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE, fig.width=6, fig.height=4)
```

## Random Forest using R
Random forests or random decision forests are an ensemble learning method for classification, regression and other tasks, that operate by constructing a multitude of decision trees at training time and outputting the class that is the mode of the classes (classification) or mean prediction (regression) of the individual trees. Random decision forests correct for decision trees' habit of overfitting to their training set. The source data for this and the following examples are at: https://github.com/stricje1/random_forest.

The training algorithm for random forests applies the general technique of bootstrap aggregating, or bagging, to tree learners. Given a training set `X = x_1,…,x_n `with responses `Y = y_1,…,y_n`, bagging repeatedly (B times) selects random samples with replacement of the training set and fits trees to these samples:
1.	Each sample may be comprised of different mixes members of the population, i.e., sample will have some members in common and some will be different
2.	For `n=1,…,N`, and `m=1,…,M`, there will be `N` training sets comprised of m members sampled with replacement
3.	Each sample may be comprised of a different mix of variables from the original set `X`
4.	Each tree (regression or classification) will train using one of the `X_i` training sets
5.	After training, predictions are made by either averaging of the prediction from individual trees or by majority vote in the case of classification trees

Random Forest algorithm is built in `randomForest` package of R and same name function allows us to use the Random Forest in R.

###Load libraries

```{r}
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
```

Some of the commonly used parameters of randomForest functions are 
•	`x`: Random Forest Formula 
•	`data`: Input data frame 
•	`ntree`: Number of decision trees to be grown 
•	`replace`: Takes True and False and indicates whether to take sample with/without replacement 
•	`sampsize`: Sample size to be drawn from the input data for growing decision tree 
•	`importance`: Whether independent variable importance in random forest be assessed 
•	`proximity`: Whether to calculate proximity measures between rows of a data frame

## Medical longevity Study of primary biliary cirrhosis (PBC)
Data was obtained from a Mayo Clinic randomized trial in primary biliary cirrhosis (PBC) of the liver conducted between 1974 and 1984. A total of 424 PBC patients, referred to Mayo Clinic during that ten-year interval met eligibility criteria for the randomized placebo-controlled trial of the drug D-penicillamine (DPCA). The data and partial likelihood model is described in Fleming and Harrington (1991). The variables in the data set are:
•	case number 
•	number of days between registration and the earlier of death, transplantation, or study analysis time in July 1986 
•	status: 0=alive, 1=liver transplant, 2=dead 
•	drug: 1= D-penicillamine, 2=placebo 
•	age in days 
•	sex: 0=male, 1=female 
•	presence of ascites: 0=no 1=yes 
•	presence of hepatomegaly 0=no 1=yes 
•	presence of spiders 0=no 1=yes 
•	presence of edema 0=no edema and no diuretic therapy for edema; .5 = edema present without diuretics, or edema resolved by diuretics; 1 = edema despite diuretic therapy 
•	serum bilirubin in mg/dl 
•	serum cholesterol in mg/dl 
•	albumin in gm/dl 
•	urine copper in ug/day 
•	alkaline phosphatase in U/liter 
•	SGOT in U/ml 
•	triglycerides in mg/dl 
•	platelets per cubic ml / 1000 
•	prothrombin time in seconds 
•	histologic stage of disease 

```{r}
data(pbc) 
summary(pbc)
```

The first thing we want to do is transform variable values: years to days; 0-1 to T-F, transform age to years, and impute missing statuses.

```{r}
pbc1<-pbc 
pbc1$Years<-pbc$days/365 
pbc1$age<-pbc$age/365 
pbc1$Status <- NULL 
pbc1$status
```

The output for the last step was omitted but you might want to print it out. Now we pull in the transformed data

# Get transformed data

```{r}
pbc2<-pbc1[,2:20] 
head(pbc2)
```

## Reshaping Variable
Let’s talk briefly about reshaping data. Data Reshaping in R is something like arranged rows and columns in our own way to use it as per our requirements. We take most of the data we use as a data frame format in R to do data processing using functions like `rbind(`), `cbind()`, etc. (Smith, 2020) In this process, we reshape or re-organize the data into rows and columns. Reshaped data is reorganized data transformed a particular way which we need the data for further processing. Now we'll reshape continuous variables data for exploratory analysis, using the `melt()` function. 

If you have used R before, you may recall a package called `reshape2`. Now, `tidyr` supersedes `reshape2` (2010-2014) and `reshape` (2005-2010).` tidyr` is designed specifically for tidying data, not general reshaping (`reshape2`), or the general aggregation (`reshape`). The `data.table` package provides high-performance implementations of `melt()` and `dcast()`. However, we used `reshape2` here and it seems to still work.

```{r}
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
```

# Plot contuinuous variables
Next, we define 10 ggplots, one for each variable and year, (x,year). These will come in handy later.

```{r}
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
```

# Show multiple pots in a window
Here we demonstrate one way to print more than one plot in one window, simultaneously (there are other ways to do this). So, we will use the ggplots that we just defined shown below. 

```{r}
if(require(!gridExtra)) install.packages("gridExtra")
library(gridExtra)
grid.arrange(gg1,gg2,gg3,gg4,gg5,gg6,nrow=2)
```

### Create Trial and Test Sets 
Now, we create a trial using only randomized patients and a test set using the remaining patients, i.e., those that were not randomized. We will also create a survival object and plot the survival probability function.

```{r}
pbc.trial <- pbc2[-which(is.na(pbc2$treatment)),]
pbc.test <- pbc2[which(is.na(pbc2$treatment)),] 
head(pbc.trial)
```

###Create the Survival Probability Function
Here, we use gg_survival to create an object representing the probability of surviving PBC.

```{r}
gg_dta <- gg_survival(interval = "Years", 
                      censor = "status", 
                      by = "treatment", 
                      data = pbc.trial, 
                      conf.int = .95)
```

### Plot the survival Probability Function
We now plot the probability function we just created. This will reflect both the treatment and the placebo (or treatment 2) and will show a range of values that increase over time. That is, we will see there is greater uncertainty regarding a patient's survival as the observation time increase.

```{r}
plot(gg_dta) + labs(y = "Survival Probability", 
                    x = "Observation Time (Years)", 
                    color = "Treatment", fill = "Treatment") + 
  theme(legend.position = c(.2,.2)) + 
  coord_cartesian(y = c(0,1.01))
```

# Plot the cumulative hazard function
Now, we plot the cumulative hazard function for the treatment and placebo.

```{r}
plot(gg_dta, type="cum_haz") + 
  labs(y = "Cumulative Hazard", 
       x = "Observation Time (Years)", 
       color = "Treatment", fill = "Treatment") + 
  theme(legend.position = c(.2,.8))
```

# Duplicate the trial data
Now, we want to duplicate the trial data and group it by bilirubin values. Bilirubin forms due to the natural breakdown of hemoglobin in red blood cells, so too much bilirubin in the urine or bloodstream is not a good thing. It indicates problems with your red blood cells or with your liver.

```{r}
pbc.bili <- pbc.trial
pbc.bili$bili_grp <- cut(pbc.trial$bili, 
                         breaks = c(0, .8, 1.3, 3.4, 
                                    max(pbc.trial$bili)))
```

# Plot the gg_survival object directly
When now plot the survival probability for four different values of bilirubin. The normal adult range is 0.8 to 1.3 mg/dL. If you are in the high range, you can develop jaundice. 

```{r}
plot(gg_survival(interval = "Years",censor = "status", 
                 by = "bili_grp", data = pbc.bili), 
     error = "none") + 
  labs(y = "Survival Probability", 
       x = "Observation Time (Years)", 
       color = "Bilirubin")
if(!require(reshape2)) install.packages(reshape2) 
library(reshape2)
dta <- melt(pbc2, id.vars=c("bili","Years")) 
dtb <- melt(pbc2, id.vars=c("Years","status")) 
head(dtb)
```

# Analog to: choose_palette(gui = “shiny”)
We’ll talk more about building Shiny apps and their GUI. For now, Shiny carries out mapping between assorted color spaces including RGB, HSV, HLS, CIEXYZ, CIELUV, HCL (polar CIELUV), CIELAB and polar CIELAB. Qualitative, sequential, and diverging color palettes based on HCL colors are provided along with corresponding.

```{r}
library(colorspace)
ggplot(dta, aes(x=bili, y=Years, color=variable)) + 
  geom_point(alpha=.4) + 
  geom_rug(data=dta) + 
  labs(y="", x="bili") + 
  scale_fill_gradientn(colours = 
                         colorspace::rainbow_hcl(17)) + 
  facet_wrap(~variable, scales="free_y", ncol=3)
```

The plots in  gave us nice colors, but little information. So, let’s look using shades of blue.

Now we repeat the plots with shade of blue as seen

```{r}
ggplot(dta, aes(x=bili, y=Years, color=value)) + 
  geom_point(alpha=.4) + 
  geom_rug(data=dta) + 
  labs(y="", x="bili") + 
  scale_fill_brewer (type = "seq", palette = "Set2") + 
  facet_wrap(~variable, scales="free_y", ncol=3)
```

# Grow and store the random survival forest
Bagging (Bootstrap Aggregating) generates `m` new training data sets. Each new training data set picks a sample of observations with replacement (bootstrap sample) from original data set. By sampling with replacement, some observations may be repeated in each new training data set. The `m` models are fitted using the above `m` bootstrap samples and combined by averaging the output (for regression) or voting (for classification).

Out-of-Bag error (miscalculation rate) is equivalent to validation or test data. In random forests, there is no need for a separate test set to validate result. It is estimated internally, during the run, as follows:
As the forest is built on training data , each tree is tested on the 1/3rd of the samples (36.8%) not used in building that tree (similar to validation data set). This is the out of bag error estimate - an internal error estimate of a random forest as it is being constructed. 

Fast Unified Random Forests for Survival, Regression, and Classification (RF-SRC) uses fast OpenMP parallel computing of random forests for regression, classification, survival analysis, competing risks, multivariate, unsupervised, quantile regression, and class imbalanced q-classification. Different splitting rules invoked under deterministic or random splitting are available for all families. Different types of variable importance (VIMP), holdout VIMP, as well as confidence regions can be calculated for single and grouped variables. Minimal depth variable selection. Fast interface for missing data imputation using a variety of different random forest methods.

1. Types of forests

There is no need to set the type of forest as the package automagically determines the underlying random forest requested from the type of outcome and the formula supplied. There are several possible scenarios:

a. Regression forests for continuous outcomes.

b. Classification forests for factor outcomes.

c. Multivariate forests for continuous and/or factor outcomes and for mixed (both type) of outcomes.

d. Unsupervised forests when there is no outcome.

e. Survival forests for right-censored survival.

f. Competing risk survival forests for competing risk.

2. Splitting

a. Splitting rules are specified by the option splitrule.

b. For all families, pure random splitting can be invoked by setting splitrule="random".

c. For all families, computational speed can be increased using randomized splitting invoked by the option nsplit. See Improving Computational Speed.

```{r}
rfsrc_pbc <- rfsrc(Surv(Years, status) ~ ., 
                   data = pbc.trial)
```

Use random splitting (nsplit = 10) and impute missing values (na.action = “na.impute”)

```{r}
rfsrc_pbc2 <- rfsrc(Surv(Years, status) ~ ., 
                    data = pbc.trial, 
                    nsplit = 10, 
                    na.action = "na.impute")
```

# Print the forest summary

```{r}
summary(rfsrc_pbc)
summary(rfsrc_pbc2)
```

### Predict Patient Survival
Now, we predict the survival for 106 patients not in randomized trial:

```{r}
pbc.test$status<-ifelse(pbc.test$status == "T",1,0) 
head(pbc.test)

rfsrc_pbc_test <- predict(rfsrc_pbc, 
                          newdata = pbc.test, 
                          na.action = "na.impute")
rfsrc_pbc
rfsrc_pbc2
```

# Print prediction summary
The `print.rfsrc` function returns information on how the random forest was grown. Here the `family = "surv"` forest has `ntree = 1000` trees (the default `ntree` argument). We used `nsplit = 10` random split points to select random split rule, instead of an optimization on each variable at each split for performance reasons.

```{r}
rfsrc_pbc_test
```

Next, we setup the subsequent prediction summary:

```{r}
rfsrc_pbc_test2 <- predict(rfsrc_pbc2, 
                           newdata = pbc.test, 
                           na.action = "na.impute")
```

Then, we print the subsequent prediction summary:

```{r}
rfsrc_pbc_test2
```

# Variable Importance
Variable importance (VIMP) was originally deﬁned in CART using a measure involving surrogate variables (Breiman, H., Olshen, & Stone, 1984). The most popular VIMP method uses a prediction error approach involving "noising-up" each variable in turn. VIMP for a variable xv is the diﬀerence between prediction error when xv is noised up by randomly permuting its values, compared to prediction error under the observed values (Ishwaran H. , Kogalur, Gorodeski, & Minn, 2010).

Since VIMP is the diﬀerence in Out-of-Bag prediction error before and after permutation, a large VIMP value indicates that misspeciﬁcation detracts from the predictive accuracy in the forest. If VIMP is close to zero the variable contributes nothing to predictive accuracy, and negative values indicate the predictive accuracy improves when the variable is misspeciﬁed. In the latter case, we assume noise is more informative than the true variable. As such, we ignore variables with negative and near zero values of VIMP, relying on large positive values to indicate that the predictive power of the forest is dependent on those variables. The `gg_vimp` function extracts VIMP measures for each of the variables used to grow the forest. The `plot.gg_vimp` function shows the variables, in VIMP rank order in , labeled with the named vector in the lbls argument. Now, we extract VIMP measures for each of the variables used to grow the forest.

```{r}
gg_variable(rfsrc_pbc) 
gg_dta<-gg_vimp(rfsrc_pbc)
gg_dta
plot(gg_dta)

gg_dta_10<-gg_vimp(rfsrc_pbc, nvar=10)
gg_dta_10
plot(gg_dta_10)
```

Now we plot the VIMP the top ten variables using default settings.

```{r}
library(ggRandomForests)
cls <- sapply(pbc, class)
dta.labs <- data.frame(cbind(names = colnames(pbc), label = labels, type = cls))
# Put the "years" variable on top.
dta.labs <- rbind(dta.labs[nrow(dta.labs),], dta.labs[-nrow(dta.labs),])

st.labs <- as.character(dta.labs$label)
names(st.labs) <- rownames(dta.labs)
library(ggRandomForests)
cls <- sapply(pbc, class)
dta.labs <- data.frame(cbind(names = colnames(pbc), label = labels, type = cls))
```
# Minimal Depth
In VIMP, predictive risk factors are determined by testing the forest model prediction under alternative data settings, ranking the most important variables according to their impact on predictive ability of the forest. Another method uses inspection of the forest construction to rank variables. Minimal depth (Ishwaran H. , Kogalur, Chen, & J., 2011) (Ishwaran H. , Kogalur, Gorodeski, & Minn, 2010) assumes that variables with high impact on the prediction are those that most frequently split nodes nearest to the root node, where they partition the largest samples of the population. 

Within each tree, node levels are numbered based on their relative distance to the root of the tree (with the root at 0). Minimal depth measures important risk factors by averaging the depth of the ﬁrst split for each variable over all trees within the forest. We assume that in the metric smaller minimal depth values indicate the variable separates large groups of observations, and therefore has a large impact on the forest prediction. 

In general, to select variables according to VIMP, we examine the VIMP values, looking for some point along the ranking where there is a large diﬀerence in VIMP measures. Given minimal depth is a quantitative property of the forest construction, Ishwaran et al. (2010) also derive an analytic threshold for evidence of variable impact. A simple optimistic threshold rule uses the mean of the minimal depth distribution, classifying variables with minimal depth lower than this threshold as important in forest prediction. 

The `randomForestSRC::var.select()` function uses the minimal depth methodology for variable selection, returning an object with both minimal `depth` and `vimp` measures. The `ggRandomForests::gg_minimal_depth()` function is analogous to the `gg_vimp` function. Variables are ranked from most important at the top (minimal depth measure), to least at the bottom (maximal minimal depth)

# Return an object with both minimal depth and vimp measures

```{r}
varsel_pbc <- var.select(rfsrc_pbc)
ggMindepth <- gg_minimal_depth(varsel_pbc, lbls = Years) 
print(ggMindepth)
plot(ggMindepth)
```

# Both minimal depth and VIMP
VIMP and Minimal Depth measures use diﬀerent criteria, so we expect the variable ranking to be slightly diﬀerent. We use `gg_minimal_vimp()` function to compare rankings between minimal depth and VIMP. In this call, we plot the stored `gg_minimal_depth()` object (`gg_md`), which would be equivalent to calling `plot.gg_minimal_vimp(ggMindepth)` or `plot(gg_minimal_vimp(ggMindepth))`.

```{r}
plot(gg_minimal_vimp(ggMindepth))
```

# Get the minimal depth selected variables

```{r}
xvar <- varsel_pbc$topvars 
xvar
```

The points along the red dashed line indicate where the measures agree. Points above the red dashed line are ranked higher by VIMP than by minimal depth, indicating the variables are more sensitive to misspeciﬁcation. Those below the line have a higher minimal depth ranking, indicating they are better at dividing large portions of the population. The further the points are from the line, the more the discrepancy between measures.

# Data generation

```{r}
ggrf <- gg_variable(rfsrc_pbc, time = c(1, 3), 
                    time.labels = c("1 Year", "3 Years"))
```

# Bilirubin variable dependence plot
Here we generate variable dependence plots of survival at 1 and 3 years on bilirubin variable. Individual cases are marked with red circles (alive or censored) and green triangles (dead). Loess smooth curve with shaded 95% conﬁdence band indicates decreasing survival with increasing bilirubin.

```{r}
plot(ggrf, xvar = "bili", se = .95, alpha = .3) + 
  labs(y = "Survival", x = "bili") + 
  theme(legend.position = "none")
```

# Pull the categorical variables

```{r}
xvar.cat <- c("edema", "stage") 
xvar <- xvar[-which(xvar %in% xvar.cat)]
```

# Continuous Variable Dependence Plots.
We now generate variable dependence plots of predicted survival at 1 and 3 years on continuous variables of interest. Individual cases are marked with red circles for censored cases and green triangles for death events. Loess smooth curve indicates the survival trend with increasing values.

```{r}
plot(ggrf, xvar = xvar[2:6], panel = TRUE, 
     se = FALSE, alpha = .3, 
     method = "glm", formula = y~poly(x,2)) + 
  labs(y = "Survival") + 
  theme(legend.position = "none") #optional
```

Variable dependence plots for categorical variables are constructed using boxplots to show the distribution of the predictions within each category. Here we demonstrate variable dependence of survival 1 and 3 years on the edema categorical variable. Symbols with red circles indicate censored cases and green triangles indicate death events. Boxplots indicate distribution of predicted survival for all observations within each edema group.

```{r}
plot(ggrf, xvar = xvar.cat, panel = TRUE, notch = TRUE, alpha = .3) + 
  labs(y = "Survival") + 
  scale_fill_gradientn(colours = colorspace::rainbow_hcl(17))
```

### Partial Dependence Plot 
The partial dependence plot is a global method: The method considers all instances and gives a statement about the global relationship of a feature with the predicted outcome. We calculate the 1- and 3-year partial dependence plots for age (Figure 10 16) and alkaline phosphatase

```{r}
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
```

Categorical Features
So far, we have only thought about numerical features. For categorical features, the partial dependence is very easy to compute. For each of the categories, like “yes” or “no” for the presence of ascites we get a PDP estimate by forcing all data instances to have the same category as in Figure 10 18. For another example, if we look at the PBC dataset and are interested in the partial dependence plot for the edema, we get 3 numbers, one for each category. To compute the value for "no edema and no diuretic therapy for edema", we replace the edema of all data instances with "no edema and no diuretic therapy for edema" and average the predictions. We plot this instance along with for continuous variables in Figure 10 19 and with 1-year and 3-year in plots in Figure 10 20.

```{r}
part_var01  <- plot.variable(rfsrc_pbc, xvar.names = 
c("edema","bili"),  partial = TRUE) 
part_var02  <- plot.variable(rfsrc_pbc, xvar.names = 
c("copper","albumin"),  partial = TRUE)
part_var03  <- plot.variable(rfsrc_pbc, xvar.names = 
c("chol","trig"),  partial = TRUE)

xvar <- ggMindepth$topvars 
plot(ggrf, xvar=xvar, panel=TRUE, alpha=.4) + labs(y="age", x="")
```

Now, we calculate the 1, 3 and 5 year partial dependence

```{r}
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
```

Recall that presence of edema has three values representing categorical flags:
•	0.0 = no edema and no diuretic therapy for edema; 
•	0.5 = edema present without diuretics, or edema resolved by diuretics; 
•	1.0 = edema despite diuretic therapy
The distributions for these categories is surprisingly similar and we might need to investigate the physiological and pathophysiological aspect.

Next, we convert all partial plots to gg_partial objects.

```{r}
gg_dta <- lapply(partial_pbc, gg_partial)
```

Now, we combine the objects to get multiple time curves along variables on a single figure.

```{r}
gg_dta <- lapply(partial_pbc, gg_partial) 
pbc_ggpart <- combine.gg_partial(gg_dta[[1]], gg_dta[[2]], lbls = c("1 Year", "3 Years")) 
#pbc_ggpart2 <- combine.gg_partial(ggRandomForests::gg_partial(gg_dta[[1]], gg_dta[[2]]), lbls = c("1 Year", "3 Years")) 
plot(pbc_ggpart[["bili"]]) + 
  theme(legend.position = c(.9, .85)) + 
  labs(y = "Survival", 
       x = "bili", 
       color = "Time", shape = "Time") + 
  scale_color_brewer(palette = "Set2")
```

Let’s create a temporary holder and remove the stage and edema data.

```{r}
ggpart <- pbc_ggpart 
ggpart$edema <- ggpart$stage <- NULL 
ggpart$bili <- ggpart$sgot <- ggpart$chol <- NULL 
ggpart$platelet <- ggpart$trig <- ggpart$alk <- NULL 
```

We also panel plot the remainder. 

```{r}
plot(ggpart, panel = TRUE) + 
  labs(x = "", y = "Survival", color = "Time", shape = "Time") + 
  scale_color_brewer(palette = "Set2") + 
  theme(legend.position = c(.9, .15))
```

Now, we generate the partial dependence plot for stage and edema by survival rates at 1-year and 3-year marks.

```{r}
ggpart <- pbc_ggpart 
head(ggpart$edema)
ggpart$stage
names(ggpart) <- c("edema", "stage") 
class(ggpart) <- c("gg_partial_list", class(ggpart)) 
plot(ggpart$edema, panel=TRUE, notch = TRUE, alpha = .3) + 
  labs(x = "", y = "Survival (%)", color="Time", shape="Time") + 
  scale_color_brewer(palette = "Set1") + 
  theme(legend.position = c(.35, .1))
```

The `gg_interaction()` function wraps the `find.interaction()` matrix for use with the provided S3 plot and print functions.

```{r}
interaction_pbc <- find.interaction(rfsrc_pbc)
```

Now, we generate the interactive plots for selected variables, albumin, ascites, serum bilirubin, serum cholesterol, copper, edema, and prothrombin

```{r}
ggint <- gg_interaction(interaction_pbc) 
plot(ggint, xvar = xvar) + 
  labs(y = "Interactive Minimal Depth") + 
  theme(legend.position = "none")
```

# Conditional Dependence Plots
Now, we generate the conditional dependence plot for serum bilirubin with fitted line.

```{r}
ggvar <- gg_variable(rfsrc_pbc, time = 1) 
ggvar$stage <- paste("stage = ", ggvar$stage, sep = "") 
var_dep <- plot(ggvar, xvar = "bili", 
                method = "glm", alpha = .5, se = FALSE) + 
  labs(y = "Survival", x = "bili") + 
  theme(legend.position = c(.35, .1)) + 
  scale_color_brewer(palette = "Set2") + 
  scale_shape(solid=TRUE) + 
  coord_cartesian(y = c(-.01,1.01))
var_dep
```

We now show the conditional dependence of survival against bilirubin, versus other categorical covariates, say edema and stage.

```{r}
var_dep + facet_grid(edema~stage)
```

Furthermore, we find intervals with similar number of observations.

```{r}
copper_cts <-quantile_pts(ggvar$copper, groups = 6, intervals = TRUE)
```

Next, we create the conditional groups and add to the `gg_variable()` object.

```{r}
copper_grp <- cut(ggvar$copper, breaks = copper_cts) 
ggvar$copper_grp <- copper_grp
```

Finally, we adjust naming for facets before plotting.

```{r}
levels(ggvar$copper_grp) <- paste("copper = ",levels(copper_grp), sep = "")
```

So, the variable dependency plot we generate is serum cholesterol versus serum bilirubin.

```{r}
var_dep <- plot(ggvar, xvar = "bili", 
                method = "glm", alpha = .5, se = FALSE) + 
  labs(y = "copper", x = "bili") + 
  theme(legend.position = c(.35, .1)) + 
  scale_color_brewer(palette = "Set2") + 
  scale_shape(solid=TRUE) + 
  coord_cartesian(y = c(-.01,1.01))
var_dep
```

```{r}
chol_cts <-quantile_pts(ggvar$chol, groups = 6, intervals = TRUE) 
chol_grp <- cut(ggvar$chol, breaks = chol_cts) 
ggvar$chol_grp <- chol_grp
```

# Adjust naming for facets

```{r}
levels(ggvar$chol_grp) <- paste("chol = ",levels(chol_grp), sep = "")
```

# Plot.gg_variable

```{r}
var_dep <- plot(ggvar, xvar = "bili", 
                method = "glm", alpha = .5, se = FALSE) + 
  labs(y = "cholesterol", x = "bili") + 
  theme(legend.position = c(.35, .1)) + 
  scale_color_brewer(palette = "Set2") + 
  scale_shape(solid=TRUE) + 
  coord_cartesian(y = c(-.01,1.01))
var_dep
```

### Partial dependence coplots

```{r}
data(rfsrc_pbc, package="ggRandomForests")
```

For the partial dependence coplots, we create the variable plot.

````{r}
ggvar <- gg_variable(rfsrc_pbc, time = 1)
```

Then, we find intervals with similar number of observations.

```{r}
copper_cts <-quantile_pts(ggvar$copper, groups = 6, intervals = TRUE)
```

# Create the conditional groups and add to the gg_variable object

```{r}
copper_grp <- cut(ggvar$copper, breaks = copper_cts) 
partial_coplot_pbc <- gg_partial_coplot(rfsrc_pbc, 
                                        xvar = "bili", 
                                        groups = copper_grp, 
                                        surv_type = "surv", 
                                        time = 1, 
                                        show.plots = FALSE)
```

Now, we load the cached set.

```{r}
data(partial_coplot_pbc, package="ggRandomForests")
```

# Partial Coplot
Finally, we display the partial dependence coplots by the conditional groups.

```{r}
plot(partial_coplot_pbc) + 
  labs(x = "serum bilirubin (mg/dl)", 
       y = "Survival at 1 year (%)")
```

Next, we generate the plot of urine copper by serum bilirubin for 1-year survival percentage 

```{r}
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
```

## Business Application - Banking

###Scenario and dataset
A marketing department of a bank runs various marketing campaigns for cross-selling products, improving customer retention and customer services. In this example, the bank wanted to cross-sell term deposit product to its customers. Contacting all customers is costly and does not create good customer experience. So, the bank wanted to build a predictive model which will identify customers who are more likely to respond to term deport cross sell campaign. We will use sample Marketing Data sample for building random forest based model using R.

### Read and Explore data

```{r}
bank<-read.csv(file="./Banking.csv",header = T) 
bank<-data.frame(bank)
str(bank)
names(bank)
head(bank)
```

Input dataset has 20 independent variables and a target variable. The target variable y is binary.

```{r}
table(bank$RESP)/nrow(bank)
```

Eleven % of the observations has target variable "yes" and remaining 89% observations take value "no".

Now, we will split the data sample into development and validation samples.

```{r}
sample.ind <- sample(2, nrow(bank), replace = T,prob = c(0.6,0.4))
```

#sample.ind <- sample(2, 10000, replace=F)

```{r}
library(forcats)
bank.train <- bank[sample.ind==1,]
bank.test <- bank[sample.ind==2,]
table(bank.train$RESP)/nrow(bank.train)
table(bank.test$RESP)/nrow(bank.test)
bank.train$RESP=as_factor(bank.train$RESP)
bank.test$RESP=as_factor(bank.test$RESP)
bank.test$RESP
```

Both development and validation samples have similar target variable distribution. This is just a sample validation.

If target variable is factor, classification decision tree is built. We can check the type of response variable.

```{r}
class(bank.train$RESP)
class(bank.test$RESP)
```

Class of target or response variable is factor, so a classification Random Forest will be built. The current data frame has a list of independent variables, so we can make it formula and then pass as a parameter value for randomForest.

### Make Formula

```{r}
varNames <- names(bank.train)
```

# Exclude ID or Response variable

```{r}
varNames <- varNames[!varNames %in% c("RESP")]
```

# add + sign between exploratory variables

```{r}
varNames1 <- paste(varNames, collapse = "+")
```

# Add response variable and convert to a formula object

```{r}
rf.form <- as.formula(paste("RESP", varNames1, sep = " ~ "))
```

# Building Random Forest using R
Now, we have a sample data and formula for building Random Forest model. Let’s build 500 decision trees using Random Forest and plot the outcome.

```{r}
bank.rf <- randomForest(rf.form, bank.train, ntree=500,importance=T) 
par(mfrow = c(1, 1))
plot(bank.rf, lwd = 3)
```

500 decision trees or a forest has been built using the random forest algorithm-based learning. We can plot the error rate across decision trees. The plot seems to indicate that after 100 decision trees, there is not a significant reduction in error rate.

YES and NO are lines for error in prediction for that specific label, and OOB (our first column) is simply the weighted-average of the two. As the number of trees increase, your OOB error gets lower because you get a better prediction from more trees.

### Variable Importance Plot
Variable importance plot is also a useful tool and can be plotted using varImpPlot function. Top 5 variables are selected and plotted based on Model Accuracy and Gini value. We can also get a table with decreasing order of importance based on a measure (1 for model accuracy and 2 node impurity).

```{r}
varImpPlot(bank.rf, sort = T, 
           main="Variable Importance", n.var = 6,
           lwd = 2, col = "blue")
```

# Variable Importance Table

```{r}
var.imp <- data.frame(importance(bank.rf, type=2))
```

Here, we make row names as columns:

```{r}
var.imp$Variables <- row.names(var.imp) 
var.imp[order(var.imp,decreasing = T),]
```

Based on random forest variable importance, the variables could be selected for any other predictive modelling techniques or machine learning.
Now, we want to measure the accuracy of the Random Forest model. Some of the other model performance statistics are: - KS - Lift Chart - ROC curve

### Predict Response Variable Value using Random Forest
Generic predict function can be used for predicting response variable using Random Forest object. Here we use a predicting response variable:

```{r}
bank.train$predicted.response <- predict(bank.rf, bank.train)
```

### Confusion Matrix
`confusionMatrix()` function from caret package can be used for creating confusion matrix based on actual response variable and predicted value.

# Load Library or packages

```{r}
library(e1071)
library(caret)
#if(!require(e1071)) install.packages("e1071") 
#if(!require(caret)) install.packages("caret")
```

# Create Confusion Matrix

```{r}
confusionMatrix(bank.train$predicted.response, bank.train$RESP)
```

# Predicting response variable

```{r}
library(ModelMetrics)
truth <- bank.test$RESP
pred <- predict(bank.rf, bank.test)
mse(pred, truth)
```

It has accuracy of 99.81%, which is fantastic. Now we can predict response for the validation sample and calculate model accuracy for the sample.

# Create Confusion Matrix

```{r}
confusionMatrix(pred, truth)
```

Accuracy level has dropped to 91.4% but still significantly higher. We can perform a little more thorough exploration, but we really need to see how a random forest works.

## How random forest works
Each tree is grown as follows:

1. Random Record Selection : Each tree is trained on roughly 2/3rd of the total training data (exactly 63.2%) . Cases are drawn at random with replacement from the original data. This sample will be the training set for growing the tree.

2. Random Variable Selection : Some predictor variables (say, `m`) are selected at random out of all the predictor variables and the best split on these `m` is used to split the node. By default, `m` is square root of the total number of all predictors for classification. For regression, `m` is the total number of all predictors divided by 3. The value of `m` is held constant during the forest growing.

Note : In a standard tree, each split is created after examining every variable and picking the best split from all the variables.

3. For each tree, using the leftover (36.8%) data, calculate the misclassification rate - out of bag (OOB) error rate. Aggregate error from all trees to determine overall OOB error rate for the classification. If we grow 200 trees then on average a record will be OOB for about .37*200=74 trees.

4. Each tree gives a classification on leftover data (OOB), and we say the tree "votes" for that class. The forest chooses the classification having the most votes over all the trees in the forest. For a binary dependent variable, the vote will be YES or NO, count up the YES votes. This is the RF score and the percent YES votes received is the predicted probability. In regression case, it is average of dependent variable.

For example, suppose we fit 500 trees, and a case is out-of-bag in 200 of them:
- 160 trees votes class 1
- 40 trees votes class 2

In this case, RF score is class1. Probability for that case would be 0.8 which is 160/200. Similarly, it would be an average of target variable for regression problem.

Having this knowledge, We can also gain insight by plotting error rates and votes.

```{r}
par(mfrow = c(1, 2))
plot(bank.rf$err.rate, main = "Error Rate", lwd = 2, col = "blue")
plot(bank.rf$votes, main = "Votes", lwd = 2, col = "blue")
```

The `pivot_longer()` "lengthens" data, increasing the number of rows and decreasing the number of columns. We can use it to plot the error rate with the different values of the reponse.

```{r}
library(tidyr)
plotdf <- pivot_longer(data.frame(ntrees=1:nrow(bank.rf$err.rate),bank.rf$err.rate),-ntrees)
ggplot(plotdf,aes(x=ntrees,y=value,col=name)) + 
  geom_line(lwd=1.5) + ggtitle("Bank Randon Forest Error Rate") + theme_bw()
```

X0 and X1 are lines for error in prediction for that specific label, and OOB (your first column) is simply the average of the two. As the number of trees increase, your OOB error gets lower because you get a better prediction from more trees.


## Fit a Random Forest to the fgl data and compare with SVM
The fgl data frame has 214 rows and 10 columns. It was collected by B. German on fragments of glass collected in forensic work.

```{r}
#if(!require(MASS)) install.packages("MASS") 
library(MASS) 
data(fgl) 
set.seed(17) 
fgl.rf <- randomForest(type ~ ., data = fgl, mtry = 2, 
                       importance = TRUE, do.trace = 100)
print(fgl.rf)
```

We can compare random forests with support vector machines by doing ten repetitions of 10-fold cross-validation, using the errorest functions in the ipred package. #errorest performs resampling based estimates of prediction error: misclassification error, root mean squared error or Brier score for survival data.

```{r}
#if(!require(ipred)) install.packages("ipred") 
library(ipred) 
set.seed(131) 
error.RF <- numeric(10) 
for(i in 1:10) 
  error.RF[i] <- errorest(type ~ ., data = fgl, 
                          model = randomForest, mtry = 2)$error 
summary(error.RF)
```

# SVM

```{r}
set.seed(563) 
error.SVM <- numeric(10) 
for (i in 1:10) 
  error.SVM[i] <- errorest(type ~ ., data = fgl, 
                           model = svm, cost = 10, gamma = 1.5)$error 
summary(error.SVM)
```

We see that the random forest compares quite favorably with SVM. We have found that the variable importance measures produced by random forests can sometimes be useful for model reduction

```{r}
par(mfrow = c(2, 2)) 
for (i in 1:4) 
  plot(sort(fgl.rf$importance[,i], dec = TRUE), 
       type = "h", main = paste("Measure", i))
```

Now we can also check the error rate of the FLG random forest for all its variables.

```{r}
plotdf <- pivot_longer(data.frame(ntrees=1:nrow(fgl.rf$err.rate),fgl.rf$err.rate),-ntrees)
ggplot(plotdf,aes(x=ntrees,y=value,col=name)) + 
  geom_line(lwd=1) + ggtitle("FLG Rand Forest Error Rate") + theme_bw()
```

The legend shows that the variables are the colored lines for error in prediction (except the green line) for that specific label, and OOB (the green line) is simply the average of the all the variables. As the number of trees increase, your OOB error gets lower because you get a better prediction from more trees.

We can compare random forests with support vector machines by doing ten repetitions of 10-fold cross-validation, using the `errorest` functions in the `ipred` package. `errorest` performs resampling, based estimates of prediction error: misclassification error, root mean squared error or Brier score for survival data.

```{r}
library(ipred) 

set.seed(131)
error.RF <- numeric(10)
for(i in 1:10) error.RF[i] <- errorest(type ~ ., 
data = fgl, model = randomForest, mtry = 2)$error
summary(error.RF) 

set.seed(563)
error.SVM <- numeric(10)
for (i in 1:10) error.SVM[i] <- errorest(type ~ ., data = fgl, model = svm, cost = 10, gamma = 1.5)$error
summary(error.SVM)
```

We see that the random forest compares quite favorably with SVM. We have found that the variable importance measures produced by random forests can sometimes be useful for model reduction. 

