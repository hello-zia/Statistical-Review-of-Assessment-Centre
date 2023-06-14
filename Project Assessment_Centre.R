# Assessment Centre Data Exploration
# Skills used: data cleaning, exploratory factor analysis (EFA), extraction, standarised effect sizes, visualisation, range restriction, multiple regression


# install.packages("readxl")
library(readxl)
ac.data<-read_excel("Assessment_Centre.xlsx", col_names = TRUE, na = "#NULL!")
View(ac.data)

# Returning the values of the class attribute of an R object with class() function
class(ac.data)
ac.data<-as.data.frame(ac.data)
str(ac.data)

# Data Cleaning: converting characters to factors 
ac.data$gender<-factor(ac.data$gender, levels = c("Woman", "Man"))
ac.data$ethnicity<-factor(ac.data$ethnicity, levels = c("White", "Ethnic_Minority"))
str(ac.data)

# Checking for Construct Validity

# Exploratory Factor Analysis (EFA)
# Selecting vectors to be factored: dplyr() function
# install.packages("dplyr")
library(dplyr)
factor.data<-select(ac.data, 5:44)

# Converting  into a standard data frame
factor.data<-as.data.frame(factor.data) 
factor.data

# Checking for missing values: sum() to check NAs 
sum(is.na(factor.data))
# Result: There is no missing values in this selection.

# Checking the Size of data 
length(ac.data)*nrow(factor.data)

# Checking factorability: REdaS and psych packages
# install.packages("REdaS")
library(REdaS)
# install.packages("psych")
library(psych)

# Barlett's test p < .05; Result: admissible
bart_spher(factor.data)
# KMO < .50 (overall MSA = 0.93); Result: admissible
KMO(factor.data)



# Various ways to perform factor analysis

# Minimum Residuals of Ordinary least squares (OLS)
fa.parallel(factor.data, fm="minres")

# Weighted least squares (WLS) solution
fa.parallel(factor.data, fm="wls")

# Generalized weighted least squares (GLS)
fa.parallel(factor.data, fm="gls")

# Principal factor solution
fa.parallel(factor.data, fm="pa")

# Factor analysis by principal axis # Maximum likelihood estimation (MLE)
fa.parallel(factor.data, fa="fa", fm="ml")

# Result: Parallel analysis suggests that the number of factors = 4  and the number of components = 4 




# Extraction - Factor Rotation

# 4-factor extraction with oblique rotation
# install.packages("GPArotation")
library(GPArotation)

efa4<-fa(factor.data, nfactor=4, fm="ml", rotate="oblimin")

# Suppressing loadings of < .30
# Print and summary functions for the psych class
print.psych(efa4, cut= .3, sort=TRUE)




# Checking for Discrimination: adverse impact (background checks for candidates)

# Exercise 1
ac.data$exercise1.av<-rowMeans(ac.data[5:14], na.rm =TRUE) 
ac.data$exercise1.av
summary (ac.data$exercise1.av)       

# Exercise 2
ac.data$exercise2.av<-rowMeans(ac.data[15:24], na.rm =TRUE)
ac.data$exercise2.av
summary (ac.data$exercise2.av)            

# Exercise 3
ac.data$exercise3.av<-rowMeans(ac.data[25:34],na.rm=TRUE)
ac.data$exercise3.av
summary (ac.data$exercise3.av)

# Exercise 4
ac.data$exercise4.av<-rowMeans(ac.data[35:44],na.rm=TRUE)
ac.data$exercise4.av
summary(ac.data$exercise4.av)

# Overall Assessment Rating: average of the exercise scores
ac.data$oar<-(ac.data$exercise1.av + ac.data$exercise2.av +
                ac.data$exercise3.av + ac.data$exercise4.av)/4

View(ac.data)
summary(ac.data$oar)
mean(ac.data$oar, na.rm = TRUE)
sd(ac.data$oar, na.rm = TRUE)

# Adverse impact relating to "gender"

# Mean value: Welch Two sample t-test
t.test(oar ~ gender, data = ac.data)

# Creating subsets
man.only<-subset(ac.data, gender == "Man")
woman.only<-subset(ac.data, gender == "Woman")

man.only
woman.only

mean(man.only$oar)
mean(woman.only$oar)

ac.data$gender<-as.factor(ac.data$gender)

# Computing standardised effect sizes
# install.packages("effsize")
library(effsize)

# Cohen's d 
cohen.d(ac.data$oar, ac.data$gender, na.rm = TRUE)

# Hedge's g 
cohen.d(ac.data$oar, ac.data$gender, hedges.correction = TRUE, na.rm = TRUE)

# Result
# d estimate = .17 (CI [ -0.10,0.44]), g estimate = 0.17(CI[-0.10,0.44]) 
# The effect size is small, non-significance observed in the t-test
# No evidence for adverse impact relating to gender



# Adverse impact relating to "ethicity"

# Mean value: Welch Two sample t-test
t.test(oar ~ ethnicity, data = ac.data)

# Creating subsets
White.only<-subset(ac.data, ethnicity == "White")
Ethnic_Minority.only<-subset(ac.data, ethnicity == "Ethinc_Minority")

White.only
Ethnic_Minority.only

mean(White.only$oar)
mean(Ethnic_Minority.only$oar)

ac.data$ethnicity<-as.factor(ac.data$ethnicity)

# Computing standardised effect sizes (Cohen's d and Hedge's d)
# install.packages("effsize")
library(effsize)

# Cohen's d 
cohen.d(ac.data$oar, ac.data$ethnicity, na.rm = TRUE)

# Hedge's g 
cohen.d(ac.data$oar, ac.data$ethnicity, hedges.correction = TRUE, na.rm = TRUE)

# Result
# d estimate = .25 (CI [ -09.,0.59]), g estimate = 0.25(CI[-0.09,0.59]) 
# The effect size is small, non-significance observed in the t-test
# No evidence for adverse impact relating to ethnicity




# Criterion-related validity

# Correlating the Overall Assessment Rating with the Overall Performance Rating
# Estimating the correlation with confidence intervals and significance
# Eliminating missing values from a vector, matrix, or data frame: use = "complete"
cor.test(ac.data$oar, ac.data$ojp, use = "complete")

# Saving correlation as an object to correct
# Discarding the entire row that NA is present: use = "complete.obs"
correlation.test<-cor.test(ac.data$oar, ac.data$ojp, use = "complete.obs")

correlation.test
# = .42, p-value < .001 (significantly related)
# Result: evidence that the Overall Assessment Rating measures Overall Performance Rating effectively




# Visualisation: Scatterplot
library(ggplot2)
scatter<-ggplot(ac.data, aes(oar, ojp), position = "jitter")
scatter + geom_point() + geom_smooth(method = "lm", colour = "Magenta") +
  labs(x = "Overall Assessment Rating", y = "Job Performance")

# Correcting the correlation above for attenuation relating to unreliability
# in the criterion or range restriction

# Unrestricted correction 
validity.coef<-cor(ac.data$oar, ac.data$ojp, use = "complete.obs")
validity.coef

unrestricted_correlation<-validity.coef
# Using Criterion of .52, from Viswesvaran et. al. (1996) 
criterion.reliability<-.52 

# Applying correction formula to get corrected correlation
# sqrt means square root
unrestricted_correlation/sqrt(criterion.reliability) # = .58

# Range restriction
# install.packages("psych")
library(psych)

# Estimating the unrestricted SD for the predictor
unrestricted.sd<-sd(ac.data$oar)

# Estimating the restricted SD for the predictor and criterion variables 
# Selecting both predictor and criterion variables
restricted.df<-subset(ac.data, select=c("oar","ojp"))
# Creating an NA omit data frame, which will restrict according to criterion
restricted.df<-na.omit(restricted.df)
restricted.sd<-sd(restricted.df$oar) # = .67

unrestricted.sd # = .74
restricted.sd # = .68

rangeCorrection(unrestricted_correlation, unrestricted.sd, restricted.sd) # = .45




# Comparing strengths among Predictor(Exercises) for Performance: using multiple regression to compare beta coefficient

library(QuantPsyc)

multiple_regression<-lm(ojp ~ exercise1.av + exercise2.av + exercise3.av + exercise4.av, data = ac.data)
summary(multiple_regression)
lm.beta(multiple_regression)

# Result: exercise 1 (β = .18), exercise 2 (β = .14), exercise 3 (β = .20), exercise 4 (β = .05).

