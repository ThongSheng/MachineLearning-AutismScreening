#################### 
## Data cleansing ##
####################
# Autism data for adults
autism.adult <- read.csv("autism_screening.csv", header = T, na.strings = "?", 
                   colClasses = c(rep("factor",10),"numeric","factor","character","factor","factor",
                                  "character","factor","numeric","factor","factor","factor"))
autism.adult <- na.omit(autism.adult[-20])
colnames(autism.adult)[c(14,15,16)] <- c("jaundice","PDD","country_of_res")
summary(autism.adult)

# Autism data for children 2017
autism.child1 <- read.csv("Child-Data2017.csv", header = T, na.strings = "?",
                   colClasses = c(rep("factor",11),"numeric","factor","character","factor","factor",
                                  "character","factor","numeric","factor","factor","factor"))
autism.child1 <- na.omit(autism.child1[-c(1,21)])
colnames(autism.child1)[c(14,15,16)] <- c("jaundice","PDD","country_of_res")
summary(autism.child1)

# Autism data for children 2018
autism.child2 <- read.csv("Child-Data2018.csv", header = T,
                          colClasses = c(rep("factor",11),"numeric","factor","character","factor","factor",
                                         "character","factor","character","numeric","factor","character",
                                         "factor","factor"))
autism.child2 <- na.omit(autism.child2[-c(1,19,22,23)])
colnames(autism.child2) <- c("A1_Score","A2_Score","A3_Score","A4_Score","A5_Score","A6_Score","A7_Score",
                             "A8_Score","A9_Score","A10_Score","age","gender","ethnicity","jaundice","PDD",
                             "country_of_res","used_app_before","result","age_desc","Class.ASD")
summary(autism.child2)

# Merging all dataset
autism <- rbind(autism.adult,autism.child1,autism.child2)
summary(autism)
attach(autism)


########################
## Hypothesis Testing ##
########################
# Test which factors are significant in predicting autism
logit.fit1 <- glm(Class.ASD ~ ., data=autism, family=binomial)
summary(logit.fit1)
# Returns warning messages with inaccurate results because too many variables

# Testing whether specific variables (e.g.: age, gender, jaundice, PDD) are significant in predicting autism
logit.fit2 <- glm(Class.ASD ~ age + gender + jaundice + PDD, data=autism, family=binomial)
summary(logit.fit2)
# Results show only gender is insignificant

# Testing whether male has a higher chance of having autism than female using this dataset
nrow(autism[autism$gender=="m" & autism$Class.ASD=="YES",]) # number of male with autism
nrow(autism[autism$gender=="f" & autism$Class.ASD=="YES",]) # number of female with autism
prop.test(x = c(359, 204), n = c(858, 508), alternative = "greater", correct = F)
# Fail to prove that male has a higher chance of having autism than female

# Pie chart to show the distribution of autism among ethnicity
new.ASD<-Class.ASD[Class.ASD == "YES"]
new.ethnicity<- ethnicity[Class.ASD == "YES"]
neww.ASD<-rep(1, length(new.ASD))

autism.by.ethnicity = aggregate.data.frame(neww.ASD,by = list(new.ethnicity),sum)
names(autism.by.ethnicity) = c("ethnicity","autism")
pie(autism.by.ethnicity$autism, autism.by.ethnicity$ethnicity)

# Pie chart to show the distribution of autism among country of residence
new.ASD<-Class.ASD[Class.ASD == "YES"]
new.country<- country_of_res[Class.ASD == "YES"]
neww.ASD<-rep(1, length(new.ASD))

autism.by.ethnicity = aggregate.data.frame(neww.ASD,by = list(new.country),sum)
names(autism.by.ethnicity) = c("country","autism")
pie(autism.by.ethnicity$autism, autism.by.ethnicity$country)
