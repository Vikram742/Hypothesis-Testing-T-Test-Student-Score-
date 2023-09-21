#set the working directory and read the data
student<- read.csv("student-mat.csv",stringsAsFactors = TRUE)
dim(student)
#395 rows and 33 columns
summary(student)
#data cleaning
#missing values
sum(is.na(student))
# No missing values found
#duplicates
sum(duplicated(student))
# No duplicate records found
#check for data types of all columns
str(student)
#many columns

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)

student %>% 
  group_by(sex) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x = sex , y = n))+
  geom_bar(stat = "identity")+
  geom_label(aes(label = n))+
  labs(title = 'Count of Female & Male Students', x = 'Gender' , y = 'Count of Students')

#Null Hypothesis (ho) : final grades of boys and girls is the same
#Altenate Hypothesis (ha) : final grades of boys and girls is not the same
# Let's just keep these two columns and remove the rest

student1<-student[,c(2,33)]

t.test(G3~sex, data = student)
#t=-2.0651 indicates the distance from 0
#df = 390.57 is related to the sample size, how many free data points are available for making comparisons
#p value = 0.03958 is the probability value and indicates that we can reject the null hypothesis as it is less
# than that of alpha (0.05). Hence it is statisticall y significant.
#95% confidence interval suggests that the true difference in means will lie between -1.85 and -0.04 (95% of time)
# We can see the difference in means between the two groups (10.91-9.96) = 0.95

hist(student1$G3)
G3_density<-density(student1$G3)
plot(G3_density)

#Both the histogram and the density plot indicate that there are students who got 0. Could this be due to 
# non attendance of exams.

student1 %>% 
  filter(G3==0) %>% 
  count()
#38 students in total out of 395 have got a score of 0. That is 9.62% students.
# Let us check the mean for both groups by removing students who got zeros.

student2<- student1 %>% 
  select(sex, G3) %>% 
  filter(G3 != 0)
# we have created a new data frame called student 2 which includes a total of 357 students with no zero marks

t.test(G3~sex, student2)
# mean of females is 11.20 and 11.86 of males. The difference in mean of the two groups is 0.66 as compared to 
# the earlier mean difference of 0.95. 
# P value is shown as 0.05335. For us to reject the null hypothesis the p value should be less than 0.05. 
# Therefore it is difficult to say if it is statistically significant.

library(knitr)
library(rmarkdown)
