"
****************************************************************
Name:Sai Shreya Challa
Student Number:A00264982

QMM1002 Module 3 Applied Activity
****************************************************************
"
"
#############	
#Question #1		
1.	(17 marks) The data set “mpg.csv” provided in Moodle gives the number of miles per gallon (mpg) for a random sample
of U.S. cars vs. Japanese cars.
We would like to test the hypothesis that Japanese cars get better mileage than U.S. cars at α = 0.01.
#############"
mpgdata<-read.csv(file.choose("mpg.csv"))

us_cars<-subset(mpgdata,Country=="US",select = c(Mileage,Country))
mean_us<-mean(us_cars$Mileage)#15.8

japancars<-subset(mpgdata,Country=="JAPAN",select = c(Mileage,Country))
mean_jp<-mean(japancars$Mileage)#25.8

#Japanese cars seemS to get better MEAN mileage

#a.	(2 marks) State the null and alternative hypothesis
#Null Hypothesis #no difference in the mean mileage between Japanese and US cars.
#HO: Mu_japan-Mu_us=0


#Alternative hypothesis #true difference in means between group JAPAN and group US is greater than 0
#HA: Mu_japan-Mu_us>0

#b.	(3 marks) Check all assumptions and conditions for performing a two-sample t-test.
#Are they satisfied? Include histograms for the mileage in each country to check the nearly normal condition. 

#Independence: The cars in each group are independent of each other.

#Equal variances: The variances of mileage in each group are equal.
#Randomization: The data set is described as a random sample, so this assumption is met.
#Independent groups:two groupsof US and Japan we are comparing are independent of each other.
#Normality: The distribution of mileage in each group is checked using histograms .
#Create histograms
hist(us_cars$Mileage, col="purple", xlab="Mileage", main="Mileage in US")
hist(japancars$Mileage, col="pink", xlab="Mileage", main="Mileage in Japan")
#distribution of mileage in each group is roughly symmetric and unimodal, which suggests that the normality assumption is satisfied.
#
#Boxplot for groups
boxplot(mpgdata$Mileage~ mpgdata$Country,col=c("purple", "pink"), xlab="Country",main="Mileages for different countries") 


#c.	(2 marks) Perform the hypothesis test using the t.test() function 
##Perform the pooled t-test with the assumption that variances are equal
t.test(mpgdata$Mileage~ mpgdata$Country,
       var.equal=TRUE,#equal variance assumption = pooled test
       paired=FALSE,
       conf.level=0.99,
       alternative="greater")

"data:  mpgdata$Mileage by mpgdata$Country
t = 8.4785, df = 18, p-value = 5.306e-08
alternative hypothesis: true difference in means between group JAPAN and group US is greater than 0
99 percent confidence interval:
 6.989586      Inf
sample estimates:
mean in group JAPAN    mean in group US 
               25.8                15.8 "

#d.	(2 marks) Determine the critical value 𝑡∗.
qt(0.01/2, df=10+10-2)#-2.87844
qt(0.01/2, df=10+10-2, lower.tail=FALSE)#2.87844
#t = 8.4785 > 2.87844 = critical t value
#Reject the null hypothesis


#e.	(2 marks) Make a decision (using the p-value OR critical value) and interpret your results
#The p-value 5.306e-08< 0.01
#the critical value is  8.4785 > 2.87844, # reject the null hypothesis.
#This indicates that there is enough evidence to suggest that Japanese cars get better mileage than U.S. cars.


#f.	 (3 marks) Determine the 95% confidence interval for the mean difference in mileage for Japanese and US cars.
#Interpret the interval 
t.test(mpgdata$Mileage ~ mpgdata$Country,var.equal=TRUE, 
       paired=FALSE, conf.level = 0.95)
"Welch Two Sample t-test

data:  mpgdata$Mileage by mpgdata$Country
t = 8.4785, df = 18, p-value = 1.061e-07
alternative hypothesis: true difference in means between group JAPAN and group US is not equal to 0
95 percent confidence interval:
  7.52206 12.47794
sample estimates:
mean in group JAPAN    mean in group US 
               25.8                15.8  "
#95% confident that the true mean difference between group JAPAN and group US is between 7.52206  and 12.47794. 
#This suggests that mileage from a japan car is better and more than a mileage of a car from US 
#The interval does not contain 0 so this support the rejection of the
#null hypothesis from the hypothesis test.

#g.	(3 marks) Create a bar plot of the mean mileage for US and Japanese cars with error bars showing the 95% confidence intervals.
#Add proper labels.

sd_us<-sd(us_cars$Mileage)
sd_japan<-sd(japancars$Mileage)
n_us<-length(us_cars$Mileage)
n_japan<-length(japancars$Mileage)
#ci_us<-qt(0.975, df =10- 1) * sd_us / sqrt(sum(df$country == "US"))

ci_us <- qt(0.975, df = n_us - 1) * sd_us / sqrt(n_us)
ci_japan <- qt(0.975, df =n_japan - 1) * sd_japan / sqrt(n_japan)

install.packages("ggplot2")
library(ggplot2)

# creating a data frame with mean mileage and 95% confidence intervals for US and Japanese cars
mileage <- c(mean_us, mean_jp) # mean mileage
lower <- c(mean_us - ci_us,mean_jp - ci_japan) # lower bound of confidence interval
upper <- c(mean_us + ci_us,mean_jp + ci_japan) # upper bound of confidence interval
df <- data.frame(mileage, lower, upper, group = c("US", "JAPAN"))

# create a barplot with error bars
ggplot(df, aes(x = group, y = mileage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  labs(title = "Mean Mileage for US and Japanese Cars", x = "Country", y = "Mileage") +
  theme_minimal()



"
#############	
#Question #2	
 Can you use the two-sample t-test for the mean to determine if your average hours 
 studied are greater in the Spring 2022 semester than the Winter 2022 semester? 
 Explain your reasoning. If you believe the test is valid, conduct it using the t.test() 
 function at alpha = 0.05. Upload your personalized data set to the assignment drop box.
#############	"
data<-read.csv(file.choose())
Spring<-subset(data,Term=="S23", select=c(Hours.spent.studying,Term))
springmean<-mean(Spring$Hours.spent.studying)#1.93
Winter<-subset(data,Term=="W22", select=c(Hours.spent.studying,Term))
wintermean<-mean(Winter$Hours.spent.studying)#1.80

#Seems like studied more in Spring than in Winter

#To perform two sample t-test it should satisfy the conditions of 
#Independence: The hours of study in each group are independent of each other.
#Equal variances: The variances of mileage in each group should be equal.
#Randomization: The data set is described as a personal data set and not random sample, so this assumption is not met.
#Independent groups:two groups we are comparing are not independent of each other.
#Normality: The distribution of mileage in each group is checked using histograms .
#Create histograms
hist(Spring$Hours.spent.studying, col="blue", xlab="Hours", main="Hours of study in Spring")
hist(Winter$Hours.spent.studying, col="grey", xlab="Hours", main="Hours of study in Winter")
#the histograms are not symmetric and does not appear normal so the condition is not satisfied
#Boxplot
boxplot(data$Hours.spent.studying~data$Term,col=c("blue", "grey"), xlab="Hours",main="Hours of study in two terms") 
#the conditions for t-test to be valid doesn't seem to be satisfied so we cannot perform t-test.


