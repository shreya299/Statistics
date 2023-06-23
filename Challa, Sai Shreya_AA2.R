"
****************************************************************
Name:Sai Shreya Challa
Student Number:A00264982

QMM1002 Module 2 Applied Activity
****************************************************************
"

#############	
#Question #1		
#What is the P-value for t≤-2.77, when the degrees of freedom are the last 2 digits of your student number.
#############
df<-82# 82 are the last 2 digits of my student id
pt(-2.77, df=82)
#p-value is 0.003465829

"
#############	
#Question #2		
7 marks) After completing training at a telemarketing company, it is expected that an employee will make an average of 420 calls per day.
For a certain employee, the number of calls made daily is tracked over 30 days, and the average number of calls made per day is found to be 386,
with a standard deviation of 85 calls.
The manager would like to see if there is evidence that this employee is underperforming. Test the manager’s hypothesis given a significance level of 0.05.
a)	What are the null and alternative hypotheses?
b)	What is the value of the test statistic? The p-value?
c)	Do you reject or fail to reject the null hypothesis? Explain your answer using p-values or critical values and interpret the decision in context.  
#############	"
#Null hypothesis
#H0:mu>=420
#Alternate Hypothesis
#HA: mu<420

mean.calls<-386
sd.calls<-85
n.calls<-30

t.stat<-(mean.calls-420)/(sd.calls/sqrt(n.calls))
t.stat#-2.19089
pt(t.stat, df=n.calls-1)#0.01832234
#p= 0.019<0.05 reject the null hypothesis

#Therefore, we can conclude that there is evidence to suggest that the employee is under-performing, as the sample mean of 386 calls per day is significantly less than the number of calls mean of 420 calls per day.


"
#############	
#Question #3
(8 marks) Read the “Donors.csv” dataset into R. This dataset contains a random sample of 916 donors (from a population of 1.5 million)
who have donated to a Canadian charitable organization. It includes the variables age (in years),
homeowner (H=yes, U=unknown), gender, wealth (1=lowest, 9=highest), children, donated last (0=did not donate, 1=did donate),
and amount donated last (in dollars).Since there are 9 categories of wealth, the mean value is 5. 
An analyst for the organization is 
interested in whether the mean wealth category of the donor population differs from 5.
Test this hypothesis at α=0.05, and interpret your results using the t.test() function.
In Canada, the average age of a person is approximately 39 years old. Using the “Donors.csv” data the Canadian charitable organization
would like to know if the average age of their donors is greater than 39 years.
Test this hypothesis at α=0.01, and interpret your results using the t.test() function.
Check the nearly normal condition for both variables (wealth and age). Is it satisfied?
#############	"
#Set donor variable to read the dataset
donor <- read.csv(file = file.choose("Donors.csv"),header=TRUE,sep=',')

#Null hypothesis
#H0:mu=5
#Alternate hypothesis
#HA:mu!=5

t.test(donor$Wealth,mu=5,alternative = "two.sided")
"
data:  donor$Wealth
t = 3.7438, df = 915, p-value = 0.0001926
alternative hypothesis: true mean is not equal to 5
95 percent confidence interval:
  5.161017 5.515839
sample estimates:
  mean of x 
5.338428 
"
#p-value = 0.0001926<0.05 reject the null hypothesis and there is evidence of a difference between the mean wealth category and 5.

#H0:mu>=39
#HA:mu<39

t.test(donor$Age,mu=39,alternative = "less",conf.level = 1-0.01)

"One Sample t-test

data:  donor$Age
t = 43.887, df = 915, p-value = 1
alternative hypothesis: true mean is less than 39
99 percent confidence interval:
     -Inf 63.62944
sample estimates:
mean of x 
 62.38755  "

#p-value=1>0.01 Do not reject the null hypothesis
#There is no evidence that the average age of the donors is greater than 39 years.

hist(donor$Age,xlab="Age(years)", main="Histogram of Ages",right=FALSE)
#left skewed where sample size>40,unimodal -> nearly normal condition
#Yes, the condition is satisfied
hist(donor$Wealth,xlab="Wealth(1=lowest, 9=highest)", main="Histogram of Wealth",right=FALSE)
#left  skewed where sample size>40,unimodal -> nearly normal condition
#Yes, the condition is satisfied


"
#############	
#Question #4
4.	(5 marks)  Case Study 1 Check Point Test the hypothesis that the mean number of hours you spend studying per day is
greater than 3.13 hours.Use all the recorded values for hours studying/homework in your personalized data set to 
conduct the test.Interpret the decision from your test, does it seem like you study more than McGill University students (that study 3.13 hours per day)?
Upload your personalized data set to the assignment drop box.
#############	"
#Set data variable to read the dataset
data<- read.csv(file = file.choose(),header=TRUE,sep=',')
#Null hypothesis is i study for more than 3.13 hours
#H0:mu>=3.13 study more than 3.13 hours
#Alternate hypothesis is i study for less than 3.13 hours
#HA:mu<3.13 study less than 3.13 hours


t.test(data$Hours.spent.studying,mu=3.13,alternative = "greater")

"	One Sample t-test

data:  data$Hours.spent.studying
t = -14.687, df = 147, p-value < 2.2e-16
alternative hypothesis: true mean is less than 3.13
95 percent confidence interval:
     -Inf 1.971475
sample estimates:
mean of x 
 1.824324 "

#p-value<0.05 reject the null hypothesis
# There is evidence that i do not study for 3.13 or greater than 3.13 hours per day
#it seems like i don't study more than McGill University students (that study 3.13 hours per day)
