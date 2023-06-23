"
****************************************************************
Name:Sai Shreya Challa
Student Number:A00264982

QMM1002 Module 1 Applied Activity
****************************************************************
"

#############	
#Question #1		
#Determine the following:  Read in the “Health” dataset into R.
#This dataset gives a sample from 129 individuals of body temperature (in degrees Fahrenheit) and heart rate in beats per minute.
#Note that 1=male and 2=female.
#############"

#Set health variable to read the dataset
health <- read.csv(file = file.choose("Health.csv"),header=TRUE,sep=',')

#a.	(2 marks) Determine the mean and standard deviation of the heart rate.
mb<-mean(health$heart.rate) #mean of the heart rate
#73.7907

sb<-sd(health$heart.rate) #standard deviation of the heart rate
#7.081749

#b.	(1 mark) Calculate the standard error of the mean.
n<-length(health$heart.rate)
SE<-sb/sqrt(n)#standard error of the heart rate
#0.6235132

#c.	(1 mark) How many degrees of freedom does the t-statistic have?
df<-n-1 #128 is degrees of freedom

#d.	(2 marks) Use a histogram to check the nearly normal condition for heart rate. Is the condition satisfied?
hist(health$heart.rate,xlab="Heart rate(bpm)", main="Histogram of Heart beat per minute")
#symmetric,unimodal -> nearly normal condition
#Yes, the condition is satisfied

#e.	(3 marks) Determine the 95% confidence interval for the population heart rate mean. Interpret the interval.
t.crit<-qt(0.975, n-1)
SE<-sb/sqrt(n)
mb-t.crit*SE#72.55697
mb+t.crit*SE#75.02443

#95% confident the mean population heart rate is between 72.55 bpm and 75.02 bpm.

t.test(health$heart.rate, conf.level = 0.95)

#f.	(3 marks) Find a (last two digits of your student number)% confidence interval for the population body temperature mean. 
#Comment on if this interval is wider or narrower than the 95% confidence interval.

#82 ARE THE LAST TWO DIGITS so 82% confidence interval
n<-length(health$temperature)
mp<-mean(health$temperature)
SE<-sd(health$temperature)/sqrt(n)
t.crit<-qt(0.975, df=n-1)    #95% confidence interval
mp-t.crit*SE#98.1397
mp+t.crit*SE#98.38898

#95% confident the mean body temperature (in degrees Fahrenheit) is between 98.13 and 98.4

t.crit<-qt(0.91, n-1) #82% confidence interval
mp-t.crit*SE#98.17942
mp+t.crit*SE#98.34926

#82% confident the mean body temperature (in degrees Fahrenheit) is between 98.18 and 98.35

t.test(health$temperature, conf.level = 0.95)
t.test(health$temperature, conf.level = 0.82)

#82% confidence interval is narrower than 95 % confidence interval

#g.	(3 marks) Based on these statistics, how many people should be sampled to estimate the populate mean heart rate
#within 2 bpm with 99% confidence?
ME<-2#2bpm margin of error
s<-7.081749#standard deviation

n<-(qnorm(0.995)*s/ME)^2 # 99% confidence interval
n #first iteration, use result to find t value and recalculate n 
n<-(qt(0.995, n-1)*s/ME)^2
n   #87.18365
ceiling(n)#88

#88 people should be sampled to estimate the populate mean heart rate within 2 bpm with 99% confidence

#############	
#Question #2

##Create a 95% confidence interval for the mean number of hours spent studying per day. 
#Use all the recorded values for hours studying/homework in your personalized data set to create the interval.
#Interpret the interval in the context of the problem. From the interval does it seem like you study every day?
#Does it seem like you study more than 3.13 hours per day? Upload your personalized data set to the assignment dropbox.
#############
#Set data variable to read the dataset
data<- read.csv(file = file.choose(),header=TRUE,sep=',')

n1<-length(data$Hours.spent.studying)
mp1<-mean(data$Hours.spent.studying)
SE1<-sd(data$Hours.spent.studying)/sqrt(n1)
t.crit1<-qt(0.975, df=n1-1)    

mp1-t.crit1*SE1#1.636413
mp1+t.crit1*SE1#2.001884

t.test(data$Hours.spent.studying, conf.level = 0.95)

#95%  confident the mean number of hours spent studying per day is between 1.64 and 2

#The interval for the number of hours spent on studying is between 1.64 and 2.00

#It seems like i try to study everyday at least for about 1 to 2 hrs
#No,it doesn't seem like i study for more than 3.13 hours per day
