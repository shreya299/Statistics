"
****************************************************************
Name:Sai Shreya Challa
Student Number:A00264982

QMM1002 Module 4 Applied Activity
****************************************************************
"
"
#############	
#Question #1
(16 marks) The managers of many stores claim that despite relatively 
constant sales, profits this year are greater than last year.
They have achieved this by improving their supply chain and distribution operations.
Read in the Store.Profits dataset that gives the profits over two years for 15 randomly selected stores.
#############"
storeprofits<-read.csv(file.choose("Store.Profits.csv"))

#find difference between this year and last year
(storeprofits$diffprofits<-storeprofits$Profits.Year.2-storeprofits$Profits.Year.1)

"a.	Do the data support the claim that profits are higher in year two compared to year one?
Choose the appropriate type of hypothesis test and test this claim as follows:"

#i.	(3 marks) Explain why you chose your test (two-sample (Welch’s, pooled, or paired test) by referring to the assumptions/conditions required for the test you chose 
#To test whether there is a significant difference in profits between the two years for the 15 randomly selected stores, I would choose a paired t-test.
"The paired t-test is appropriate because we are comparing profits from the same stores over two different years, making it a paired design. The assumptions/conditions required for a paired t-test are:
Independence: The paired observations (year one and year two profits for each store) are assumed to be independent of each other.
Normality: The population of the paired differences (year two profits - year one profits) should be approximately normally distributed.
Homogeneity of Variances: The variances of the paired differences should be approximately equal.
"
#check nearly normal condition
hist(storeprofits$diffprofits, xlab="Difference in Profits($)", 
     main="Differences in Profits between two years")
boxplot(storeprofits$diffprofits, ylab="Difference in Profits($)", 
        main="Differences in Profits between two years")

#ii.	(2 marks) State the null and alternative hypotheses 
#Null hypothesis:Null hypothesis: The mean difference in profits between year two and year one is zero.
#H0: mud = 0
#Alternative hypothesis : The mean difference in profits between year two and year one is greater than zero.
#Ha: mud > 0

#iii.	(2 marks) Test the claim at 𝛼 = 0.05 using t.test() 
t.test(storeprofits$Profits.Year.2,storeprofits$Profits.Year.1, paired =TRUE,alternative = "greater")
"	Paired t-test

data:  storeprofits$Profits.Year.2 and storeprofits$Profits.Year.1
t = 1.8092, df = 14, p-value = 0.04597
alternative hypothesis: true mean difference is greater than 0
95 percent confidence interval:
 23.18359      Inf
sample estimates:
mean difference 
       875.1647 "
#iv.	(2 marks) Make a decision to reject or fail to reject (Report your p-value or critical value depending on the method you choose) 
#p-value = 0.04597<𝛼 = 0.05
#Since the p-value is less than the significance level of 0.05, we reject the null hypothesis.
#There is enough evidence to support the claim that the profits in year two are higher than the profits in year one.


#v.	(1 mark) Interpret your decision in the context of the problem
#In the context of the problem, we can conclude that the improvement in supply chain and
#distribution operations has led to an increase in profits in year two compared to
#year one for the sampled stores.

"b.	(3 marks) Find a 95% confidence interval for the difference in mean profits and interpret it in the contact of the problem. "
#Create a confidence interval
t.test(storeprofits$Profits.Year.2,storeprofits$Profits.Year.1, paired =TRUE,conf.level = 0.95)
"	Paired t-test

data:  storeprofits$Profits.Year.2 and storeprofits$Profits.Year.1
t = 1.8092, df = 14, p-value = 0.09193
alternative hypothesis: true mean difference is not equal to 0
95 percent confidence interval:
 -162.3117 1912.6411
sample estimates:
mean difference 
       875.1647 "

#We are 95% confident that the difference in mean profits falls between ($)[-162.3117 1912.6411] interval

"c.	(3 marks) Create a paired-differences plot that shows the year one profit, year two profit, and difference between these values for each store. Give your plot an appropriate title and axis labels. (HINT: you will have to reformat the data into long format!)"
library(ggplot2)
# Convert the data to long format
store.profits.long <- melt(storeprofits, id.vars = c("Store"), 
                           measure.vars = c("Profits.Year.1", "Profits.Year.2"),
                           variable.name = "Year", value.name = "Profits")
library(ggplot2)
store.profits.long$Diff <- store.profits.long$Profits[store.profits.long$Year == "Profits.Year.2"] -
  store.profits.long$Profits[store.profits.long$Year == "Profits.Year.1"]

ggplot(store.profits.long, aes(x = Store, y = Profits, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_segment(aes(x = Store, xend = Store, y = 0, yend = Diff), color = "black", size = 1) +
  geom_point(aes(x = Store, y = Diff), color = "yellow", size = 3) +
  ggtitle("Year One vs. Year Two Profits for Stores")+
  labs( x = "Store", y = "Profits", fill = "Year")

"
#############	
#Question #2
(4 marks)  Case Study 1 Check Point  Do you think that you should use the two-sample t-test (Welch’s),
pooled t-test, or paired t-test to determine
if your average hours studied are greater in the Winter 2022 semester than the Fall 2021 semester?
Explain your reasoning in your own words (explain any conditions that you check and if they are met).
Upload your personalized data set to the assignment drop box.
#############"
"
The appropriate t-test to determine if the average hours studied are greater in the W22 term 
than the S23 term would be the two-sample t-test (Welch's t-test).

Reasoning:
1.Independent Samples: The two datasets, one for the W22 term and one for the S23 term.
The hours studied in each term are independent of each other as they are measured on different dates.
2.Unequal Variances: They do not have equal variances between the two terms, as the hours studied in
each term are unequal.Therefore, assuming equal variances, as in a pooled t-test, may not be appropriate.
3.Different Sample Sizes: The sample sizes for the W22 term and S23 term are different, as not all dates have recorded data for both terms.
This further supports the use of Welch's t-test, which accounts for unequal sample sizes.
4.Comparing Means: The objective is to compare the average hours studied between the W22 term and the S23 term.
The paired t-test would be used if we were comparing the same individuals' study hours before and after a particular event, but in this case, we are comparing two independent groups.

Conditions Met:
It is assumed that the data meets the conditions required for a t-test, such as the data being approximately normally distributed and the samples being random and representative of the population.
In conclusion, based on the given data, the appropriate t-test to determine if the average hours studied are greater in the W22 term than the S23 term is the two-sample t-test (Welch's t-test), which accounts for unequal variances and different sample sizes between the two terms.
                                                                                                                                               
                                                                                                                                               