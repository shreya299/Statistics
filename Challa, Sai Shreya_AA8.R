"
****************************************************************
Name:Sai Shreya Challa
Student Number:A00264982

QMM1002 Module 8 Applied Activity
****************************************************************
"

"
#############	
#Question #1
1.	(18 marks) Most professional baseball players are born in August. 
It has been suggested that children born in the summer have a better chance 
of making it to the professional leagues than their peers perhaps because 
they can be outdoors when they are young or due to the cut off dates for 
children entering baseball leagues (July 31 in the United States). The data 
in the table report the number of professional ballplayers born in each month
of the year for a random sample of professional baseball players. We would 
like to determine if this observed distribution of players born in each month
is significantly different than what we would expect if players birthdays 
were equally distributed across the year. 
a.	(2 marks) State the null and alterative hypothesis for the chi-square
goodness of fit test.
b.	(2 marks) Create vectors for the observed birthdays in each month and 
expected birthdays in each month. What is the expected number of birthdays 
per month under the null hypothesis? 
c.	(5 marks) Conduct a chi-square goodness of fit test at α = 0.05 and 
decide to reject or fail to reject the null hypothesis (using the p-value
or critical value). Interpret your decision in the context of the problem. 
d.	(1 mark) Calculate and print out the standardized residuals for each
month. 
e.	 (4 marks) Create a bar plot that shows the residual values.
Are there any unusual values? Specifically comment on which unusual values are above the mean and which are below the mean. 
f.	(4 marks) Create a grouped bar plot to show the observed and expected 
distributions for birthdays in each month. Are more players born in August? 
Explain using the plot.
"

#Null hypothesis:The distribution of players' birthdays is equally distributed across the year.
#H0: 
#Alternative hypothesis:The distribution of players' birthdays is not equally distributed across the year.


Birth=data.frame(
  Month = c("January","February","March","April","May","June","July","August","September","October","November","December"),
  Player_Count = c(137,121,116,121,126,114,102,165,134,115,105,122))
print(Birth)
"       Month Player_Count
1    January          137
2   February          121
3      March          116
4      April          121
5        May          126
6       June          114
7       July          102
8     August          165
9  September          134
10   October          115
11  November          105
12  December          122"

# Observed birthdays
observed <- c(137, 121, 116, 121, 126, 114, 102, 165, 134, 115, 105, 122)

# Total number of players
total_players <- sum(observed)

# Expected birthdays per month
expected <- rep(total_players/12, 12)

#chi-square test
(chi_sq<-chisq.test(observed))
"	Chi-squared test for given probabilities

data:  observed
X-squared = 24.825, df = 11, p-value = 0.009668"

#p-value=0.009668 <0.05=alpha 
#reject the null hypothesis. There is a significant evidence that the observed distribution differs from the expected model.


(standardized_residuals <- residuals(chi_sq))
"[1]  1.2464655 -0.1952295 -0.6457592 -0.1952295  0.2553002 -0.8259711 -1.9072423  3.7694317  0.9761476
[10] -0.7358652 -1.6369245 -0.1051236"

# Create bar plot of standardized residuals
barplot(standardized_residuals,main="Comparison of Birth and Month Residuals",names.arg = month.abb, xlab = "Month", ylab = "Standardized Residuals",col=c("blue","yellow", "green","red","gray","pink","brown","violet","chocolate","turquoise","black","skyblue"))

# Comment on unusual values
mean_residual <- mean(standardized_residuals)
above_mean <- sum(standardized_residuals > mean_residual)
below_mean <- sum(standardized_residuals < mean_residual)

(unusual_values <- paste("There are", above_mean, "unusual values above the mean and", below_mean, "unusual values below the mean."))
#"There are 4 unusual values above the mean and 8 unusual values below the mean."
#May, January, September and August are above the mean while others are below the mean.
library(ggplot2)

players <- data.frame(
  month = rep(c("January","February","March","April","May","June","July","August","September","Octuber","November","December"), 2),
  Counts = c(observed, expected),
  Distribution = c(rep("observed", length(observed)), rep("expected", length(expected)))
)

ggplot(players, aes(month, Counts, fill = Distribution)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Months", y = "Count") +
  ggtitle("Birth Distributions") +
  theme(
    plot.margin = margin(1, 1, 1, 1, "cm"),
    axis.line = element_line(color = "green", size = 1)
  ) +
  scale_fill_manual(values = c("observed" = "blue", "expected" = "chocolate"))

#The bar for August month is significantly higher as observed in barplot and are most likely to make it to professional league in Baseball.
