"
****************************************************************
Name:Sai Shreya Challa
Student Number:A00264982

QMM1002 Module 6 Applied Activity
****************************************************************
"

"
#############	
#Question #1
15 marks) A pharmaceutical company has developed a new drug that they believe increases focus and 
attention. To test this hypothesis, the company obtains 27 volunteers and randomly assigns 9 to each 
of three groups. The first group receives a placebo. The second group receives 50mg of the drug. 
The third group receives 100mg of the drug. After taking the treatment, participants complete a 
computerized focus task to measure their level of focus and attention. The percentage accuracy for 
each participant is shown in the table. You must recreate this table in R (HINT: use data.frame() 
or cbind()). 
#############
"
accuracy <- data.frame(
  Group = rep(c("Placebo", "50mg", "100mg"), each = 9),
  Percentage = c(81,80,72,82,83,89,76,88,83,92,86,87,76,80,87,92,83,84,86,93,97,81,94,89,98,90,91)
)
# Printing the data frame
accuracy

#a.(2 marks) State the null and alternative hypotheses.

#Null hypothesis: no difference in mean accuracy between the groups
#H0: mu_p = mu_d50mg = mu_d100mg
#Alternative hypothesis:there is difference in mean accuracy between the groups
#HA: at least one mean is different

#b.	(2 marks) Perform a one-way ANOVA in R and print out the completed summary table.
model <- aov(Percentage ~ Group, data = accuracy)
summary(model)
"            Df Sum Sq Mean Sq F value  Pr(>F)   
Group        2  408.1  204.04   7.289 0.00336 **
Residuals   24  671.8   27.99                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"

#c.	 (1 mark) Determine the critical F value.

qf(0.05, df1=2, df2=24, lower.tail = FALSE)#make a decision using critical f value
#3.402826

#d.	(2 marks) At 𝛼 = 0.05 state your decision regarding the null hypothesis (using the p-value or critical value and interpret your result in the context of the problem.

#F = 7.289 (from ANOVA table) > 3.402826 = F.critcal
#Reject the null hypothesis, the percentage accuracy between groups do not have the same effect

#e.	 (3 marks) Check the independence, equal variance, and normal population assumptions. Explain if the assumptions are met or not (show and describe all plots that you create). 
#to check conditions
boxplot(Percentage ~ Group, data = accuracy,main="Comparison of Groups", 
        ylab="Percentage", xlab="Group", col=c("blue","yellow", "green"))
#no significant differences (some much lower values for control).relatively similar spreads across the three groups, suggesting equal variance.
hist(model$residuals, main="Histogram of Residuals", 
     xlab="Residuals") #data points roughly following a straight line, suggesting normality.
plot(model) #other residual plots
#appear to be independent as there are no visible patterns 

#the assumptions of independence, equal variance, and normal population are reasonably met

#f.	(3 marks) Create a bar plot of the accuracy for each type of drug. Include axis labels, a title, 95% error bars and colors from a chosen palette. From this plot, do any of the drugs seem to help participants become more accurate than others? 
# Bar plot
library(ggplot2)

ggplot(accuracy, aes(x = Group, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity") +
  labs(x = "Drug Group", y = "Accuracy", title = "Accuracy of Each Drug Group") +
  geom_errorbar(aes(ymin = Percentage - 1.96 * sd(Percentage) / sqrt(length(Percentage)),
                    ymax = Percentage + 1.96 * sd(Percentage) / sqrt(length(Percentage))),
                width = 0.5, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1")

#From the bar plot, it appears that both the  100mg followed by 50mg groups show higher accuracy compared to the placebo group.

#g.	(2 marks) Perform Tukey’s HSD test to confirm your results from the bar plot. Interpret your result to explain which mean(s) are different. 
TukeyHSD(model, conf.level=0.95)
"  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = Percentage ~ Group, data = accuracy)

$Group
                   diff        lwr        upr     p adj
50mg-100mg    -5.777778 -12.006076  0.4505207 0.0725971
Placebo-100mg -9.444444 -15.672743 -3.2161460 0.0025028
Placebo-50mg  -3.666667  -9.894965  2.5616318 0.3225832"

#From the Tukey's HSD test, none of the pairwise differences between the means are statistically significant at 𝛼 = 0.05


"
#############	
#Question #2
The data set Combined.csv contains the hours studied by past analytics students. 
Test the hypothesis that the mean number of hours studied is the same for all three programs 
(Business = BAPG, Crime = CAGC, and Health = HAGC). Prepare your data for analysis by completing 
the following steps: Divide the data into subsets by program, making separate data sets 
for students in the BAPG, CAGC, and HAGC programs. Take a random sample of 50 days from 
each subset using the sample_n() function from the dplyr package. Perform an ANOVA to test 
your hypothesis (HINT: think about what format the data set needs to be in for ANOVA). 
State your final conclusion regarding the null hypothesis and interpret the results.
#############	
"
# Loading the required packages
install.packages("dplyr")
library(dplyr)

# Reading the data from Combined.csv
dataa <- read.csv("Combined.csv")

#Null hypothesis:the mean number of hours studied is the same for all three programs
#H0:mub=muc=muh
#Alternative hypothesis:the mean number of hours is different
##HA: at least one mean is different

# Dividing the data into subsets by program
BAPG <- filter(dataa, dataa$Program == "BAPG")
CAGC <- filter(dataa, dataa$Program == "CAGC")
HAGC <- filter(dataa, dataa$Program == "HAGC")

# Taking a random sample of 50 days from each subset
sample_BAPG <- sample_n(BAPG, 50)
sample_CAGC <- sample_n(CAGC, 50)
sample_HAGC <- sample_n(HAGC, 50)

# Combining the samples into a single data frame
combined_samples <- rbind(sample_BAPG, sample_CAGC, sample_HAGC)

# Performing ANOVA
result <- aov(Study ~ Program, data = combined_samples)
summary(result)
"             Df Sum Sq Mean Sq F value Pr(>F)
Program       2    7.9   3.970    0.79  0.456
Residuals   147  738.4   5.023               "

#to check conditions
boxplot(Study ~ Program, data = combined_samples) #no significant differences (some much lower values for control)
hist(result$residuals) #nearly normal 
plot(result) #other residual plots

#if alpha=0.05
#p-value0.456< 0.05=alpha,reject the null hypothesis and there are significant differences in the mean number of hours studied among the programs.