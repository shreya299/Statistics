"
****************************************************************
Name:Sai Shreya Challa
Student Number:A00264982

QMM1002 Module 5 Applied Activity
****************************************************************
"
"
#############	
#Question #1
(8 marks) A sample of companies was selected randomly from each of three regions in Canada
(West, East, and Prairies), and annual salaries for marketing managers were collected. 
#############
"
"a.	(2 marks) A test is conducted to determine if the salaries of managers in the West
is different than $87 500. Identify the appropriate test and state the null and alternative hypotheses. 
"
"To determine if the salaries of managers in the West
is different than $87,500 a one sample t-test should be used.

Null hypothesis:the mean salaries of managers in the West
is equal than $87,500
H0:mu=$87500

Alternate hypothesis:the mean salaries of managers in the West
is different than $87,500
Ha:mu!=$87500
"


"b.	(2 marks) A test is conducted to determine if the salaries of managers in the West 
is different than in the Prairies. Identify the appropriate test and 
state the null and alternative hypotheses. 
"
"To determine if the salaries of managers in the West
is different than $87,500 two-sample t-test should be used.

Null hypothesis:there is no difference between the mean salaries of managers in
the West and the Prairies.

H0:muw-mup=0

Alternate hypothesis:there is no difference between the mean salaries of managers in
the West and the Prairies.
Ha:muw-mup!=0
"

"
c.	(2 marks) A test is conducted to determine if the salaries of managers in 
the West, Prairies, and East are the same. Identify the appropriate test and 
state the null and alternative hypotheses."

"To determine if the salaries of managers in 
the West, Prairies, and East are the same we should use ANOVA test as
it is used to compare the means of more than two groups

Null hypothesis:the mean salaries of managers in the West, Prairies, and East regions are the same
H0:muw=mup=mue

Alternate hypothesis:at least one of the means is different from the other regions
Ha:At least one mean is different
"


"d.	(2 marks) If companies were not randomly selected, and instead the salaries 
of marketing managers over the last three years were collected from the Government of
Canada, what kind of study would this be? Explain your reasoning."

"If companies were not randomly selected, and instead the salaries 
of marketing managers over the last three years were collected from the Government of
Canada,then the kind of study would this be is observational study.As the data is not collected randomly
or would it be easy to control the variables relationship with each other.
A statistical study is observational when it is conducted using pre-existing data, collected without any particular design"


"
#############	
#Question #2.	(17 marks) A doctor is interested in how exercise affects sleep, 
so she conducts an experiment. 30 healthy individuals are randomly assigned to three groups:
light exercise, moderate exercise, and heavy exercise. To collect her data,
she has the light exercise group come into her facility in the morning to perform the routine.
Then she has the moderate exercise group perform the routine after lunch.
Finally, the heavy exercise group performs their routine in the late afternoon.
She records the number of hours each participant sleeps that night in the file exercise.csv.
#############	
"


#a.	(1 mark) What is/are the factor(s) in this experiment? 
# The factor in this experiment is the level of exercise done

#b.	(3 marks) What are the levels of the factor(s)? 
#the levels of the factor exercise are Light exercise,moderate exercise,heavy exercise

#c.	(1 mark) What are the subjects? 
#The subjects are the 30 healthy individuals randomly assigned in three levels

#d.	(1 mark) What is the response variable? 
#The response variable is the number of hours of sleep each individual has at night.

#e.	(2 marks) What control issue(s) do you think is/are present in this design? Explain your thinking.
#The difference in sleep patterns and habits the each individual are having is one major control issue
#present which can potentially effect the #experiment.Also external factors such as
#daily schedule,stress levels and caffeine intakes can be control issues that are not 
#controlled by the doctor for the experiment.

"#f.	(2 marks) What type of experimental design is used (factorial, completely randomized, or
randomized block)? Explain your thinking." 

#This is completely randomized design as the 30 healthy individuals are randomly assigned to three groups:
#light exercise, moderate exercise, and heavy exercise

#g.	(2 marks) State the null and alternative hypothesis for this experiment. 
"
Null hypothesis:There is no significant difference in the mean number of hours slept among the three exercise groups.
H0:mul=mum=muh

Alternate hypothesis:at least one of the means is different from the other exercise groups
Ha:At least one mean is different
"

"#h.	(5 marks) Conduct an ANOVA to determine if different levels of exercise effect sleep at 𝛼 = 0.05.
Print out the ANOVA table (with the appropriate p-value), make a decision (reject or do not reject),
and interpret your result."
exercise<-read.csv(file.choose("exercise.csv"), header=TRUE, fileEncoding="UTF-8-BOM")
exercise.aov<-aov(Sleep~Routine,data=exercise)
(summary<-summary(exercise.aov))
print(summary)

" #ANOVA TABLE
            Df Sum Sq Mean Sq F value Pr(>F)  
Routine      2  5.727  2.8636   5.046 0.0122 *
Residuals   33 18.728  0.5675                 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"

#P-VALUE IS 0.0122<𝛼 = 0.05
#Decision: Reject the NULL HYPOTHESIS
#Interpretation: there are significant differences in the mean number of hours slept among the three exercise groups.

#Checking the conditions
hist(exercise.aov$residuals, main="Histogram of Residuals for Exercise ANOVA",  xlab="Residuals", col="orange")
#This histogram is nearly normal

boxplot(Sleep~Routine,data=exercise, main="Boxplot of Exercise level Differences",col=c("red", "blue", "green")) 
# looks similar and there is no severe skews