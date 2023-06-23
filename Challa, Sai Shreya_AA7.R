"
****************************************************************
Name:Sai Shreya Challa
Student Number:A00264982

QMM1002 Module 7 Applied Activity
****************************************************************
"

"
#############	
#Question #1
1.	(22 marks) An experiment on mung beans was performed to investigate the environmental effects of 
salinity and water temperature on sprouting. Beans were randomly allocated to each of 36 petri dishes
that were subject to one of four levels of Salinity (0ppm, 4ppm, 8ppm, and 12 ppm) and one of three 
Temperatures (32°C, 34°C, or 36° C). After 48 hours, the biomass of the sprouts in grams per meter (gm) 
was measured to determine which combination of Salinity and Temperature results in the highest biomass of
bean sprouts. The data is given in the file “sprouts.csv”.  
#############
"
library(ggplot2)

# Read the data from "sprouts.csv"
sprouts_data<- read.csv(file = file.choose("sprouts.csv"),header=TRUE,sep=',')


"a. (4 marks) Create three boxplots for biomass by salinity, biomass by temperature, and biomass by 
both salinity and temperature. Based on these plots, explain if you think higher or lower salinity and
temperatures result in higher biomass for sprouts. 
"
# Boxplot for biomass by salinity
boxplot(Biomass~Salinity, data = sprouts_data, xlab = "Salinity", ylab = "Biomass", main = "Biomass by Salinity")

# Boxplot for biomass by temperature
boxplot(Biomass ~Temperature, data = sprouts_data, xlab = "Temperature", ylab = "Biomass", main = "Biomass by Temperature")

# Boxplot for biomass by both salinity and temperature
boxplot(Biomass ~ Salinity +Temperature, data = sprouts_data, xlab = "Salinity and Temperature", ylab = "Biomass", main = "Biomass by Salinity and Temperature")

"
The relationship between biomass and salinity/temperature:
Higher salinity levels (8ppm and 12ppm) tend to result in lower biomass compared to lower salinity levels (0ppm and 4ppm).
Higher temperature(36 C) tend to result in higher biomass compared to lower temperature levels (32C and 34C).
"

"
#b.	(2 marks) State the null and alternative hypotheses for a two-way analysis of variance to determine 
the effect the factors salinity and temperature have on biomass."

"Null hypothesis (H0): There is no significant effect of salinity and temperature on the biomass of bean sprouts.
#H0: mu_salinity = mu_temperature
#H0: The effects of Biomass are constant across the samples
Alternative hypothesis (Ha): There is a significant effect of salinity and/or temperature on the biomass of bean sprouts.
Ha:at least one mean is different than the others
"


"
#c.	(4 marks) Conduct the two-way analysis of variance at the 𝛼 = 0.05 level. Print a summary of the 
completed ANOVA table. State your decision regarding the null hypotheses (using the p-values or critical
values) and interpret your result in the context of the problem. 
"

model <- aov(Biomass ~ Salinity * Temperature, data = sprouts_data)
summary(model)
"                     Df Sum Sq Mean Sq F value   Pr(>F)    
Salinity              3  36.47  12.157  16.981 3.93e-06 ***
Temperature           2  34.72  17.358  24.247 1.73e-06 ***
Salinity:Temperature  6   5.30   0.883   1.233    0.324    
Residuals            24  17.18   0.716                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"
#Check p-value for interaction term first
#p = 0.324 < 0.05 = alpha
#Reject the null for Interaction
#There is a significant effect of salinity and/or temperature on the biomass of bean sprouts where higher temperature and lower salinity results in higher bio mass




"
#d.	(2 marks) Are the similar variance and normality conditions satisfied? Create at least 2 plots and
interpret the plots to explain your answers. 
"
#Histogram of residuals for Nearly Normal Condition
hist(model$residuals, xlab="Residuals", 
     main="Histogram of Residuals") 
#no skew, this passes the nearly normal condition.

#check other residual plots
plot(model) 
#Option 1: Fitted vs. Residuals - not much indication of spread in variance
#Option 2: QQ plot - normal


"
#e.	(4 marks) Create an interaction plot (Hint: use temperature on the x-axis). Explain how this plot 
supports the conclusion from the ANOVA test regarding the significance of the main effects and 
interaction. 
"

interaction.plot(sprouts_data$Salinity, sprouts_data$Temperature, sprouts_data$Biomass, 
                 xlab = "Temperature", ylab = "Biomass", main = "Interaction Plot", trace.label="Temperature c")

##On the plot High temperature is the most different from the rest and gives the highest biomass.


"
#f.	(3 marks) Perform Tukey’s HSD Test. Interpret any significant values. Which salinity/temperature 
combination would you recommend?
"
TukeyHSD(model, conf.level=0.95)

"Salinity:Temperature:
The combination 0ppm:34C-0ppm:32C is significant (p adj = 0.0141316).
The combination 0ppm:36C-0ppm:32C is significant (p adj = 0.0077689).
The combination 0ppm:36C-0ppm:34C is significant (p adj = 0.1766667).
"
 
#0ppm:36C-0ppm:34C combination has a significantly higher mean biomass value 
 
"
#g.	(3 marks) Create a grouped bar plot for the mean of biomass by salinity and temperature. Use a 
chosen colour palette. Explain how this bar plot supports your recommendation for the best 
salinity/temperature combination from part f.
"
ggplot(sprouts_data, aes(x =Salinity, y = Biomass, fill = Temperature)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Salinity", y = "Mean Biomass", fill = "Temperature") +
  scale_fill_manual(values = c("pink", "brown", "blue")) +
  ggtitle("Mean of Biomass by Salinity and Temperature")

#0ppm:36C-0ppm:34C combination has a significantly higher mean biomass value compared to the other bars, it supports the recommendation that this combination is the best for achieving higher biomass
