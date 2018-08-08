#Problem A
# Assumptions of ANOVA 
#1. Homogenity of variance: The populations have same variance
#2. Normal Distribution of populations
#3. Independent sampling of values.
yeast_data<- read.delim("YEAST DATA.txt", header = F, sep = "")
heads_yeast <- c("Sequence Name","mcg","gvh","alm","mit","erl","pox","vac","nuc","Class")
colnames(yeast_data) <- heads_yeast
anova_result <- aov(nuc ~ Class, data = yeast_data)
anova_result
summary(anova_result)
#Now we will verify ANOVA assumptions.
#Normality
qqnorm(yeast_data$nuc)
qqline(yeast_data$nuc)

# As per the diagram displayed, The data is normally distributed with littlebit of skewness 
#as most of the data points are falling allong with reference line but some are placed away from it.
#a histogram will clarify the skewness of the distribution.
hist(yeast_data$nuc ,xlab = "nuc", ylab = "frequency",main="NUC",col="yellow")
# as we can see, more data is concentrated for nuc valuee 0.2, 
#and distribution is skewed towards this concentrated data

#so apparently yeast_data is not exactly a fit for anova test.
# P value in summary of anova_results are less than 0.05 
#we can also conclude that the assumptions are not accurate.

#Problem B
#As per above analysis, we can conclude that YEAST_DATA(NUC) is not a fit for ANOVA TEST.
# which means, the data we tested is skewed towards a small range of data points, 
# and not normally distributed
# So ANOVA test can be used to check if the assumptions are correct or not.
# For statistically analysing data we have more options in R.
# 1. Plots
# 2. LeveneTest
# 3. Bartletts test
summary(leveneTest(nuc ~ Class, data = yeast_data,center = mean))
summary(leveneTest(nuc ~ Class, data = yeast_data,center = median))

anova_residuals<- residuals(anova_result)
shapiro.test(x = anova_residuals )
#	Shapiro-Wilk normality test

#data:  anova_residuals
#W = 0.7959, p-value < 2.2e-16

# Null hypothesis rejected.