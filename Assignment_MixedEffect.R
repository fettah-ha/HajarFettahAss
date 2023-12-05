#1)
data(sleepstudy)
install.packages("Matrix")
library(lme4)
data(sleepstudy)
head(sleepstudy)
#B)
str(sleepstudy)
hist(sleepstudy$Reaction)
hist(sleepstudy$Days)
hist(sleepstudy$Subject)
#Reaction varibale
#Histogram
library(ggplot2)
ggplot(sleepstudy, aes(x=Reaction))+geom_histogram(fill="blue", color="black")+
  labs(title="Distribution of Reaction", x="Reaction", y="Frequency")
#Density
library(ggplot2)
ggplot(sleepstudy, aes(x = Reaction)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Density Plot of Reaction", x = "Reaction", y = "Density")
#Boxplot

boxplot(sleepstudy$Reaction, main="Boxplot of reaction", 
        xlab="Reaction", col="skyblue")
#Days varibale
#hystogram
library(ggplot2)
ggplot(sleepstudy, aes(x=Days))+geom_histogram(fill="pink", color="black")+
  labs(title="Distribution of Days", x="Days", y="Frequency")
#density
ggplot(sleepstudy, aes(x = Days)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Density Plot of Days", x = "Days", y = "Density")
#Boxplot
boxplot(sleepstudy$Days, main="Boxplot of Days", 
        xlab="Days", col="skyblue")
#Subject variable
ggplot(sleepstudy, aes(x=as.factor(Subject)))+geom_histogram(fill="blue", color="black")+
  labs(title="Distribution of Subject", x="Subject", y="Frequency")

ggplot(sleepstudy, aes(x = as.factor(Subject))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Subject", x = "Subject", y = "Frequency")
#Boxplot for reaction for eah subject
ggplot(sleepstudy, aes(x = as.factor(Subject), y = Reaction)) +
  geom_boxplot() +
  labs(title = "Reaction Times for Each Subject", x = "Subject", y = "Reaction Time")

#Boxplot for reaction for each day 
ggplot(sleepstudy, aes(x =Days, y = Reaction)) +
  geom_boxplot() +
  labs(title = "Reaction Times for Each day", x = "Days", y = "Reaction Time")
#Scatter plot to visulize the realtion ship between reaction and days 
ggplot(sleepstudy, aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relationship between Days and Reaction ", x = "Days", y = "Reaction")

#2)
#a) Compute and report summary statistics for the key variables
summary(sleepstudy$Reaction)
summary(sleepstudy$Days)
summary(sleepstudy$Subject)
#b) Create visualizations (e.g., histograms, boxplots) to better understand the distribution of reaction times over different days.
#Boxplot of Reaction per days
ggplot(sleepstudy, aes(x = Days, y = Reaction)) +
  geom_boxplot() +
  labs(title = "Reaction Over  Days", x = "Days", y = "Reaction")
#Histogram Reaction
library(ggplot2)
ggplot(sleepstudy, aes(x=Reaction))+geom_histogram(fill="blue", color="black")+
  labs(title="Distribution of Reaction", x="Reaction", y="Frequency")

#Density for Reaction
library(ggplot2)
ggplot(sleepstudy, aes(x = Reaction)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Density Plot of Reaction", x = "Reaction", y = "Density")

#3)Fitting model
install.packages("lme4")
library(lme4)
m_model <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
summary(m_model)

#4)Interpretation
#The estimated variance for the random intercepts among subjects is 612.10
#The standard deviation of the random intercept among subject is 24.741
#The intercept (251.405) represents the estimated mean Reaction when Days is zero.
#The correlation between the random intercepts and the random effect of Days is low (0.07)
#The intercept (251.405) represents the estimated mean Reaction when Days is zero.
#The negative correlation between the intercept and the effect of Days (-0.138) suggests that subjects with higher initial Reaction times tend to have a smaller increase in Reaction over time.

#5) Residuals
residuals <- residuals(m_model)
#Visualization of the residuals 
library(ggplot2)
ggplot(sleepstudy, aes(x = fitted(m_model), y = residuals)) +
  geom_point() +
  labs(title = "Residual", x = "Fitted Values", y = "Residuals")
# Checking Normality if residuals
qqnorm(residuals, main = "Normal Q-Q Plot of Residuals")
qqline(residuals)
#the points on the Q-Q plot approximately fall along a straight line, it suggests that the residuals follow a normal distribution
#Plot of Residuals VS Fitted values of the model
residuals <- residuals(m_model)
fitted_values <- fitted(m_model)
plot(fitted_values, residuals, ylab = "Residuals", xlab = "Fitted Values",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")