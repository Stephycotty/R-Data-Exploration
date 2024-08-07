library(tidyverse)
library(dplyr)
melanoma<-melanoma[-c(1)]
head(melanoma)
tail(melanoma)
melanoma<-melanoma%>%
  mutate_at(vars(status,sex,ulcer),as.factor)
melanoma <- melanoma %>%
  mutate(sex = recode_factor(sex, '1' = "Male", '0' = "Female")) %>%
  mutate(ulcer = recode_factor(ulcer, '1' = "Present", '0' = "Absent"))%>%
  mutate(status = recode_factor(status, '1' = "Died-Melanoma", '2' = "Alive",'3' ="Died-Other"))
summarise(melanoma) 
summary(melanoma)
  

#Histogram showing tumour thickness
library(ggplot2)
ggplot(melanoma, aes(x = thickness)) +
  geom_histogram(aes(y = ..density..),  # Plot density on the y-axis
                 color = "black", 
                 fill = "deepskyblue4", 
                 bins = 7) +  # Adjust the number of bins as needed
  geom_density(color = "red") +  # Add density curve with red color
  theme(plot.title = element_text(family = "Times New Roman", hjust = 0.3)) +
  labs(title = "Tumour Thickness Distribution", y = "Density", x = 'Thickness')

#Histogram showing Patient's Age
library(ggplot2)
ggplot(melanoma, aes(x = age)) +
  geom_histogram(aes(y = ..density..),  # Plot density on the y-axis
                 color = "black", 
                 fill = "deepskyblue4", 
                 bins = 10) +  # Adjust the number of bins as needed
  geom_density(color = "red") +  # Add density curve with red color
  theme(plot.title = element_text(family = "Times New Roman", hjust = 0.3)) +
  labs(title = "Patient Age Distribution", y = "Density", x = 'Age')

#Histogram showing Survial Time
library(ggplot2)
ggplot(melanoma, aes(x = time)) +
  geom_histogram(aes(y = ..density..),  # Plot density on the y-axis
                 color = "black", 
                 fill = "deepskyblue4", 
                 bins = 10) +  # Adjust the number of bins as needed
  geom_density(color = "red") +  # Add density curve with red color
  theme(plot.title = element_text(family = "Times New Roman", hjust = 0.3)) +
  labs(title = "SurvialTime(days)", y = "Density", x = 'Time')


#Boxplot showing thickness grouped by sex
ggplot(melanoma, aes(x=as.factor(sex), y=thickness)) + 
  geom_boxplot(fill="deepskyblue4") + theme(plot.title = element_text(family = "Times New Roman", hjust = 0.5),) + 
  labs(title = "Distribution of Tumour Thickness grouped by Sex", x="Sex",y = "Thickness(mm)")

#Boxplot showing thickness grouped by status
ggplot(melanoma, aes(x=as.factor(status), y=thickness)) + 
  geom_boxplot(fill="deepskyblue4") + theme(plot.title = element_text(family = "Times New Roman", hjust = 0.5),) + 
  labs(title = "Distribution of Tumour Thickness grouped by Status", x="Status",y = "Thickness(mm)")


#Ulcer detection grouped by sex
bar_plot_range <- ggplot(melanoma, aes(x = as.factor(sex), fill = ulcer)) +
  geom_bar(stat = "count", position = "dodge", color = "black") +
  geom_text(
    stat = "count",
    aes(label = paste0((..count..), " (", scales::percent(..count../sum(..count..)), ")")),
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  labs(title = "Ulcer Detection Grouped by Sex", x = "Sex", y = "count") +
  scale_y_continuous(breaks = seq(0, 100, by = 20), labels = paste0(seq(0, 100, by = 20))) +
  theme_minimal()
# Display the plot
print(bar_plot_range)


#barplot for Status vs  Ulcer
bar_plot_range <- ggplot(melanoma, aes(x = as.factor(ulcer), fill = status)) +
  geom_bar(stat = "count", position = "dodge", color = "black") +
  geom_text(
    stat = "count",
    aes(label = paste0((..count..), " (", scales::percent(..count../sum(..count..)), ")")),
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  labs(title = "Staus grouped by Ulcer detection", x = "Ulcer", y = "count") +
  scale_y_continuous(breaks = seq(0, 100, by = 20), labels = paste0(seq(0, 100, by = 20))) +
  theme_minimal()
# Display the plot
print(bar_plot_range)

#3
attach(melanoma)
# correlation between time and thickness
cor(melanoma$time,melanoma$thickness,method = "pearson")
#Regression Analysis of time vs age
regression_model <- lm(formula = time~thickness)
regression_model
# Scatter plot with regression line
par(bg = "white")  # Set background color
plot(
  x = melanoma$thickness,
  y = melanoma$time,
  main = "Scatter plot Showing Survial time vs Tumour Thickness",
  ylab = "Survial Time(days)",
  xlab = "Tumour thickness(mm)",
  col = "deepskyblue4",   # Darken the points
  pch = 16,           # Change point type
  cex = 1.2,          # Increase point size
  grid.col = "black",  # Darken the grid
  lwd = 1.9           # Increase line width for the grid
)  
grid()
# Add regression line
summary(regression_model)
abline(regression_model,col = "red",lwd =2)



attach(melanoma)
# correlation between time vs age
cor(melanoma$time,melanoma$age,method = "pearson")
#Regression Analysis of time vs age
regression_model1 <- lm(formula = time~age)
regression_model1
# Scatter plot with regression line
par(bg = "white")  # Set background color
plot(
  x = melanoma$age,
  y = melanoma$time,
  main = "Scatter plot Showing Survial time vs Patient Age",
  ylab = "Survial Time(days)",
  xlab = "Patient Age(years)",
  col = "deepskyblue4",   # Darken the points
  pch = 16,           # Change point type
  cex = 1.2,          # Increase point size
  grid.col = "black",  # Darken the grid
  lwd = 1.9           # Increase line width for the grid
)  
grid()
# Add regression line
summary(regression_model1)
abline(regression_model1,col = "red",lwd =2)

attach(melanoma)
# correlation between thickness vs age
cor(melanoma$thickness,melanoma$age,method = "pearson")
#Regression Analysis of thickness vs age
regression_model2 <- lm(formula = thickness~age)
regression_model2
# Scatter plot with regression line
par(bg = "white")  # Set background color
plot(
  x = melanoma$age,
  y = melanoma$thickness,
  main = "Scatter plot Showing Tumour Thickness vs Patient Age",
  ylab = "Tumour Thickness(mm)",
  xlab = "Patient Age(years)",
  col = "deepskyblue4",   # Darken the points
  pch = 16,           # Change point type
  cex = 1.2,          # Increase point size
  grid.col = "black",  # Darken the grid
  lwd = 1.9           # Increase line width for the grid
)  
grid()
# Add regression line
summary(regression_model2)
abline(regression_model2,col = "red",lwd =2)


#5
#Significance sample testing
#T-test for difference in mean of times between males and females
test1 <- t.test(time ~ sex, data = melanoma)
test1
#T-test for difference in mean of thickness between males and females
test2 <- t.test(thickness ~ sex, data = melanoma)
test2
#T-test for difference in mean of ages between males and females
test3 <- t.test(age ~ sex, data = melanoma)
test3

#6
# Update all installed packages
update.packages(ask = FALSE)



par (mfrow=c(2,2))
library(ggplot2)

#QQ plot for Survival time grouped by sex
qq_plot <- ggplot(melanoma, aes(sample = time)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Survial Time Grouped by Gender",x = "Theoretical", y = "Sample") +
  facet_wrap(~sex, scales = "free")

# Display the plot
print(qq_plot)

#QQ plot for 'Thickness' grouped by 'sex'
qq_plot <- ggplot(melanoma, aes(sample = thickness)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Thickness of Tumour Grouped by Gender",x = "Theoretical", y = "Sample") +
  facet_wrap(~sex, scales = "free")

# Display the plot
print(qq_plot)

#QQ plot for Patient Age grouped by sex
qq_plot <- ggplot(melanoma, aes(sample = age)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot for Patient Age Grouped by Gender",x = "Theoretical", y = "Sample") +
  facet_wrap(~sex, scales = "free")

# Display the plot
print(qq_plot)
















