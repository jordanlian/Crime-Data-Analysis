# import libraries
library(crimedata)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(fitdistrplus)

' Import crime_data() and save as new variable origin_crime, 
and then save new variable crime_data to manipulate without altering original data'
origin_crime <- get_crime_data()
str(origin_crime)
head(origin_crime)

crime_data <- origin_crime
crime_data

# Filter offense code and offense type for better understanding
code_class <- distinct(crime_data, offense_code, offense_type)
code_class

# Count number of offense codes and offense types
length(unique(crime_data$offense_code))
length(unique(crime_data$offense_type))

# Filter and plot total crimes per city in 2019
city_count <- table(crime_data$city_name)
barplot(city_count, main="Total Number of Crimes per City in 2019",
        xlab="City",
        ylab="Frequency")

# Change Date Formats to Month only
crime_data$date_single <- format(as.POSIXct(crime_data$date_single, format='%Y/%m/%d %H:%M:%S'),format='%m')

# Filter crime frequency per month
date_freq <- as.data.frame(table(crime_data$date_single))
date_freq$avg <- as.numeric(as.character(date_freq$Freq)) / 15
colnames(date_freq) = c("Month", "Total_Freq", "Avg_Freq")
date_freq

#Monthly Crime Rates
  #Plot of crime frequency per month as scatter plot
freq_scatter <- ggplot(date_freq) +
  geom_point(mapping = aes(x = Month, y = Avg_Freq))
print(freq_scatter + labs(y="Average Frequency per City"))

  #Plot of crime frequency per month as bar plot
barplot(date_freq$Avg_Freq, names.arg=date_freq$Month, main="Crime Frequency per Month",
        xlab="Month",
        ylab="Average Frequency per City")

  #mean crimes per month
mean_crimes <- mean(date_freq$Avg_Freq)
mean_crimes

  #variance of crimes per month
var_crimes <- var(date_freq$Avg_Freq)
var_crimes

  #standard deviation of crimes per month
stdev_crimes <- sd(date_freq$Avg_Freq)
stdev_crimes

# New York Crime Data set, filtered from crime_data
NY <- crime_data[crime_data$city_name == "New York",]
NY

  #count total number of crimes in NY in 2019
NY_count <- count(NY)
NY_count

  #group crimes in NY by month
NY_date_freq <- as.data.frame(table(NY$date_single))
colnames(NY_date_freq) = c("Month", "Month_Freq")
NY_date_freq

  #group total number of crimes in NY by offense code and create bar plot
NY_crime <- table(NY$offense_code)
barplot(NY_crime, main="New York Total Crime",
        xlab="Offense Code",
        ylab="Frequency")

  #create scatterplot of crimes in NY by month
legend_key <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15), hjust=0.5))
NY_scatter <- ggplot(NY_date_freq) +
  geom_point(mapping = aes(x = Month, y = Month_Freq))
print(NY_scatter + legend_key + labs(y="Crimes Each Month", title="New York Monthly Crimes"))

  #mean of crimes in NY per month
NY_mean <- mean(NY_date_freq$Month_Freq)
NY_mean

  #variance of crimes in NY per month
NY_var <- var(NY_date_freq$Month_Freq)
NY_var

  #standard deviation of crimes in NY per month
NY_stdev <- sd(NY_date_freq$Month_Freq)
NY_stdev

  # Hypothesis Test of Unequal Variances - NY
NY_chi <- (NY_count - 1)*NY_var / var_crimes
NY_chi
NY_var
# Austin Crime Data set, filtered from crime_data
Aus <- crime_data[crime_data$city_name == "Austin",]
Aus

  #count total number of crimes in Austin in 2019
Aus_count <- count(Aus)
Aus_count

  #group crimes in Austin by month
Aus_date_freq <- as.data.frame(table(Aus$date_single))
colnames(Aus_date_freq) = c("Month", "Month_Freq")
Aus_date_freq

  #group total number of crimes in Austin by offense code and create bar plot
Aus_crime <- table(Aus$offense_code)
barplot(Aus_crime, main="Austin Total Crime",
        xlab="Offense Code",
        ylab="Frequency")

  #create scatterplot of crimes in Austin by month
Aus_Scatter <- ggplot(Aus_date_freq) +
  geom_point(mapping = aes(x = Month, y = Month_Freq))
print(Aus_Scatter + legend_key + labs(y="Crimes Each Month", title="Austin Monthly Crimes"))

  #mean of crimes in Austin per month
Aus_mean <- mean(Aus_date_freq$Month_Freq)
Aus_mean

  #variance of crimes in Austin per month
Aus_var <- var(Aus_date_freq$Month_Freq)
Aus_var

  #standard deviation of crimes in Austin per month
Aus_stdev <- sd(Aus_date_freq$Month_Freq)
Aus_stdev

# Chicago Crime Data set, filtered from crime_data
Chi <- crime_data[crime_data$city_name == "Chicago",]
Chi

  #count total number of crimes in Chicago in 2019
Chi_count <- count(Chi)
Chi_count

  #group crimes in Chicago by month
Chi_date_freq <- as.data.frame(table(Chicago$date_single))
colnames(Chi_date_freq) = c("Month", "Month_Freq")
Chi_date_freq

  #group total number of crimes in Chicago by offense code and create bar plot
Chi_crime <- table(Chicago$offense_code)
barplot(Chi_crime, main="Chicago Total Crime",
        xlab="Offense Code",
        ylab="Frequency")

  #create scatterplot of crimes in Chicago by month
Chi_scatter <- ggplot(Chi_date_freq) +
  geom_point(mapping = aes(x = Month, y = Month_Freq))
print(Chi_scatter + legend_key + labs(y="Crimes each Month", title="Chicago Monthly Crimes"))

  #mean of crimes in Chicago per month
Chi_mean <- mean(Chi_date_freq$Month_Freq)
Chi_mean

  #variance of crimes in Chicago per month
Chi_var <- var(Chi_date_freq$Month_Freq)
Chi_var

  #standard deviation of crimes in Chicago per month
Chi_stdev <- sd(Chi_date_freq$Month_Freq)
Chi_stdev

# LA Crime Data set, filtered from crime_data
LA <- crime_data[crime_data$city_name == "Los Angeles",]
LA

  #count total number of crimes in LA in 2019
LA_count <- count(LA)
LA_count

  #group crimes in LA by month
LA_date_freq <- as.data.frame(table(LA$date_single))
colnames(LA_date_freq) = c("Month", "Month_Freq")
LA_date_freq

  #group total number of crimes in LA by offense code and create bar plot
LA_crime <- table(LA$offense_code)
barplot(LA_crime, main="Los Angeles Total Crime",
        xlab="Offense Code",
        ylab="Frequency")

  #create scatterplot of crimes in LA by month
LA_scatter <- ggplot(LA_date_freq) +
  geom_point(mapping = aes(x = Month, y = Month_Freq))
print(LA_scatter + legend_key + labs(y="Crimes each Month", title="Los Angeles Monthly Crime"))

  #mean of crimes in Chicago per month
LA_mean <- mean(LA_date_freq$Month_Freq)
LA_mean

  #variance of crimes in Chicago per month
LA_var <- var(LA_date_freq$Month_Freq)
LA_var

  #standard deviation of crimes in Chicago per month
LA_stdev <- sd(LA_date_freq$Month_Freq)
LA_stdev

# Grouped Bar Plot
month_list <- c(1:12)
as.data.frame(month_list)
city_crime <- bind_cols(month_list, NY_date_freq$Month_Freq, Chi_date_freq$Month_Freq, LA_date_freq$Month_Freq, Aus_date_freq$Month_Freq)
colnames(city_crime) = c("Month", "NY_Month_Freq", "Chi_Month_Freq", "LA_Month_Freq", "Aus_Month_Freq")
city_crime <- as.data.frame(city_crime)
city_crime

# Modify Dataframe
city_long <- gather(city_crime, variable, value, -Month)
city_long

# Bar Plot
city_bar <- ggplot(city_long, aes(Month, value))
month_crimes <- city_bar + geom_bar(stat = "identity", aes(fill =  variable), position = "dodge") +
  xlab("Months") + ylab("Frequency") +
  ggtitle("Monthly Crimes by City")
print(month_crimes + legend_key + scale_fill_discrete(name = "City", labels = c("Austin", "Chicago", "Los Angeles", "New York")))

# Boxplot
boxplot(value~variable,data=city_long, main="Monlthy Crimes in NY, LA, Chicago, Austin",
        xlab="New York", ylab="Crimes per Month")

# Correlation of total crimes per city to average income
a <- data.frame(city_count)
colnames(a) = c("City", "Yearly_Crime")
a
Avg_Salary <- c(67462, 65883, 55198, 29481, 59255, 43573, 58385, 32131, 54700, 54505, 60762, 104552, 85562, 41107, 41625)
Population <- c(978908, 692600, 2693976, 670031, 909585, 152960, 3979576, 770517, 518012, 692587, 8336817, 881549, 753675, 300576, 548073)
data <- cbind(a,Avg_Salary)
Cor_data <- cbind(data,Population)
Cor_data

#correlation for yearly crime and average salary
cor(Cor_data$`Yearly_Crime`, Cor_data$Avg_Salary)
cor_crime_salary <- ggplot(Cor_data) +
  geom_point(mapping = aes(x = Avg_Salary, y = Yearly_Crime))
print(cor_crime_salary + legend_key + labs(x="Average Yearly Salary", y="Yearly Crime", title="Correlation Plot: Avg. Yearly Salary vs Yearly Crime Rates"))
ggplot(data = Cor_data, aes(x=Cor_data$Avg_Salary, y=Cor_data$Yearly_Crime))+
  geom_point(aes(size=City, color=City))+
  labs(x="Average Salary", y="Yearly Crime")  
#correlation for yearly crime and population
cor(Cor_data$`Yearly_Crime`, Cor_data$Population)
cor_crime_population <- ggplot(Cor_data) +
  geom_point(mapping = aes(x = Population, y = Yearly_Crime))
print(cor_crime_population + legend_key + labs(x="Population", y="Yearly Crime", title="Correlation Plot: Avg. Population vs Yearly Crime Rates"))
ggplot(data = Cor_data, aes(x=Cor_data$Population, y=Cor_data$Yearly_Crime))+
  geom_point(aes(size=City, color=City))+
  labs(x="Population", y="Yearly Crime")

#regression for advanced analytics
descdist(date_freq$Avg_Freq)
fit_n <- fitdist(date_freq$Avg_Freq, "norm")
summary(fit_n)
fit_ln <- fitdist(date_freq$Avg_Freq, "lnorm")
summary(fit_ln)

gofstat((list(fit_n,fit_ln)))

par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(list(fit_n), legendtext = plot.legend, xlab = "Average Crime", xlegend = 'topleft')
cdfcomp(list(fit_n), legendtext = plot.legend, xlab = "Average Crime",)
qqcomp(list(fit_n), legendtext = plot.legend, xlab = "Average Crime",)
ppcomp(list(fit_n), legendtext = plot.legend, xlab = "Average Crime",)