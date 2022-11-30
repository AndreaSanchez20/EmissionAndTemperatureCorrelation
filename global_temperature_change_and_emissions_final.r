#This project focuses on answering the question:how significant is green house gas emissions to climate change?

#This project uses GlobalTemperatures.csv and emission_data.csv

emission_data <-  read.csv("C:/Andrea/ENTM_data_science/climate_change/data/emission_data.csv", header=FALSE)
emission_data

world_emission_data <- emission_data[emission_data$V1 == "World" | emission_data$V1 == "Country",]
world_emission_data
#https://statisticsglobe.com/filter-data-frame-rows-by-logical-condition-in-r

#redefine row and column names
rownames(world_emission_data) <- c("Year","Emission")

#delete 1st row
world_emission_data = world_emission_data[,-1]

#transpose table
world_emission_data <- t(world_emission_data)

head(world_emission_data)


#filter by year. return only 1990-2012
world_emission_data_clean <- world_emission_data[world_emission_data[,1]<=2012 & world_emission_data[,1]>=1990,]
str(world_emission_data_clean)
world_emission_data_clean

#Save dataframe as a csv file
write.csv(world_emission_data_clean,"C:/Andrea/ENTM_data_science/climate_change/data/world_emission_data_clean.csv", row.names = FALSE)

global_temperatures <- read.csv("C:/Andrea/ENTM_data_science/climate_change/data/GlobalTemperatures.csv")
global_temperatures

#Add a year column for each row
df_new <- global_temperatures
df_new$Year <- strftime(df_new$dt, "%Y")
head(df_new)

#Take the mean temperature of each year
df_avg_temp <- aggregate(LandAverageTemperature~Year, df_new, FUN = mean )
#df_avg_temp_min <- aggregate(LandAverageTemperature~Year, df_new, FUN = min )

head(df_avg_temp)
#head(df_avg_temp_min)

#get data from 1990-2012
global_avg_temp <- df_avg_temp[df_avg_temp[,1]<=2012 & df_avg_temp[,1]>=1990,]

head(global_avg_temp)

#Write dataframe as a csv file
write.csv(global_avg_temp,"C:/Andrea/ENTM_data_science/climate_change/data/global_avg_temp.csv", row.names = FALSE)

#emission_avg_temp <- rbind(world_emission_data_clean, global_avg_temp)
emission_avg_temp <- merge(x = world_emission_data_clean, y = global_avg_temp, by = "Year")
head(emission_avg_temp)
write.csv(emission_avg_temp,"C:/Andrea/ENTM_data_science/climate_change/data/emission_avg_temp.csv", row.names = FALSE)

#read previous dataframe and store the data into a dataframe 
#scaled the data to see results and saved it in emission_and_temp_data_world.csv file
#global_avg_temp_and_emission <- read.csv("C:/Andrea/ENTM_data_science/climate_change/data/emission_and_temp_data_world.csv")
global_avg_temp_and_emission <- read.csv("C:/Andrea/ENTM_data_science/climate_change/data/emission_avg_temp.csv")

global_avg_temp_and_emission

#Variable 1: Emission
#Plot of GHG temperature vs. year
emission_year_plot <- plot(global_avg_temp_and_emission$Year,global_avg_temp_and_emission$Emission,
                           xlab = "Year", ylab = "GHG Emissions (tonnes)",
                           main = "Global Green House Gas Emissions" )

#This plot shows that greenhouse gas emissions steadily increase every year. 
#It shows that it is a fact that every year there are more greenhouse gas emissions

#Emission summary
summary(global_avg_temp_and_emission[,2])

#The minimum emission value is 803 and the maximum emission is 1400

#Variable 2: World Average Temperature
#The average temperature measured from a global scale. 

#plot of year vs. average world temperature
temp_year_plot <- plot( global_avg_temp_and_emission$Year,global_avg_temp_and_emission$LandAverageTemperature,
                       xlab = "Year", ylab = "Avg Temperature (Celsius)", ylim=c(2,10),
                       main = "Global Temperatures" )

#This plot shows the fluctuations of temperature throughout the years.
#The temperature rises and lowers but increases during the years.

#Avg world temperature summary
summary(global_avg_temp_and_emission[,3])

#The minimum  temperature is 0.75 nd tyhe maximum temperature has gotten is 9.73 degrees Celsius

#The linear model is used to find the relationship between the gas emissions and average world temperature
#A regression model is chosen because it's a method that determines the strength of the realtionship between two variables.
#The linear regression model shows the linear relationship between 2 vaariables using a line of best fit
#sources: https://www.investopedia.com/terms/r/regression.asp
x<-global_avg_temp_and_emission$Emission
y<-global_avg_temp_and_emission$LandAverageTemperature

relation<-lm(y~x)
print(summary(relation))

# Plot the chart.
plot(x,y,col = "blue",main = "Temperature (C) & Greenhouse Emission",
     abline(lm(y~x)),cex = 1.3,pch = 16,xlab = "Greenhouses Emission (tonnes)",ylab = "Temperature (C)", ylim=c(2,10))

#Results:
#As the emissions increase,the temperature also rises.
#The trendline shows a steady increase in both variables. 
#We can see that emissions create a shift on temperature
#

#Correlation is the relationship between two variables. It defines the degree to which two variables are linearly related
#We are looking at the correlation because this is helpful to indicate a preictive relationship of both varibles
#Sources: jmp.com/en_us/statistics-knowledge-portal/what-is-correlation.html


cor.test(x, y, method=c("pearson"))

#Results:
#The correlation value between GHG emissions and avg temperature is 0.75 which s considered a strong positive relationship.
#This result means that when emissions increases, the average temperature also increases
#The larger the sample size and the more extreme the correlation (closer to -1 or 1), the more likely the null hypothesis of no correlation will be rejected.

#Sources:
#http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
#https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/#between-two-variables
#https://www.statology.org/what-is-a-strong-correlation/
#https://www.nrdc.org/stories/global-warming-101






