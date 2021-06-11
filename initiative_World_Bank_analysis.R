library(WDI)
WDIsearch('internet')
#Internet (% of population), Corruption Control, Government Effectiveness, Political Stability, Regulatory Quality, Rule of Law, Social Contribution (% of revenue)
df <- WDI(indicator = c('IT.NET.USER.ZS', 'CC.EST', 'GE.EST', 'PV.EST', 'RQ.EST', 'RL.EST', 'GC.REV.SOCL.ZS'),
          start = 2010, end = 2019, extra = FALSE)

head(df, 20)
summary(df)
dim(df)
View(df)

del_cols <- 'iso2c'
df <- df[ , !(names(df) %in% del_cols)]

#Renaming column names
names(df)[names(df) == 'country'] <- 'Country'
names(df)[names(df) == 'year'] <- 'Year'
names(df)[names(df) == 'IT.NET.USER.ZS'] <- 'Internet_Users'
names(df)[names(df) == 'CC.EST'] <- 'Corruption_Control'
names(df)[names(df) == 'GE.EST'] <- 'Government_Effectiveness'
names(df)[names(df) == 'PV.EST'] <- 'Political_Stability'
names(df)[names(df) == 'RQ.EST'] <- 'Regulatory_Quality'
names(df)[names(df) == 'RL.EST'] <- 'Rule_of_Law'
names(df)[names(df) == 'GC.REV.SOCL.ZS'] <- 'Social_Contribution'

#Removing NA values
df <- na.omit(df)
View(df)
dim(df)

#Filtering from 1st Quartile to 3rd Quartile
library(dplyr)
data_filtered <- filter(df, Internet_Users > 24.61 & Internet_Users < 50.26)
data_filtered <- filter(df, Corruption_Control > -0.6163 &  Corruption_Control < 0.1418)
data_filtered <- filter(df, Government_Effectiveness > -0.58126 & Government_Effectiveness < 0.18251)
data_filtered <- filter(df, Political_Stability > -0.49899 & Political_Stability < 0.08176)
data_filtered <- filter(df, Regulatory_Quality > -0.4359 & Regulatory_Quality < 0.2102)
data_filtered <- filter(df, Rule_of_Law > -0.56677 & Rule_of_Law < 0.14538)
data_filtered <- filter(df, Social_Contribution > 0.000 & Social_Contribution < 12.879)

#data_filtered <- filter(df, Internet_Users > 50.26 & Internet_Users < 75.42)
#data_filtered <- filter(df, Corruption_Control > 0.1418 &  Corruption_Control < 0.8391)
#data_filtered <- filter(df, Government_Effectiveness >  0.18251 & Government_Effectiveness < 0.98089)
#data_filtered <- filter(df, Political_Stability >  0.08176 & Political_Stability < 0.84221)
#data_filtered <- filter(df, Regulatory_Quality >  0.2101 & Regulatory_Quality < 0.9712)
#data_filtered <- filter(df, Rule_of_Law > 0.14538 & Rule_of_Law < 0.85553)
#data_filtered <- filter(df, Social_Contribution >  12.879 & Social_Contribution < 27.821)

View(data_filtered)
dim(data_filtered)

#Further filtering based on only internet users
summary(data_filtered)
data_filtered <- filter(data_filtered, Internet_Users > 47.12 & Internet_Users < 70.58)

#Selecting countries from main data frame
names <- unique(data_filtered[,1])
names <- as.array(names)
library(stringr)

counter <- 0
new_df <- data.frame(matrix(NA, nrow = 1, ncol = 9))
for (i in 1:nrow(df)){
  for (j in 1:nrow(names)){
    if(!is.na(str_match(df$Country[i], names[j]))){
      counter <- counter + 1
      new_df[counter, ] <- df[i,]
    }
  }
}

View(new_df)

#Visualization
library(DataExplorer)
plot_histogram(new_df)

library(ggplot2)
#Box plots
ggplot(data = new_df, mapping = aes(x = new_df$Country, y= new_df$Internet_Users)) + geom_boxplot() + coord_flip()
ggplot(data = new_df, mapping = aes(x = new_df$Country, y= new_df$Corruption_Control)) + geom_boxplot() + coord_flip()
ggplot(data = new_df, mapping = aes(x = new_df$Country, y= new_df$Government_Effectiveness)) + geom_boxplot() + coord_flip()
ggplot(data = new_df, mapping = aes(x = new_df$Country, y= new_df$Political_Stability)) + geom_boxplot() + coord_flip()
ggplot(data = new_df, mapping = aes(x = new_df$Country, y= new_df$Regulatory_Quality)) + geom_boxplot() + coord_flip()
ggplot(data = new_df, mapping = aes(x = new_df$Country, y= new_df$Rule_of_Law)) + geom_boxplot() + coord_flip()
ggplot(data = new_df, mapping = aes(x = new_df$Country, y= new_df$Social_Contribution)) + geom_boxplot() + coord_flip()

#plotting
ggplot(data = new_df) +
  geom_line(mapping = aes(x=new_df$Year, y=new_df$Internet_Users, color = new_df$Country))
ggplot(data = new_df) +
  geom_line(mapping = aes(x=new_df$Year, y=new_df$Corruption_Control, color = new_df$Country))
ggplot(data = new_df) +
  geom_line(mapping = aes(x=new_df$Year, y=new_df$Government_Effectiveness, color = new_df$Country))
ggplot(data = new_df) +
  geom_line(mapping = aes(x=new_df$Year, y=new_df$Political_Stability, color = new_df$Country))
ggplot(data = new_df) +
  geom_line(mapping = aes(x=new_df$Year, y=new_df$Regulatory_Quality, color = new_df$Country))
ggplot(data = new_df) +
  geom_line(mapping = aes(x=new_df$Year, y=new_df$Rule_of_Law, color = new_df$Country))
ggplot(data = new_df) +
  geom_line(mapping = aes(x=new_df$Year, y=new_df$Social_Contribution, color = new_df$Country))

#Renaming column names of new data frame
names(new_df)[names(new_df) == 'X1'] <- 'Country'
names(new_df)[names(new_df) == 'X2'] <- 'Year'
names(new_df)[names(new_df) == 'X3'] <- 'Internet_Users'
names(new_df)[names(new_df) == 'X4'] <- 'Corruption_Control'
names(new_df)[names(new_df) == 'X5'] <- 'Government_Effectiveness'
names(new_df)[names(new_df) == 'X6'] <- 'Political_Stability'
names(new_df)[names(new_df) == 'X7'] <- 'Regulatory_Quality'
names(new_df)[names(new_df) == 'X8'] <- 'Rule_of_Law'
names(new_df)[names(new_df) == 'X9'] <- 'Social_Contribution'

unique_country <- function(newdf, unique_name){
  country_df <- data.frame(matrix(NA, nrow = 1, ncol = 9))
  cntr <- 0
  for (k in 1:nrow(newdf)){
    if(!is.na(str_match(newdf$Country[k], unique_name)))
    cntr <- cntr + 1
    country_df[cntr, ] <- newdf[k,]
  }
  #Renaming column names of country_df data frame
  names(country_df)[names(country_df) == 'X1'] <- 'Country'
  names(country_df)[names(country_df) == 'X2'] <- 'Year'
  names(country_df)[names(country_df) == 'X3'] <- 'Internet_Users'
  names(country_df)[names(country_df) == 'X4'] <- 'Corruption_Control'
  names(country_df)[names(country_df) == 'X5'] <- 'Government_Effectiveness'
  names(country_df)[names(country_df) == 'X6'] <- 'Political_Stability'
  names(country_df)[names(country_df) == 'X7'] <- 'Regulatory_Quality'
  names(country_df)[names(country_df) == 'X8'] <- 'Rule_of_Law'
  names(country_df)[names(country_df) == 'X9'] <- 'Social_Contribution'
  return(country_df)
}

#Running above function to create data frames of each individual country
armenia_df <- unique_country(new_df, 'Armenia')
armenia_df <- armenia_df[-c(10),]
azerbaijan_df <- unique_country(new_df, 'Azerbaijan')
azerbaijan_df <- azerbaijan_df[-c(10),]
barbados_df <- unique_country(new_df, 'Barbados')
barbados_df <- barbados_df[-c(7),]
bhutan_df <- unique_country(new_df, 'Bhutan')
bhutan_df <- bhutan_df[-c(9),]
chile_df <- unique_country(new_df, 'Chile')
chile_df <- chile_df[-c(9),]
colombia_df <- unique_country(new_df, 'Colombia')
colombia_df <- colombia_df[-c(10),]
dominican_republic_df<- unique_country(new_df, 'Dominican Republic')
dominican_republic_df <- dominican_republic_df[-c(9),]
gabon_df <- unique_country(new_df, 'Gabon')
gabon_df <- gabon_df[-c(8),]
guatemala_df <- unique_country(new_df, 'Guatemala')
guatemala_df <- guatemala_df[-c(9),]
iraq_df <- unique_country(new_df, 'Iraq')
iraq_df <- iraq_df[-c(6),]
jordan_df <- unique_country(new_df, 'Jordan')
jordan_df <- jordan_df[-c(10),]
kazakhstan_df <- unique_country(new_df, 'Kazakhstan')
kazakhstan_df <- kazakhstan_df[-c(10),]
lebanon_df <- unique_country(new_df, 'Lebanon')
lebanon_df <- lebanon_df[-c(10),]
macao_df <- unique_country(new_df, 'Macao SAR, China')
macao_df <- macao_df[-c(10),]
mauritius_df <- unique_country(new_df, 'Mauritius')
mauritius_df <- mauritius_df[-c(10),]
mexico_df <- unique_country(new_df, 'Mexico')
mexico_df <- mexico_df[-c(9),]
peru_df <- unique_country(new_df, 'Peru')
peru_df <- peru_df[-c(10),]
thailand_df <- unique_country(new_df, 'Thailand')
thailand_df <- thailand_df[-c(10),]
trinidad_df <- unique_country(new_df, 'Trinidad and Tobago')
trinidad_df <- trinidad_df[-c(9),]
south_africa_df <- unique_country(new_df, 'South Africa')
south_africa_df <- south_africa_df[-c(10),]

#Converting into time series
armenia_df <- ts(armenia_df, start = 2010, end = 2018, frequency = 1)
azerbaijan_df <- ts(azerbaijan_df,start = 2010, end = 2018,  frequency = 1)
barbados_df <- ts(barbados_df,start = 2010, end = 2018,  frequency = 1)
bhutan_df <- ts(bhutan_df, start = 2010, end = 2018,  frequency = 1)
chile_df <- ts(chile_df,start = 2010, end = 2018,  frequency = 1)
colombia_df <- ts(colombia_df,start = 2010, end = 2018,  frequency = 1)
dominican_republic_df <- ts(dominican_republic_df, start = 2010, end = 2018, frequency = 1)
gabon_df <- ts(gabon_df,start = 2010, end = 2018,  frequency = 1)
guatemala_df <- ts(guatemala_df,start = 2010, end = 2018, frequency = 1)
iraq_df <- ts(iraq_df, start = 2010, end = 2018,  frequency = 1)
jordan_df <- ts(jordan_df, start = 2010, end = 2018, frequency = 1)
kazakhstan_df <- ts(kazakhstan_df,start = 2010, end = 2018,  frequency = 1)
lebanon_df <- ts(lebanon_df,start = 2010, end = 2018,  frequency = 1)
macao_df <- ts(macao_df,start = 2010, end = 2018,  frequency = 1)
mauritius_df <- ts(mauritius_df,start = 2010, end = 2018,  frequency = 1)
mexico_df <- ts(mexico_df,start = 2010, end = 2018,  frequency = 1)
peru_df <- ts(peru_df, start = 2010, end = 2018, frequency = 1)
thailand_df <- ts(thailand_df,start = 2010, end = 2018,  frequency = 1)
trinidad_df <- ts(trinidad_df,start = 2010, end = 2018,  frequency = 1)
south_africa_df <- ts(south_africa_df, start = 2010, end = 2018, frequency = 1)

#Differences
armenia_df <- diff(armenia_df)
azerbaijan_df <- diff(azerbaijan_df)
barbados_df <- diff(barbados_df)
bhutan_df <- diff(bhutan_df)
chile_df <- diff(chile_df)
colombia_df <- diff(colombia_df)
dominican_republic_df <- diff(dominican_republic_df)
gabon_df <- diff(gabon_df)
guatemala_df <- diff(guatemala_df)
iraq_df <- diff(iraq_df)
jordan_df <- diff(jordan_df)
kazakhstan_df <- diff(kazakhstan_df)
lebanon_df <- diff(lebanon_df)
macao_df <- diff(macao_df)
mauritius_df <- diff(mauritius_df)
mexico_df <- diff(mexico_df)
peru_df <- diff(peru_df)
thailand_df <- diff(thailand_df)
trinidad_df <- diff(trinidad_df)
south_africa_df <- diff(south_africa_df)

#ARIMA model, prediction based on corruption control. Forecast of next 5 years
library(forecast)

armenia <- Arima(armenia_df[,4], order=c(3,1,1))
model_armenia <- forecast(armenia, h=5)
autoplot(model_armenia)
checkresiduals(model_armenia)

azerbaijan <- Arima(azerbaijan_df[,4], order=c(3,1,1))
model_azerbaijan <- forecast(azerbaijan, h=5)
autoplot(model_azerbaijan)
checkresiduals(model_azerbaijan)

barbados <- Arima(barbados_df[,4], order=c(3,1,1))
model_barbados <- forecast(barbados, h=5)
autoplot(model_barbados)
checkresiduals(model_barbados)

bhutan<- Arima(bhutan_df[,4], order=c(3,1,1))
model_bhutan <- forecast(bhutan, h=5)
autoplot(model_bhutan)
checkresiduals(model_bhutan)

chile <- Arima(chile_df[,4], order=c(3,1,1))
model_chile <- forecast(chile, h=5)
autoplot(model_chile)
checkresiduals(model_chile)

colombia <- Arima(colombia_df [,4], order=c(3,1,1))
model_colombia <- forecast(colombia, h=5)
autoplot(model_colombia)
checkresiduals(model_colombia)

dominican_republic <- Arima(dominican_republic_df[,4], order=c(3,1,1))
model_dominican_republic <- forecast(dominican_republic, h=5)
autoplot(model_dominican_republic)
checkresiduals(model_dominican_republic)

gabon <- Arima(gabon_df[,4], order=c(3,1,1))
model_gabon <- forecast(gabon, h=5)
autoplot(model_gabon)
checkresiduals(model_gabon)

guatemala <- Arima(guatemala_df[,4], order=c(3,1,1))
model_guatemala <- forecast(guatemala, h=5)
autoplot(model_guatemala)
checkresiduals(model_guatemala)

iraq <- Arima(iraq_df[,4], order=c(3,1,1))
model_iraq<- forecast(iraq, h=5)
autoplot(model_iraq)
checkresiduals(model_iraq)

jordan<- Arima(jordan_df[,4], order=c(3,1,1))
model_jordan <- forecast(jordan, h=5)
autoplot(model_jordan)
checkresiduals(model_jordan)

kazakhstan <- Arima(kazakhstan_df[,4], order=c(3,1,1))
model_kazakhstan<- forecast(kazakhstan, h=5)
autoplot(model_kazakhstan)
checkresiduals(model_kazakhstan)

lebanon <- Arima(lebanon_df[,4], order=c(3,1,1))
model_lebanon <- forecast(lebanon, h=5)
autoplot(model_lebanon)
checkresiduals(model_lebanon)

macao <- Arima(macao_df[,4], order=c(3,1,1))
model_macao<- forecast(macao, h=5)
autoplot(model_macao)
checkresiduals(model_macao)

mauritius <- Arima(mauritius_df[,4], order=c(3,1,1))
model_mauritius <- forecast(mauritius, h=5)
autoplot(model_mauritius)
checkresiduals(model_mauritius)

mexico <- Arima(mexico_df[,4], order=c(3,1,1))
model_mexico <- forecast(mexico, h=5)
autoplot(model_mexico)
checkresiduals(model_mexico)

peru <- Arima(peru_df[,4], order=c(3,1,1))
model_peru<- forecast(peru, h=5)
autoplot(model_peru)
checkresiduals(model_peru)

thailand <- Arima(thailand_df[,4], order=c(3,1,1))
model_thailand <- forecast(thailand, h=5)
autoplot(model_thailand)
checkresiduals(model_thailand)

trinidad <- Arima(trinidad_df[,4], order=c(3,1,1))
model_trinidad <- forecast(trinidad, h=5)
autoplot(model_trinidad)
checkresiduals(model_trinidad)

south_africa <- Arima(south_africa_df[,4], order=c(3,1,1))
model_south_africa <- forecast(south_africa, h=5)
autoplot(model_south_africa)
checkresiduals(model_south_africa)

library(ggpubr)
ggarrange(autoplot(model_armenia), autoplot(model_azerbaijan), autoplot(model_barbados),
          autoplot(model_bhutan), autoplot(model_chile), autoplot(model_colombia), 
          autoplot(model_dominican_republic), autoplot(model_gabon), ncol = 4, nrow = 2)

ggarrange(autoplot(model_guatemala), autoplot(model_iraq), autoplot(model_jordan),
          autoplot(model_kazakhstan), autoplot(model_lebanon), autoplot(model_macao), 
          autoplot(model_mauritius), autoplot(model_mexico), ncol = 4, nrow = 2)

ggarrange(autoplot(model_peru), autoplot(model_south_africa), autoplot(model_thailand),
          autoplot(model_trinidad), ncol = 2, nrow = 2)

#Means of each country
forecast_armenia <- c(1, 2, 3)
mean_armenia <- mean(forecast_armenia)

mean_array <- c(mean_armenia)