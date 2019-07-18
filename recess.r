
install.packages("dplyr")
install.packages("prophet")
install.packages("xts")
install.packages("highcharter")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("ggvis")
install.packages("tourr")
install.packages("shiny")
install.packages("readr")
library(dplyr)
library(prophet)
library(xts)
library(highcharter)

kampalacrimes20122017 <- read.csv("G:/Recess/Data sets/Kampala_Crimes_2012_to_2017.csv")

kampalacrimes20122015 <- crimes20122017[kampalacrimes20122017$Year %in% c('2012', '2013', '2014', '2015', '2016'), c('Date', 'ID')]
#head(kampalacrimes20122017$Region)
#barplot(kampalacrimes20122017$Year)
## Creating timeseries
kampalacrimes20122015$Date <- as.Date(kampalacrimes20122015$Date, "%m/%d/%Y")
by_Date <- na.omit(kampalacrimes20122015) %>% group_by(Date) %>% summarise(Total = n())
tseries <- xts(by_Date$Total, order.by=as.POSIXct(by_Date$Date))
#kampalacrimes20122015$Date
head(kampalacrimes20122017$Date)
head(kampalacrimes20122015$Date)
kampalacrimes20122015$Date
kampalacrimes20122017$Date
#Sys.Date()

df <- kampalacrimes20122017 %>% group_by(Date) %>% summarise(y = n()) %>% mutate(y = log(y))
#df
names(df) <- c("ds", "y")
df$ds <- factor(df$ds)

head(chi)
### Times Series plot of kampala Crimes 2012-2016

hchart(tseries, name = "Crimes") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_credits(enabled = TRUE, text = "Sources: City of kampala Administration and the kampala Police Department", style = list(fontSize = "12px")) %>%
  hc_title(text = "Times Series plot of kampala Crimes") %>%
  hc_legend(enabled = TRUE)

### Overview of Prophet

# Prophet is a procedure for forecasting time series data. It is based on an additive model where 
#non-linear trends are fit with yearly and weekly seasonality, plus holidays. It works best with 
#daily periodicity data with at least one year of historical data. Prophet is robust to missing 
#data, shifts in the trend, and large outliers.

# Prophet is open source software released by Facebook's Core Data Science team.

### Fitting the model

m <- prophet(df)

### Creating dataframe with historical dates and future dates to forecast

future <- make_future_dataframe(m, periods = 365 * 6)

head(future)

tail(future)

### Forecasting by using predict method

forecast <- predict(m, future)

tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

### How to evaluate a forecasting model?

# Once a model has been selected and its parameters estimated, the model is used to 
#make forecasts. The performance of the model can only be properly evaluated after the 
#data for the forecast period have become available. A number of methods have been developed 
#to help in assessing the accuracy of forecasts.  </p>

### Basic plotting of the forecast

plot(m, forecast)

### Plotting forecast broken down into trend, weekly and yearly

prophet_plot_components(m, forecast)

