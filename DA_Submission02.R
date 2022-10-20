library(tidyverse)
library(prophet)
library(forecast)
library(scales)
library(lubridate)
library(readxl)
library(ggplot2)
View(psd)
str(psd)
df <- psd %>%
  group_by(Date) %>%
  summarise(Stock_Demand=sum(Stock_Demand))
ggplot(df, aes(x=Date, y=Stock_Demand)) + geom_bar(stat="identity")
df<- filter(df, Date>="2020-10-01"& Date<"2021-08-01")
df <- df %>%
  mutate(Date=as.Date(Date)) %>%
  complete(Date=seq.Date(min(Date), max(Date), by="day"))
df[is.na(df)] <- 0
df$Date <-floor_date(df$Date, "month")
df <- df %>%
  group_by(month=floor_date(Date, "month")) %>%
  summarise(Stock_Demand=sum(Stock_Demand))
ggplot(df, aes(x=month, y=Stock_Demand))+geom_bar(stat="identity", fill="purple")+labs(title="Monthly Stock Demand",caption = "Pharmaceutical Sales Demand for German company")+ xlab("Date")+ylab("Stock Demand")+ theme_bw()+scale_x_date(date_labels = "%m-%y", date_breaks = "1 month")+ theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1))
myts <- ts(data=df$Stock_Demand, start=c(2020, 10), end=c(2021,8), frequency=12)
plot(myts)
decompose_df <-tslm(myts ~ trend + fourier(myts, 2))
trend <- coef(decompose_df)[1] + coef(decompose_df)['trend']*seq_along(myts)
StockDemand <- cbind(data=myts,trend=trend, season= myts-trend-residuals(decompose_df), remainder=residuals(decompose_df))
autoplot(StockDemand, facet=TRUE)
adjust_df <- myts-StockDemand[, 'season']
autoplot(myts, series="Data")+ autolayer(adjust_df, series="Seasonally adjusted")
forecast(decompose_df, newdata=fourier(myts, 2, 10))


