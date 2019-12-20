library(prophet)
library(ggplot2)
df <- read.csv("02_initialData/example_wp_log_peyton_manning.csv")
m <- prophet(df, daily.seasonality = TRUE)

future <- make_future_dataframe(m, periods = 365)
tail(future)

# R
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)

ggObj <- prophet_plot_components(m, forecast)

ggsave(plot = ggObj, filename = "ts_components.pdf")
plot(forecast$weekly)
