library(tidyverse)
library(lubridate)
library(ggpubr)

data <- as_tibble(
  read.csv(
    file = 'dai_events_may2021to2022.csv',
    header = TRUE,
    colClasses = c("Date", "character", "character", "character",
                  "character", "numeric", "character")
  )
) %>% mutate(value =
  ifelse (percision == 'wad', value/10e18,
    ifelse (percision == 'ray', value/10e27,
      ifelse (percision == 'rad', value/10e45, NaN)
    )
  )
) %>% select(-percision)

#summarize count and sum of value by day and rewrite data object to save space

data <- data %>%
  filter(makerdao_smartcontract == 'DAI' & makerdao_function == 'transfer') %>%
  select(time, sender, reciever, value) %>%
  group_by(time, sender, reciever) %>%
  summarise(volume = n(), value = sum(value))

data

#determine and graph value transferred per month in DAI

monthlytransfer_data <- data %>%
  ungroup() %>%
  select(time, volume, value) %>%
  group_by(time = floor_date(time, "month")) %>%
  summarise(value = sum(value) / 1000000000, volume = sum(volume) / 1000) %>%
  rename(value_in_billions = value, volume_in_thousands = volume)

#remove first row as first month is not complete
monthlytransfer_data <- monthlytransfer_data[-1,]

monthlytransfer_data

#graph data

#plot value over time
value_over_time <- ggplot(monthlytransfer_data, aes(x = time, y = value_in_billions)) +
  geom_bar(stat = "identity")

value_over_time

#plot volume over time
volume_over_time <- ggplot(monthlytransfer_data, aes(x = time, y = volume_in_thousands)) +
  geom_bar(stat = "identity")

volume_over_time

#bind graphs together
volume_value_over_time <- ggarrange(value_over_time, volume_over_time,
                                    labels = c("value over time", "volume over time"))

volume_value_over_time

#count unique senders and receivers per month

unique_senders_receivers <- data %>%
  ungroup() %>%
  select(time, sender, reciever) %>%
  arrange(time) %>%
  filter(time >= "2021-06-01") %>%
  mutate(sender_cumsum = cumsum(!duplicated(sender)),
         receiver_cumsum = cumsum(!duplicated(reciever))) %>%
  group_by(time = floor_date(time, "month")) %>%
  summarise(sender_cumsum = last(sender_cumsum), receiver_cumsum = last(receiver_cumsum),
            sender = n_distinct(sender), reciever = n_distinct(reciever)) %>%
  mutate(sender_percent_diff = (sender - lag(sender)) / lag(sender) * 100,
         receiver_percent_diff = (reciever - lag(reciever)) / lag(reciever) * 100,
         sender_rate_change = (sender_cumsum - lag(sender_cumsum)) / lag(sender_cumsum) * 100,
         receiver_rate_change = (receiver_cumsum - lag(receiver_cumsum)) / lag(receiver_cumsum) * 100) %>%
  rename(unique_senders = sender, unique_receivers = reciever, month = time)

#remove first row as month is not complete
unique_senders_receivers

unique_senders_receivers

#plot

senders_per_month <- ggplot(unique_senders_receivers, aes(x = month, y = unique_senders)) +
  geom_bar(stat = "identity")

senders_per_month

receivers_per_month <- ggplot(unique_senders_receivers, aes(x = month, y = unique_receivers)) +
  geom_bar(stat = "identity")

receivers_per_month

sender_pd_over_time <- ggplot(unique_senders_receivers, aes(x = month, y = sender_percent_diff)) +
  geom_line() +
  geom_hline(yintercept = 0)

sender_pd_over_time

receiver_pd_over_time <- ggplot(unique_senders_receivers, aes(x = month, y = receiver_percent_diff)) +
  geom_line() +
  geom_hline(yintercept = 0)

receiver_pd_over_time

senders_over_time <- ggplot(unique_senders_receivers, aes(x = month, y = sender_cumsum)) +
  geom_line()

senders_over_time

receiver_over_time <- ggplot(unique_senders_receivers, aes(x = month, y = receiver_cumsum)) +
  geom_line()

receiver_over_time

sender_rc_over_time <- ggplot(unique_senders_receivers, aes(x = month, y = sender_rate_change)) +
  geom_line()

sender_rc_over_time

receiver_rc_over_time <- ggplot(unique_senders_receivers, aes(x = month, y = receiver_rate_change)) +
  geom_line()

receiver_rc_over_time

#bind relevant graphs together

senders_permonth_ratechange <- ggarrange(senders_per_month, sender_pd_over_time,
                                         sender_over_time, sender_rc_over_time,
                                         labels = c("senders_per_month",
                                                    "sender_pd_over_time",
                                                    "senders_over_time",
                                                    "sender_rc_over_time"),
                                         ncol = 2, nrow = 2)

senders_permonth_ratechange

#relevant graphs:

volume_value_over_time
senders_permonth_ratechange
