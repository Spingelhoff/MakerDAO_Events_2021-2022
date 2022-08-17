library(tidyverse)

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