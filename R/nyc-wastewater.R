library(tidyverse)
library(lubridate)

# Read in the data from the CDC
cdc_wastewater <- read.csv("https://data.cdc.gov/api/views/2ew6-ywp6/rows.csv")

# get NYC data and format it properly
nyc_cdc_wastewater <- cdc_wastewater %>% 
  filter(wwtp_jurisdiction == "New York City") %>% 
  mutate(date_start = as_date(date_start)) %>% 
  mutate(date_end = as_date(date_end)) %>% 
  group_by(wwtp_id) %>% 
  mutate(prev_percentile = lag(percentile)) %>% 
  ungroup() %>% 
  filter(percentile != prev_percentile)

# Plot it
png("NYC_wastewater_rates.png", width = 2000, height = 1000)

ggplot(data = nyc_cdc_wastewater, aes(x = date_end, y = percentile)) +
  geom_point(aes(size = population_served/1000)) +
  # geom_smooth(aes(weight = population_served)) +
  # facet_grid(county_names ~ .) +
  scale_size_area(max_size = 8) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(title = "Weekly COVID wastewater rates for testing sites in NYC", x = "Date", y = "% of peak rate", size = "Population served by\nwastewater site\n(thousands)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 30))

dev.off()



# nyc_wastewater <- read.csv("https://data.cityofnewyork.us/api/views/f7dc-2q9f/rows.csv") %>% 
#   mutate(Sample.Date = mdy(Sample.Date)) %>% 
#   mutate(Test.date = mdy(Test.date))
# 
# 
# # ggplot(data = nyc_wastewater %>% filter(Sample.Date >= date("2022-07-01")), aes(x = Sample.Date, y = Per.capita.SARS.CoV.2.load..N1.copies.per.day.per.population.)) +
# ggplot(data = nyc_wastewater, aes(x = Sample.Date, y = Per.capita.SARS.CoV.2.load..N1.copies.per.day.per.population.)) +
#   geom_point(aes(size = Population.Served..estimated.)) +
#   # geom_smooth(aes(weight = Population.Served..estimated.)) +
#   ylim(0, 30000000) +
#   scale_size_area(max_size = 4)