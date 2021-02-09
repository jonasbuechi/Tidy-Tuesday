# info -------------------------------------------------------------------------
# Script inspired by havely David Robinson
# Adapted by Jonas Büchi

# Jan 2021, Week 01
# tidytuesday transit cost


# INITIAL SETTING --------------------------------------------------------------

# load libraries
library(tidyverse)
library(tidytuesdayR)
library(magrittr) # for the assigment pipe
library(countrycode)
library(glue)

# load data --------------------------------------------------------------------
tt <-  tt_load("2021-01-05")
tt
transit_cost <- tt$transit_cost

# alternative loading
#transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')


# DATA WRANGLING ---------------------------------------------------------------

transit_cost <- transit_cost %>%
  filter(!is.na(e)) %>% 
  mutate(across(c(start_year, end_year, real_cost), as.numeric)) %>% 
  mutate(country_code = if_else(country == "UK", "GB", country),
         country = countrycode(country_code, "iso2c", "country.name"),
         tunnel_per = tunnel / length,
         rr = ifelse(rr, "Railroad", "Not Railroad"))



# Inspection of data -----------------------------------------------------------
transit_cost %>%
  filter(country == "United States") %>% 
  mutate(line = fct_reorder(line, year)) %>% 
  ggplot(aes(xmin = start_year,
             xmax = end_year,
             y = line,
             color = city)) +
  geom_errorbarh(height = .1)

# Boxplots by country and cost per KM
transit_cost %>% 
  filter(!is.na(cost_km_millions),
         tunnel_per == 1) %>% 
  mutate(country = fct_lump(country, 12)) %>% 
  add_count(country) %>%
  filter(n > 5) %>% 
  mutate(country = glue("{ country } ({ n })"),
         country = fct_reorder(country, cost_km_millions)) %>%
  ggplot(aes(x = cost_km_millions,
             y = country)) +
  geom_boxplot() +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(x = "Cost / KM (Millions of USD)",
       y = "")


# Boxplots by cities in China and cost per KM
transit_cost %>% 
  filter(!is.na(cost_km_millions),
         tunnel_per == 1,
         country == "China") %>% 
  mutate(city = fct_lump(city, 10)) %>% 
  add_count(city) %>%
  filter(n > 5) %>% 
  mutate(city = glue("{ city } ({ n })"),
         city = fct_reorder(city, cost_km_millions)) %>%
  ggplot(aes(x = cost_km_millions,
             y = city)) +
  geom_boxplot() +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(x = "Cost / KM (Millions of USD)",
       y = "") +
  expand_limits(x = 0)

# Developement of Costs Per Mil in China
transit_cost %>% 
  filter(tunnel_per == 1,
         end_year <= 2020,
         country == "China") %>% 
  mutate(year = (year %/% 5)* 5, # making bins of 5 years
         city = fct_lump(city, 5)) %>% 
  ggplot(aes(x = year,
             y = cost_km_millions,
             group = year)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(aes(color = city), height = 0, width = 1) +
  expand_limits(y = 0)+
  theme_minimal()
