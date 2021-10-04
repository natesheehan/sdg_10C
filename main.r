# Librarys ----------------------------------------------------------------
library(stringr)
library(tidyverse)
library(tidygeocoder)
library(sf)
library(od)

# Read world bank remitance corridor data ----------------------------------------------------
#### notes: this data is not avaliable from the WorldBank R oackage WDI
tags = c(" 0-3%", " 3-5%", " 5-10%", "10% and above") # create tags for bin labels
#### the data has been downloaded from: https://databank.worldbank.org/source/remittance-prices-worldwide-(corridors)
data_corridors = read.csv("data/36d1ed87-9a2d-41da-8eac-c1c25d45fa65_Data.csv")
colnames(data_corridors)[1] = "Sending.Countries.Name" # rename column because its called some funky shit

data_corridors[data_corridors == ".."] = NA

data_corridors[, 7:41] = sapply(data_corridors[, 7:41], as.numeric)
data = data_corridors %>% filter(X2020Q3..YR2020Q3. != "..") %>%
  mutate('2011' = (X2011Q1..YR2011Q1. + X2011Q3..YR2011Q3.) / 2) %>%
  mutate('2012' = (X2012Q1..YR2012Q1. + X2012Q3..YR2012Q3.) / 2) %>%
  mutate('2013' = (X2013Q1..YR2013Q1. + X2013Q1..YR2013Q1.) / 2) %>%
  mutate('2014' = (X2014Q1..YR2014Q1. + X2014Q3..YR2014Q3.) / 2) %>%
  mutate('2015' = (X2015Q1..YR2015Q1. + X2015Q3..YR2015Q3.) / 2) %>%
  mutate('2016' = (X2016Q1..YR2016Q1. + X2016Q3..YR2016Q3.) / 2) %>%
  mutate('2017' = (X2017Q1..YR2017Q1. + X2017Q3..YR2017Q3.) / 2) %>%
  mutate('2018' = (X2018Q1..YR2018Q1. + X2018Q3..YR2018Q3.) / 2) %>%
  mutate('2019' = (X2019Q1..YR2019Q1. + X2019Q3..YR2019Q3.) / 2) %>%
  mutate('2020' = (X2020Q1..YR2020Q1. + X2020Q3..YR2020Q3.) / 2) %>%
  select(
    Sending.Countries.Name,
    Sending.Countries.Code,
    Receiving.Countries..Name,
    Receiving.Countries..Code,
    '2011',
    '2012',
    '2013',
    '2014',
    '2015',
    '2016',
    '2017',
    '2018',
    '2019',
    '2020'
  ) %>% gather(year, value, (5:14)) %>%
  na.omit() %>%
  mutate(sdg_target = as.numeric(value < 5))  %>%
  mutate(Sending.Countries.Name = str_replace(Sending.Countries.Name, "United States of America", "United States"))  %>%
  mutate(Sending.Countries.Name = str_replace(Sending.Countries.Name, " Korea, Rep.", "South Korea"))

##### View total SDG Targets
table(data$sdg_target) # 0 = no 1 = yes

post_sdg = data %>% filter(year >= 2016) %>%
  filter(sdg_target == 0) ### SDG false

# frequency tables
send_sdg =  as.data.frame(table(post_sdg$Sending.Countries.Name)) %>% arrange(desc(Freq))
rec_sdg =  as.data.frame(table(post_sdg$Receiving.Countries..Name)) %>% arrange(desc(Freq))

summary(post_sdg$value)

# UpperQ
upperQ = post_sdg %>% filter(value >= 10.198) %>% filter(year == 2020)

# calculate countries
countries_rec = as.data.frame(upperQ$Receiving.Countries..Name)
colnames(countries_rec) = "country"

countries_send = as.data.frame(upperQ$Sending.Countries.Name)
colnames(countries_send) = "country"

countries = rbind(countries_rec,countries_send)
countries = as.data.frame(unique(countries$country))
colnames(countries) = "country"

countries = countries %>%
  geocode(country,
          method = 'osm', # calculate osm country point
          lat = latitude ,
          long = longitude)

saveRDS(countries, "data/countries.RDS")

countries = st_as_sf(countries, coords = c("longitude","latitude"), crs = "WGS84")
#Set DF to SF
countries = st_as_sf(countries)
st_crs(countries) = "WGS84"

upperQ_sf = upperQ %>% select(Sending.Countries.Name,Receiving.Countries..Name,value)
desire_lines_od = od_to_sf(upperQ_sf, countries)
mapview(desire_lines_od)


