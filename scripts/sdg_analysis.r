####
####
# Aim: Run analysis --------------------
####
####

#### As ranked by the GermanWatch 2011-2019 climate vulnerable countries index
climate_vulnerable_countries = as.data.frame(
  c(
    "Puerto Rico",
    "Myanmar",
    "Haiti",
    "Philippines",
    "Mozambique",
    "Bahamas",
    "Bangladesh",
    "Pakistan",
    "Thailand",
    "Nepal"
  )
)
colnames(climate_vulnerable_countries) = "country"

# 2020 list
climate_vulnerable_countries_19 = as.data.frame(
  c(
    "Mozambique",
    "Zimbabwe",
    "Bahamas",
    "Japan",
    "Malawi",
    "Afghanistan",
    "India",
    "South Sudan",
    "Niger",
    "Bolivia"
  )
)
colnames(climate_vulnerable_countries_19) = "country"

cvc = rbind(climate_vulnerable_countries,climate_vulnerable_countries_19)
cvc = as.data.frame(unique(cvc$country))
colnames(cvc) = "country"

if(file.exists("data/cvc.RDS")){
  cvc = readRDS("data/cvc.RDS")
} else {
  cvc = cvc %>%
    geocode(country,
            method = 'osm', # calculate osm country point
            lat = latitude ,
            long = longitude)
  #write rds for time saving
  saveRDS(cvc,"data/cvc.RDS")
}

# Calculate and visualise year_average ------------------------------------
year_av = aggregate(value ~ year, data, mean)
kableExtra::kable(year_av) %>% kableExtra::kable_material_dark()

# join send/receive countries together and fetch centroid point
countries_rec = as.data.frame(data$Receiving.Countries..Name)
colnames(countries_rec) = "country"
countries_send = as.data.frame(data$Sending.Countries.Name)
colnames(countries_send) = "country"

if(file.exists("data/countries_all.RDS")){
  countries = read_rds("data/countries_all.RDS")
} else{
  countries = rbind(countries_rec, countries_send) %>%
    count(country) %>%
    arrange(desc(n)) %>%
    geocode(country,
            method = 'osm',
            # calculate osm country point
            lat = latitude ,
            long = longitude)

  # set DF to SF
  countries = st_as_sf(countries,
                       coords = c("longitude", "latitude"),
                       crs = "wsg84")

  saveRDS(countries,"data/countries_all.RDS")
}
# set number of countries as the count of rows
n_countries = nrow(countries)


# Plot total data ---------------------------------------------------------
ggplot(data = na.omit(data), aes(x = year, y = value,  group = 1)) +
  geom_point() +
  geom_smooth() +
  annotate("text",
           x = 10,
           y = 3,
           label = "------- 3%") +
  labs(
    x = "Year",
    y = "Remitance percent value of sending $200 (or equivelent in local currency)",
    title = "Global Remitance Corridors Tax Averages",
    subtitle = paste0(
      "Data from the World bank records ",
      n_countries,
      " countries in its World Wide Remittance Database"
    )
  ) + theme_modern_rc()


post_sdg = data %>% filter(year >= 2016)

# frequency tables total
send_sdg =  as.data.frame(table(post_sdg$Sending.Countries.Name)) %>% arrange(desc(Freq))
rec_sdg =  as.data.frame(table(post_sdg$Receiving.Countries..Name)) %>% arrange(desc(Freq))

hist(post_sdg$value, main = "Histogram of Post SDG (2016) Remittance Values", xlab = "value (%)")
summary(post_sdg$value)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.075   4.808   6.626   7.382   9.175  26.971
upperQ = post_sdg %>% filter(value >= 9.175) %>% filter(year == 2020) %>% arrange((value))
upperq_share = upperQ %>% group_by(Receiving.Countries..Name) %>% tally() %>% arrange(desc(n)) %>% rename(country = Receiving.Countries..Name)
right_join(upperq_share,cvc)

lowerQ = post_sdg %>% filter(value <= 4.808) %>% filter(year == 2020) %>% arrange((value))
lowerq_share = lowerQ %>% group_by(Receiving.Countries..Name) %>% tally() %>% arrange(desc(n)) %>% rename(country = Receiving.Countries..Name)
right_join(lowerq_share,cvc)

# post_sdg = data %>% filter(year >= 2016) %>% filter(sdg_target == 0)
# summary(post_sdg$value)

# Visualise network of upper and lower Q ----------------------------------
if (file.exists("data/countries.RDS")) {
  countries = readRDS("data/countries.RDS")
} else {
  countries = countries %>%
    geocode(country,
            method = 'osm',
            # calculate osm country point
            lat = latitude ,
            long = longitude)

  saveRDS(countries, "data/countries.RDS") # save as rds
}

#Set DF to SF
countries = st_as_sf(countries,
                     coords = c("longitude", "latitude"),
                     crs = "WGS84")
countries = st_as_sf(countries)
st_crs(countries) = "WGS84"

mapview::mapview(countries)

# transform data for OD calculation
upperQ_sf = upperQ %>% select(Sending.Countries.Name, Receiving.Countries..Name, value)
upperQ_sf_od = od_to_sf(upperQ_sf, countries)

mapview::mapview(upperQ_sf_od,map.types = c("Esri.WorldShadedRelief", "OpenStreetMap.DE"), zcol = "value", legend = TRUE)

lowerQ_sf = lowerQ %>% select(Sending.Countries.Name, Receiving.Countries..Name, value)
lowerrQ_sf_od = od_to_sf(lowerQ_sf, countries)

mapview::mapview(lowerrQ_sf_od,map.types = c("Esri.WorldShadedRelief", "OpenStreetMap.DE"), zcol = "value", legend = TRUE)
