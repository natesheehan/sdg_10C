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

if(file.exists("data/countries.RDS")){
  countries = readRDS("data/countries.RDS")
} else {
  countries = countries %>%
    geocode(country,
            method = 'osm', # calculate osm country point
            lat = latitude ,
            long = longitude)

  saveRDS(countries, "data/countries.RDS")
}

countries = st_as_sf(countries, coords = c("longitude","latitude"), crs = "WGS84")
#Set DF to SF
countries = st_as_sf(countries)
st_crs(countries) = "WGS84"

upperQ_sf = upperQ %>% select(Sending.Countries.Name,Receiving.Countries..Name,value)
desire_lines_od = od_to_sf(upperQ_sf, countries)
mapview(desire_lines_od)
