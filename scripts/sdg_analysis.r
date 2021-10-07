####
####
# Aim: Run analysis --------------------
####
####

#### As ranked by the GermanWatch 2011-2019 climate vulnerable countries index
climate_vulnerable_countries = c(
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


climate_vulnerable_countries_19 = c(
  "Mozambique",
  "Zimbabwe",
  "Bahamas",
  "Japan",
  "Malawi",
  "Afganistan",
  "India",
  "South Sudan",
  "Niger",
  "Boliva"
)


year_av = aggregate(value ~ year, data, mean)

# calculate countries
countries_rec = as.data.frame(data$Receiving.Countries..Name)
colnames(countries_rec) = "country"

countries_send = as.data.frame(data$Sending.Countries.Name)
colnames(countries_send) = "country"

countries = rbind(countries_rec, countries_send) %>%
  count(country) %>%
  arrange(desc(n)) %>%
  geocode(country,
          method = 'osm',
          # calculate osm country point
          lat = latitude ,
          long = longitude)

#Set DF to SF
countries = st_as_sf(countries,
                     coords = c("longitude", "latitude"),
                     crs = "+proj=robin")

qtm(countries)


library(hrbrthemes)

ggplot(data = na.omit(data), aes(x = year, y = value,  group = 1)) +
  geom_point() +
  geom_smooth() +
  annotate("text",
           x = 10,
           y = 3,
           label = "------- 3%") +
  labs(
    x = "Year",
    y = "Remitance percent value of send $200 (or equivelent in local currency)",
    title = "Global Remitance Corridors Tax Averages",
    subtitle = "Data from the World bank records X countries in its World Wide Remittance Database"
  ) + theme_modern_rc()



post_sdg = data %>% filter(year >= 2016)
hist(post_sdg$value)
summary(post_sdg$value)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.075   4.808   6.626   7.382   9.175  26.971
# frequency tables total
send_sdg =  as.data.frame(table(post_sdg$Sending.Countries.Name)) %>% arrange(desc(Freq))
rec_sdg =  as.data.frame(table(post_sdg$Receiving.Countries..Name)) %>% arrange(desc(Freq))

upperQ = post_sdg %>% filter(value >= 9.175) %>% filter(year == 2020) %>% arrange((value))
lowerQ = post_sdg %>% filter(value <= 4.808) %>% filter(year == 2020) %>% arrange((value))


post_sdg = data %>% filter(year >= 2016) %>% filter(sdg_target == 0)
summary(post_sdg$value)

# UpperQ


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

# transform data for OD calculation
upperQ_sf = upperQ %>% select(Sending.Countries.Name, Receiving.Countries..Name, value)
desire_lines_od = od_to_sf(upperQ_sf, countries)

library(mapview)
#View OD connection
mapview(desire_lines_od)

library(tmap)
tmap_mode("plot")
tm_basemap(leaflet::providers$Stamen.TonerLite) +
  tm_shape(desire_lines_od) +
  tm_lines(
    palette = "plasma",
    breaks = c(10, 12, 14, 16, 18, 20),
    lwd = "value",
    scale = 9,
    title.lwd = "%",
    alpha = 0.5,
    col = "value",
    title = "Remittance value (%)",
    legend.lwd.show = TRUE
  ) +
  tm_scale_bar() +
  tm_layout(legend.bg.alpha = 0.5,
            legend.bg.color = "white")


library(mapdeck)

key = "pk.eyJ1IjoibmF0aGFuYWVsaXNhbWFwcGVyIiwiYSI6ImNrODNiZzdoZTA4Y2gzZ281YmJiMHNwOWIifQ.d2ntY86sJ7DR7011dUJ2cw"
set_token(key)

upperQ_sf = upperQ %>%
  geocode(
    Sending.Countries.Name,
    method = "osm",
    lat = lat_send,
    long = long_send
  ) %>%
  geocode(
    Receiving.Countries..Name,
    method = "osm",
    lat = lat_rec,
    long = long_rec
  )

mapdeck(token = key,
        style = mapdeck_style("light"),
        pitch = 45) %>%
  add_line(
    data = upperQ_sf
    ,
    layer_id = "arc_layer"
    ,
    origin = c("long_send", "lat_send")
    ,
    destination = c("long_rec", "lat_rec")
    ,
    legend = TRUE
    ,
    legend_options = list(
      stroke_width = list(title = "Value", digits = 3)
      ,
      css = "max-height: 100px;" ## css applied to both stroke_from and stroke_to
    )
  )
