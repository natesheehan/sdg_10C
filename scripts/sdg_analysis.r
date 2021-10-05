####
####
# Aim: Run analysis --------------------
####
####
ggplot(data = data, aes(x = year, y = value)) +
  geom_point(color = "darkorchid4") +
  labs(
    title = "Global remitance distribution",
    subtitle = "Note using pipes",
    y = "Average (%) of remittance payed through corridor",
    x = "Year"
  ) + theme_bw(base_size = 15)

year_av = aggregate(value ~ year, data, mean)

ggplot(data = data, aes(x = year, y = value,  group = 1)) +
  geom_point() +
  geom_smooth() +
  annotate("text",
           x = 10,
           y = 3,
           label = "------- 3%") +
  labs(
    x = "Year",
    y = "Percent value of trading $200 (or equivelent in local currency)",
    title = "Global Remitance Corridors Tax Averages",
    subtitle = "Data from the World bank records X countries in its World Wide Remittance Database"
  ) +
  theme_modern_rc()



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

countries = rbind(countries_rec, countries_send)
countries = as.data.frame(unique(countries$country))
colnames(countries) = "country"

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
  add_arc(
    data = upperQ_sf
    ,
    layer_id = "arc_layer"
    ,
    origin = c("long_send", "lat_send")
    ,
    destination = c("long_rec", "lat_rec")
    ,
    stroke_from = "Sending.Countries.Name"
    ,
    stroke_to = "Receiving.Countries..Name"
    ,
    stroke_width = "value"
    ,
    legend = TRUE
    ,
    legend_options = list(
      stroke_width = list(title = "Value", digits = 3)
      ,
      css = "max-height: 100px;" ## css applied to both stroke_from and stroke_to
    )
  )
