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


# FUNCTIONS ---------------------------------------------------------------

plot_rec_country = function(country) {
  country_rec_data = data %>%
    filter(Receiving.Countries..Name == country)  %>%
    rename(send = Sending.Countries.Name) %>%
    rename(recieve = Receiving.Countries..Name) %>%
    mutate(value = ifelse(value < 0, NA, value)) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(
      bins = case_when(
        value < 3 ~ tags[1],
        value >= 3 & value < 5 ~ tags[2],
        value >= 5 & value < 10 ~ tags[3],
        value >= 10 ~ tags[4]
      )
    )

  receive_levels = country_rec_data %>%
    filter(complete.cases(.)) %>%
    group_by(recieve) %>%
    summarize(num = n()) %>%
    arrange(-num) %>%
    pull(recieve)

  send_levels = country_rec_data %>%
    filter(complete.cases(.)) %>%
    group_by(send) %>%
    summarize(num = n()) %>%
    arrange(-num) %>%
    pull(send)

  # mutate levels into dataframe
  df = country_rec_data %>%
    mutate(recieve = factor(recieve, levels = receive_levels),
           send = fct_rev(factor(send, levels = send_levels))) %>%
    filter(complete.cases(send))

  # text col
  textcol = "grey40"

  # Plot --------------------------------------------------------------------
  p = ggplot(df, aes(x = send, y = recieve, fill = bins)) +
    facet_wrap( ~ year, ncol = 1) +
    geom_tile(colour = "black", size = 0.2) +
    geom_text(aes(label = round(value, digits = 2))) +
    scale_colour_manual(values = c("red", "blue", "green")) +
    guides(fill = guide_legend(title = "Average cost of sending remittances between countries")) +
    labs(
      x = "Countries Where Remittance is Sent",
      y = "Countries Where Remittance is Received",
      title = paste0(
        "World Bank Remittance Corridors Data\nCountry Receiving: ",
        country
      ),
      caption = "Remittance costs vary between sending and receiving country corridors.The SDG target aims to bring all corridor costs to below 5% of the amount remitted."
    ) +
    #coord_fixed()+
    theme_classic(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(colour = textcol),
      legend.margin = margin(grid::unit(0, "cm")),
      legend.text = element_text(
        colour = textcol,
        size = 7,
        face = "bold"
      ),
      legend.key.height = grid::unit(0.4, "cm"),
      legend.key.width = grid::unit(0.8, "cm"),
      axis.text.x = element_text(size = 10, colour = textcol),
      axis.text.y = element_text(vjust = 0.2, colour = textcol),
      axis.ticks = element_line(size = 0.4),
      plot.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = "black"),
      plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
      plot.title = element_text(
        colour = textcol,
        family = "sans",
        hjust = 0,
        size = 14,
        face = "bold"
      )
    )

  p

} # PLOT 10 YEAR HISTOIRC

plot_rec_country(country = "Vanuatu")


plot_send_country = function(country) {
  country_rec_data = data %>%
    filter(Sending.Countries.Name == country)  %>%
    rename(send = Sending.Countries.Name) %>%
    rename(recieve = Receiving.Countries..Name) %>%
    mutate(send = str_replace(send, "United States of America", "United States"))  %>%
    mutate(value = ifelse(value < 0, NA, value)) %>%
    mutate(value = as.numeric(value)) %>%
    mutate(
      bins = case_when(
        value < 3 ~ tags[1],
        value >= 3 & value < 5 ~ tags[2],
        value >= 5 & value < 10 ~ tags[3],
        value >= 10 ~ tags[4]
      )
    )

  receive_levels = country_rec_data %>%
    filter(complete.cases(.)) %>%
    group_by(recieve) %>%
    summarize(num = n()) %>%
    arrange(-num) %>%
    pull(recieve)

  send_levels = country_rec_data %>%
    filter(complete.cases(.)) %>%
    group_by(send) %>%
    summarize(num = n()) %>%
    arrange(-num) %>%
    pull(send)

  # mutate levels into dataframe
  df = country_rec_data %>%
    mutate(recieve = factor(recieve, levels = receive_levels),
           send = fct_rev(factor(send, levels = send_levels))) %>%
    filter(complete.cases(send))

  # text col
  textcol = "grey40"

  # Plot --------------------------------------------------------------------
  p = ggplot(df, aes(x = recieve, y = send, fill = bins)) +
    facet_wrap( ~ year, ncol = 1) +
    geom_tile(colour = "black", size = 0.2) +
    geom_text(aes(label = round(value, digits = 2))) +
    scale_colour_manual(values = c("red", "blue", "green")) +
    guides(fill = guide_legend(title = "Average cost of sending remittances between countries")) +
    labs(
      x = "Countries Where Remittance is Received",
      y = "Countries Where Remittance is Sent",
      title = paste0(
        "World Bank Remittance Corridors Data\nCountry Sending: ",
        country
      ),
      caption = "Remittance costs vary between sending and receiving country corridors.The SDG target aims to bring all corridor costs to below 5% of the amount remitted."
    ) +
    #coord_fixed()+
    theme_classic(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(colour = textcol),
      legend.margin = margin(grid::unit(0, "cm")),
      legend.text = element_text(
        colour = textcol,
        size = 7,
        face = "bold"
      ),
      legend.key.height = grid::unit(0.4, "cm"),
      legend.key.width = grid::unit(0.8, "cm"),
      axis.text.x = element_text(size = 10, colour = textcol),
      axis.text.y = element_text(vjust = 0.2, colour = textcol),
      axis.ticks = element_line(size = 0.4),
      plot.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = "black"),
      plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
      plot.title = element_text(
        colour = textcol,
        family = "sans",
        hjust = 0,
        size = 14,
        face = "bold"
      )
    )

  p

} # PLOT 10 YEAR HISTOIRC
plot_send_country(country = "China")

# rec_countries = as.data.frame(table(data$Receiving.Countries..Name)) %>% arrange(desc(Freq))
# sending_countries = as.data.frame(table(data$Sending.Countries.Name)) %>% arrange(desc(Freq))
#
#
#
# # Tidy DF for GGplot // using year 2020 q3------------------------------------------------------
# data_corridors_min = data_corridors %>%
#   select(Sending.Countries.Name,
#          Receiving.Countries..Name,
#          X2020Q3..YR2020Q3.) %>%
#   filter(X2020Q3..YR2020Q3. != "..") %>%
#   rename(value = X2020Q3..YR2020Q3.) %>%
#   rename(send = Sending.Countries.Name) %>%
#   rename(recieve = Receiving.Countries..Name) %>%
#   mutate(send = str_replace(send, "United States of America", "United States"))  %>%
#   mutate(value = ifelse(value < 0, NA, value)) %>%
#   mutate(value = as.numeric(value)) %>%
#   mutate(
#     bins = case_when(
#       value < 3 ~ tags[1],
#       value >= 3 & value < 5 ~ tags[2],
#       value >= 5 & value < 10 ~ tags[3],
#       value >= 10 ~ tags[4]
#     )
#   )  %>% filter(
#     send == "United States" |
#       send == "United Kingdom"  |
#       send == "Germany" |
#       send == "Italy"  |
#       send == "Saudi Arabia"  |
#       send == "Australia"  |
#       send == "France"  |
#       send == "Canada"  |
#       send == "Russian Federation" |
#       send == "United Arab Emirates"  |
#       send == "Malaysia" |
#       send == "Qatar"
#   ) %>% filter(
#   recieve == "India" |
#     recieve == "China" |
#     recieve == "Philippines" |
#     recieve == "Bangladesh" |
#     recieve == "Pakistan" |
#     recieve == "Viet Nam" |
#     recieve == "Sri Lanka" |
#     recieve == "Nigeria"  |
#     recieve == "Indonesia" |
#     recieve == "Egypt" |
#     recieve == "Lebanon" |
#     recieve == "Nepal"
#
# )
#
# #### Calculate level summary
# receive_levels = data_corridors_min %>%
#   filter(complete.cases(.)) %>%
#   group_by(recieve) %>%
#   summarize(num = n()) %>%
#   arrange(-num) %>%
#   pull(recieve)
#
# send_levels = data_corridors_min %>%
#   filter(complete.cases(.)) %>%
#   group_by(send) %>%
#   summarize(num = n()) %>%
#   arrange(-num) %>%
#   pull(send)
#
# # mutate levels into dataframe
# df = data_corridors_min %>%
#   mutate(recieve = factor(recieve, levels = receive_levels),
#          send = fct_rev(factor(send, levels = send_levels))) %>%
#   filter(complete.cases(send))
#
# # text col
# textcol = "grey40"
#
# # Plot --------------------------------------------------------------------
# p = ggplot(df, aes(x = recieve, y = send, fill = bins)) +
#   geom_tile(colour = "white", size = 0.2) +
#   scale_colour_manual(values = c("red", "blue", "green")) +
#   guides(fill = guide_legend(title = "Average cost of sending remittances between countries")) +
#   labs(
#     x = "Countries Where Capital is Received",
#     y = "Countries Where Capital is Sent",
#     title = "World Bank Remittance Corridors Data 2020 Q3 (Latest)",
#     caption = "Remittance costs vary between sending and receiving country corridors.The SDG target aims to bring all corridor costs to below 5% of the amount remitted."
#   ) +
#   #coord_fixed()+
#   theme_grey(base_size = 10) +
#   theme(
#     legend.position = "bottom",
#     legend.title = element_text(colour = textcol),
#     legend.margin = margin(grid::unit(0, "cm")),
#     legend.text = element_text(
#       colour = textcol,
#       size = 7,
#       face = "bold"
#     ),
#     legend.key.height = grid::unit(1.4, "cm"),
#     legend.key.width = grid::unit(0.8, "cm"),
#     axis.text.x = element_text(size = 10, colour = textcol),
#     axis.text.y = element_text(vjust = 0.2, colour = textcol),
#     axis.ticks = element_line(size = 0.4),
#     plot.background = element_blank(),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "slategrey", colour = "black"),
#     plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
#     plot.title = element_text(
#       colour = textcol,
#       family = "sans",
#       hjust = 0,
#       size = 14,
#       face = "bold"
#     )
#   )
#
# p
