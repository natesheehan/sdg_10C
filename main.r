# Librarys ----------------------------------------------------------------
library(stringr)
library(tidyverse)

# Read world bank remitance corridor data ----------------------------------------------------
#### notes: this data is not avaliable from the WorldBank R oackage WDI
tags = c(" 0-3%", " 3-5%", " 5-10%", "10% and above") # create tags for bin labels
#### the data has been downloaded from: https://databank.worldbank.org/source/remittance-prices-worldwide-(corridors)
data_corridors = read.csv("data/36d1ed87-9a2d-41da-8eac-c1c25d45fa65_Data.csv")
colnames(data_corridors)[1] = "Sending.Countries.Name" # rename column because its called some funky shit


data_corridors[data_corridors == ".."] = 0

data_corridors[, 7:41] <- sapply(data_corridors[, 7:41], as.numeric)
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
  )
data[data == 0] = NA



# Tidy DF for GGplot // using year 2019 q3------------------------------------------------------
data_corridors_min = data_corridors %>%
  select(Sending.Countries.Name,
         Receiving.Countries..Name,
         X2020Q3..YR2020Q3.) %>%
  filter(X2020Q3..YR2020Q3. != "..") %>%
  rename(value = X2020Q3..YR2020Q3.) %>%
  rename(send = Sending.Countries.Name) %>%
  rename(recieve = Receiving.Countries..Name) %>%
  mutate(send = str_replace(send, "United States of America", "United States"))  %>%
  mutate(value = ifelse(value < 0, NA, value)) %>%
  mutate(
    bins = case_when(
      value < 3 ~ tags[1],
      value >= 3 & value < 5 ~ tags[2],
      value >= 5 & value < 10 ~ tags[3],
      value >= 10 ~ tags[4]
    )
  )  %>% filter(
    send == "United States" |
      send == "United Kingdom"  |
      send == "Germany" |
      send == "Italy"  |
      send == "Saudi Arabia"  |
      send == "Australia"  |
      send == "France"  |
      send == "Canada"  |
      send == "Russian Federation" |
      send == "South Africa"
  )

data_corridors_min = data_corridors_min %>% filter(
  recieve == "India" |
    recieve == "China" |
    recieve == "Philippines" |
    recieve == "Bangladesh" |
    recieve == "Pakistan" |
    recieve == "Viet Nam" |
    recieve == "Sri Lanka" |
    recieve == "Nigeria"  |
    recieve == "Indonesia" |
    recieve == "Egypt"
)

#### Calculate level summary
receive_levels = data_corridors_min %>%
  filter(complete.cases(.)) %>%
  group_by(recieve) %>%
  summarize(num = n()) %>%
  arrange(-num) %>%
  pull(recieve)

send_levels = data_corridors_min %>%
  filter(complete.cases(.)) %>%
  group_by(send) %>%
  summarize(num = n()) %>%
  arrange(-num) %>%
  pull(send)

# mutate levels into dataframe
df = data_corridors_min %>%
  mutate(recieve = factor(recieve, levels = receive_levels),
         send = fct_rev(factor(send, levels = send_levels))) %>%
  filter(complete.cases(send))

# text col
textcol = "grey40"

# Plot --------------------------------------------------------------------
p = ggplot(df, aes(x = recieve, y = send, fill = bins)) +
  geom_tile(colour = "white", size = 0.2) +
  scale_colour_manual(values = c("red", "blue", "green")) +
  guides(fill = guide_legend(title = "Average cost of sending remittances between countries")) +
  labs(
    x = "Countries Where Capital is Received",
    y = "Countries Where Capital is Sent",
    title = "World Bank Remittance Corridors Data 2020 Q3 (Latest)",
    caption = "Remittance costs vary between sending and receiving country corridors.The SDG target aims to bring all corridor costs to below 5% of the amount remitted."
  ) +
  #coord_fixed()+
  theme_grey(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(colour = textcol),
    legend.margin = margin(grid::unit(0, "cm")),
    legend.text = element_text(
      colour = textcol,
      size = 7,
      face = "bold"
    ),
    legend.key.height = grid::unit(1.4, "cm"),
    legend.key.width = grid::unit(0.8, "cm"),
    axis.text.x = element_text(size = 10, colour = textcol),
    axis.text.y = element_text(vjust = 0.2, colour = textcol),
    axis.ticks = element_line(size = 0.4),
    plot.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      family = "KyivType Sans",
      hjust = 0,
      size = 14,
      face = "bold"
    )
  )

p
