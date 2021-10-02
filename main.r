library(WDI)
library(stringr)
library(tidyverse)


# Query world bank data on remittances  -----------------------------------
WDIsearch('remit')
dat = WDI(indicator = 'BM.TRF.MGR.CD',
          start = 2020,
          end = 2020)

dat = WDI(indicator = 'BM.TRF.MGR.CD')


# Read world bank remitance corridor data ----------------------------------------------------
#### notes: this data is not avaliable from the WorldBank R oackage WDI
#### the data has been downloaded from: https://databank.worldbank.org/source/remittance-prices-worldwide-(corridors)
data_corridors = read.csv("data/36d1ed87-9a2d-41da-8eac-c1c25d45fa65_Data.csv")
colnames(data_corridors)[1] = "Sending.Countries.Name"

# Clean that data ---------------------------------------------------------
#### all this data is messy, the format is suited for excel worksheets rather than machine readability

colnames(data_corridors)[1] = "Sending.Countries.Name" # rename column because its called some funky shit

tags = c("0-3", "3-5", "5-10", "10 and above") # create tags for bin labels

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
    send == "United Kingdom" |
      send == "France"  |
      send == "Saudi Arabia" |
      send == "Canada"  |
      send == "United States"  | send == "Australia"
  )

data_corridors_min = data_corridors_min %>% filter(
  recieve == "Philippines" |
    recieve == "China" |
    recieve == "Pakistan" |
    recieve == "Sri Lanka" |
    recieve == "Somalia"  | recieve == "Nepal"
)

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


df = data_corridors_min %>%
  mutate(recieve = factor(recieve, levels = receive_levels),
         send = fct_rev(factor(send, levels = send_levels))) %>%
  filter(complete.cases(send))


df$bins = as.character(df$bins)
df$bins[is.na(df$bins)] = "No data"
df$bins = as.factor(df$bins)
df$bins <-
  factor(df$bins,
         levels = c("0-3", "3-5", "5-10", "10 and above", "No data"))


textcol = "grey40"
p = ggplot(df, aes(x = recieve, y = send, fill = bins)) +
  geom_tile(colour = "white", size = 0.2) +
  scale_colour_manual(values = c("purple", "blue", "green")) +
  guides(fill = guide_legend(title = "Average cost of sending remittances between countries")) +
  labs(
    x = "Countries Where Capital is Received",
    y = "Countries Where Capital is Sent",
    title = "World Bank Remittance Corridors Data\n2020 Q3 (Latest)",
    caption = "Remittance costs vary between sending and receiving country corridors.\nThe SDG target aims to bring all corridor costs to below\n5% of the amount remitted."
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
    legend.key.width = grid::unit(0.2, "cm"),
    axis.text.x = element_text(size = 10, colour = textcol),
    axis.text.y = element_text(vjust = 0.2, colour = textcol),
    axis.ticks = element_line(size = 0.4),
    plot.background = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = element_text(
      colour = textcol,
      hjust = 0,
      size = 14,
      face = "bold"
    )
  )



p
## read amount paid
####data = read.csv("data/API_BM.TRF.PWKR.CD.DT_DS2_en_csv_v2_3012027.csv",skip = 4)
