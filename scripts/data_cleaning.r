# Read world bank remitance corridor data ----------------------------------------------------
#### notes: this data is not avaliable from the WorldBank R oackage WDI


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
  mutate(
    Sending.Countries.Name = str_replace(
      Sending.Countries.Name,
      "United States of America",
      "United States"
    )
  )  %>%
  mutate(Sending.Countries.Name = str_replace(Sending.Countries.Name, " Korea, Rep.", "South Korea"))
