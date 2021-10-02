# Read world bank data ----------------------------------------------------
#### notes: world bank data is messy to access, no clear API, and lack of R packages...
## read amount paid
data = read.csv("data/API_BM.TRF.PWKR.CD.DT_DS2_en_csv_v2_3012027.csv",skip = 4)
data_corridors = read.csv("data/36d1ed87-9a2d-41da-8eac-c1c25d45fa65_Data.csv")
colnames(data_corridors)[1]="Sending.Countries.Name"

# Clean that data ---------------------------------------------------------
#### all this data is messy, the format is suited for excel worksheets rather than machine readability

colnames(data_corridors)[1]="Sending.Countries.Name" # rename column because its called some funky shit

tags = c("0-3","3-5", "5-10", "10 and above") # create tags for bin labels

data_corridors_min = data_corridors %>%
  select(Sending.Countries.Name,Receiving.Countries..Name,X2020Q3..YR2020Q3.) %>%
  filter(X2020Q3..YR2020Q3. != "..") %>%
  rename(value = X2020Q3..YR2020Q3.) %>%
  rename(send = Sending.Countries.Name) %>%
  rename(recieve = Receiving.Countries..Name) %>%
  mutate(send = str_replace(send, "United States of America", "United States"))  %>%
  mutate(value = ifelse(value < 0, NA, value)) %>%
  mutate(bins = case_when(
    value < 3 ~ tags[1],
    value >= 3 & value < 5 ~ tags[2],
    value >= 5 & value < 10 ~ tags[3],
    value >= 10 ~ tags[4]
  ))

receive_levels <- data_corridors_min %>%
  filter(complete.cases(.)) %>%
  group_by(recieve) %>%
  summarize(num = n()) %>%
  arrange(-num) %>%
  pull(recieve)

send_levels <- data_corridors_min %>%
  filter(complete.cases(.)) %>%
  group_by(send) %>%
  summarize(num = n()) %>%
  arrange(-num) %>%
  pull(send)


df <- data_corridors_min %>%
  mutate(
    recieve = factor(recieve, levels = receive_levels),
    send = fct_rev(factor(send, levels = send_levels))) %>%
  filter(complete.cases(send))


df$bins <- as.character(df$bins)
df$bins[is.na(df$bins)] <- "No data"
df$bins <- as.factor(df$bins)
df$bins <- factor(df$bins, levels=c("0-3","3-5","5-10","10 and above","No data"))


ggplot(df, aes(recieve, send, fill = bins, label = round(value, 1))) +
  geom_tile(color="white") +
  scale_x_discrete(label = df$send ,position="top") +
  scale_y_discrete(label = df$recieve) +
  ylab("Sending from this country\n") +
  xlab("To this country") +
  theme(
    axis.title.x = element_text(margin = margin(0,0,0,0,unit = "mm")),
    axis.title.y = element_text(angle=90),
    axis.text.x = element_text(angle =35,hjust=0),
    panel.grid = element_blank(),
    plot.margin=margin(0,25,5,0,"mm")
  )

