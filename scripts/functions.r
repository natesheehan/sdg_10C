#### function variables
tags = c(" 0-3%",
         " 3-5%",
         " 5-10%",
         "10% and above") # create tags for bin labels



##### FUNCTIONS ---------------------------------------------------------------

# Plot historic remitance corridor rates set by receiving countries ---------
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
    facet_wrap(~ year, ncol = 1) +
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

}

# test
plot_rec_country(country = "Thailand")


# Plot historic remitance corridor rates set by sending countries ---------
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
    facet_wrap(~ year, ncol = 1) +
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

}

# test
plot_send_country(country = "Australia")

##### test code to build a overview map

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
