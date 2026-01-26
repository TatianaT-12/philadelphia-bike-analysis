# Q1 Twoline graph code:
q1_bike_rides %>%
  ggplot(aes(x=start_date, y=n_rides, color=trip_route_category)) + geom_line() + 
  labs(
    title = "Bike rides in Philadelphia",
    subtitle = "Daily Totals in 2021",
    x = "",
    y = "",
    caption = "Source: BSTAT project" ) +
  scale_x_date(date_labels = "%B") + 
  scale_color_manual(values=c("red", "green")) +
  guides(color= guide_legend(position ="inside")) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 20, face = "bold"), 
    axis.text.x = element_text(size =10),
    axis.text.y = element_text(size =10),
    legend.title = element_blank(),
    legend.position.inside = c(0.1, 0.7)
     ) 
#Bike Rides over 1 Hour code:
trips_greater_than_60 <- bike_rides%>%
  mutate(duration= as.numeric(duration)) %>%
  filter(duration > 60) %>%
  group_by(start_date) %>%
  summarize(n_rides=n(), .groups = "drop")

trips_greater_than_60 %>%
  ggplot(aes(x = start_date, y = n_rides)) + 
  geom_line(color = "#07294D", linewidth = .75, linetype = "solid") +
  labs(
    title = "Bike rides in Philadelphia over 1 Hour",
    subtitle = "Daily Totals in 2021",
    x = "",
    y = "",
    caption = "Source: BSTAT project" ) +
  scale_x_date(date_labels = "%B") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 10, face = "bold"), 
    axis.text.x = element_text(size =10),
    axis.text.y = element_text(size =10)
  ) 
#Top 10 most used bikes in Philadelphia code:
trips_by_bike_id <- bike_rides %>%
  mutate(start_date = as.Date(start_date),
         month_name = months(start_date)) %>%
  filter(month_name %in% c("January", "February", "March")) %>%
  count(bike_id, name= "n_trips") %>%
  arrange(desc(n_trips)) %>%
  slice_max(order_by = n_trips, n = 10, with_ties = FALSE)

trips_by_bike_id %>%
  ggplot(aes(x = reorder(bike_id, n_trips), y = n_trips)) + 
  geom_col(fill = "#07294D") +
  coord_flip() +
  labs(
    title = "Top 10 most used bikes in Q1 2021 in Philadelphia",
    x = "",
    y = "",
    caption = "Source: BSTAT project" ) +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 10, face = "bold"), 
    axis.text.x = element_text(size =10),
    axis.text.y = element_text(size =10)
  ) 