file_path <- file.path(dir_raw, "daiquery-Time spent breakdown UK-2024-10-07 3_19pm.csv")

# Read the Excel file
service_data <- read.csv(file_path)

aggregated_data <- service_data %>%
  group_by(month, year, service) %>%
  summarise(
    total_hours = sum(ts_total, na.rm = TRUE), 
    video_hours = sum(video_ts_total, na.rm = TRUE),
    .groups = "drop"  # Prevent warnings about grouping
  )  # Remove grouping for further operations if needed

# View the aggregated data
print(aggregated_data)

aggregated_data <- aggregated_data %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

aggregated_data <- aggregated_data %>%
  mutate(ratio = video_hours / total_hours)

p <- ggplot(aggregated_data, aes(x = date, y = ratio, color = service)) +
  geom_line() +                          
  geom_point() +                         
  labs(title = "Time spent watching videos by Service Category",
       x = "Date",
       y = "Proportion of Hours time watching videos") +
  theme_minimal() +                     
  theme(legend.title = element_blank())  



ratio_interactive_plot<- ggplotly(p)


# Save the plot as an HTML file
htmlwidgets::saveWidget(ratio_interactive_plot, "C:/Users/kerry.sloan/Documents/Projects/Meta/ratio_interactive_plot.html")

q <- ggplot(aggregated_data, aes(x = date, y = total_hours, color = service)) +
  geom_line() +                          
  geom_point() +                         
  labs(title = "Total Time Spent by Service Category",
       x = "Date",
       y = "Hours") +
  theme_minimal() +                     
  theme(legend.title = element_blank())  


total_interactive_plot <- ggplotly(q)

htmlwidgets::saveWidget(total_interactive_plot, "C:/Users/kerry.sloan/Documents/Projects/Meta/total_interactive_plot.html")

x <- ggplot(aggregated_data, aes(x = date, y = video_hours, color = service)) +
  geom_line() +                          
  geom_point() +                         
  labs(title = "Total Video Time Spent by Service Category",
       x = "Date",
       y = "Hours") +
  theme_minimal() +                     
  theme(legend.title = element_blank())  

video_interactive_plot <- ggplotly(x)

htmlwidgets::saveWidget(video_interactive_plot, "C:/Users/kerry.sloan/Documents/Projects/Meta/video_interactive_plot.html")





