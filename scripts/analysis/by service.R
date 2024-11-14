file_path <- file.path(dir_raw, "daiquery-Time spent breakdown UK-2024-10-07 3_19pm.csv")

service_data <- read.csv(file_path)

service_data <- service_data %>%
  mutate(service = recode(service,
                          "NEWS_FEED" = "Feed",
                          "VIDEOS" = "Watch",
                          "MESSENGER" = "Messenger",
                          "REELS" = "Reels",
                          "PROFILE" = "Profile",
                          "GAMES" = "Games",
                          "GROUPS" = "Groups",
                          "STORIES" = "Stories",
                          "MARKETPLACE" = "Marketplace",
                          "NOTIFICATIONS" = "Notifications",
                          "SEARCH" = "Search",
                          "PAGES" = "other",
                          "DATING" = "other",
                          "INSTANT_SHOPPING" = "other",
                          "EVENTS" = "other",
                          "NEWS" = "other",
                          "SHOPS" = "other",
                          "OTHER" = "other",
                          "FRIENDING" = "other",
                          "BOOKMARKS" = "other",
                          "MEMORIES" = "Memories",
                          "SAVED" = "other",
                          "MOST_RECENT" = "other",
                          "INFRASTRUCTURE" = "other",
                          "LIVE_VIDEO" = "other",
                          "AVATARS" = "other",
                          .default = "other"))



service_em <- service_data %>% filter(service != 'Messenger')

table_1 <- service_data %>%
  group_by(month, year, service) %>%
  summarise(Totalviews = sum(ts_total, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_month <- table_1 %>%
  group_by(month, year) %>%
  summarise(monthly_total = sum(Totalviews))

table_1 <- table_1 %>%
  left_join(df_totalviews_month, by = c("month", "year")) %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

write_xlsx(table_1, file.path(dir_output, "Table 1.xlsx"))


table_1 <- service_em %>%
  group_by(month, year, service) %>%
  summarise(Totalviews = sum(ts_total, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_month <- table_1 %>%
  group_by(month, year) %>%
  summarise(monthly_total = sum(Totalviews))

table_1 <- table_1 %>%
  left_join(df_totalviews_month, by = c("month", "year")) %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

write_xlsx(table_1, file.path(dir_output, "Table 2.xlsx"))

#Video share
table_1 <- service_data %>%
  group_by(month, year, service) %>%
  summarise(Totalviews = sum(video_ts_total, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_month <- service_data %>%
  group_by(month, year, service) %>%
  summarise(monthly_total = sum(ts_total, na.rm = TRUE))

table_1 <- table_1 %>%
  left_join(df_totalviews_month, by = c("month", "year", "service")) %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

write_xlsx(table_1, file.path(dir_output, "Table 3.xlsx"))

#Video share total line at bottom 

table_1 <- service_data %>%
  group_by(month, year) %>%
  summarise(Totalviews = sum(video_ts_total, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_month <- service_data %>%
  group_by(month, year) %>%
  summarise(monthly_total = sum(ts_total, na.rm = TRUE))

table_1 <- table_1 %>%
  left_join(df_totalviews_month, by = c("month", "year")) %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

write_xlsx(table_1, file.path(dir_output, "Table 4.xlsx"))

# Initial interactive graph- not used in slides 

aggregated_data <- service_data %>%
  group_by(month, year, service) %>%
  summarise(
    total_hours = sum(ts_total, na.rm = TRUE), 
    video_hours = sum(video_ts_total, na.rm = TRUE),
    .groups = "drop"  
  ) 

print(aggregated_data)

aggregated_data <- aggregated_data %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

aggregated_data <- aggregated_data %>% filter(date > as.Date("2022-12-01"))
aggregated_data <- aggregated_data %>% filter(service != "86")


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


htmlwidgets::saveWidget(ratio_interactive_plot, "Z:/Projects/Projects-24/P2409-6305/Work/3.ANALYSIS/5. Interoperability/Network effects analysis/Kerry Output/outputs/ratio_interactive_plot.html")

q <- ggplot(aggregated_data, aes(x = date, y = total_hours, color = service)) +
  geom_line() +                          
  geom_point() +                         
  labs(title = "Total Time Spent by Service Category",
       x = "Date",
       y = "Hours") +
  theme_minimal() +                     
  theme(legend.title = element_blank())  


total_interactive_plot <- ggplotly(q)

htmlwidgets::saveWidget(total_interactive_plot, "Z:/Projects/Projects-24/P2409-6305/Work/3.ANALYSIS/5. Interoperability/Network effects analysis/Kerry Output/outputs/total_interactive_plot.html")

x <- ggplot(aggregated_data, aes(x = date, y = video_hours, color = service)) +
  geom_line() +                          
  geom_point() +                         
  labs(title = "Total Video Time Spent by Service Category",
       x = "Date",
       y = "Hours") +
  theme_minimal() +                     
  theme(legend.title = element_blank())  

video_interactive_plot <- ggplotly(x)

htmlwidgets::saveWidget(video_interactive_plot, "Z:/Projects/Projects-24/P2409-6305/Work/3.ANALYSIS/5. Interoperability/Network effects analysis/Kerry Output/outputs/video_interactive_plot.html")





