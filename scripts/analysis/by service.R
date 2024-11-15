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



share_by_feature_tbl <- service_data |>
  as_tibble() |>
  mutate(
    date = ymd(paste(year, month, "1", sep="-")),
    is_not_msgr = service != "Messenger"
  ) |>
  group_by(month, year, date, service, is_not_msgr) |>
  summarise(
    views = sum(ts_total, na.rm = TRUE),
    views_ex_msgr = sum(ts_total*is_not_msgr, na.rm=TRUE),
    .groups = "drop"
  ) |>
  group_by(date, month, year) |>
  mutate(
    share = 100 * views / sum(views),
    share_ex_msgr = 100 * views*is_not_msgr / sum(views*is_not_msgr)
  ) |>
  ungroup()|>
  filter(date %in% c(ymd("2021-11-01", "2024-09-01"))) |>
  select(service, date, share, share_ex_msgr) |>
  gather(key=share_type, value=share, -service, -date) |>
  mutate(date_measure = paste(date, share_type)) |>
  select(service, date_measure, share) |>
  spread(key=date_measure, value=share) |>
  arrange(desc(`2021-11-01 share`))


# FIXME Table 1 - write me out?
share_by_feature_tbl



#Video share total line at bottom 

video_shares_by_service <- service_data |>
  as_tibble() |>
  group_by(month, year, service) |>
  summarise(
    video_ts = sum(video_ts_total, na.rm=TRUE),
    ts_total = sum(ts_total, na.rm=TRUE),
    .groups = "drop"
  ) |>
  group_by(month, year) |>
  mutate(share_of_video = 100 * (video_ts / ts_total)) |>
  ungroup()|>
  filter(year==2024 & month ==9) |>
  arrange(desc(share_of_video)) |>
  mutate(share_of_video = round(share_of_video,1))

# FIXME table 3: write me out?
video_share_total <- video_shares_by_service |>
  summarise(video_as_shr_total = 100*sum(video_ts) / sum(ts_total))


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





