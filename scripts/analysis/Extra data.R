expand_limits(y = 0)

options(scipen = 999)


# Construct the full file path using file.path()
file_path_2024 <- file.path(dir_raw, "daiquery-vpv breakdown in UK (anon)-2024-11-02 3_50pm.csv")

# Read the Excel file
df_2024 <- read.csv(file_path_2024)

df_unique <- distinct(df_2024)
df_unique <- df_unique %>% filter(is.na(is_ad))
df_unique <- df_unique %>% filter(is.na(is_eqp))
df_unique <- df_unique %>% filter(is.na(is_qp))
df_unique$month <- as.Date(paste0(df_unique$month, "-01"), format = "%Y-%m-%d")
df_unique <- df_unique %>% filter(month >= as.Date("2023-10-01"))
df_unique <- df_unique %>% filter(month <= as.Date("2024-06-01"))

df_unique$is_friend[is.na(df_unique$is_friend)] <- 0

df_all <- distinct(df_2024)
df_all$month <- as.Date(paste0(df_all$month, "-01"), format = "%Y-%m-%d")
df_all <- df_all %>% filter(month >= as.Date("2023-10-01"))
df_all <- df_all %>% filter(month <= as.Date("2024-06-01"))

df_all$is_friend[is.na(df_all$is_friend)] <- 0

#Slide 10
df_friends_views <- df_all %>%
  group_by(month, is_friend) %>%
  summarise(Totalviews = sum(cululative_vpv_duration, na.rm = TRUE), .groups = 'drop')

# Step 2: Group by month to get total views for each month
df_total_views <- df_all %>%
  group_by(month) %>%
  summarise(Totalviews_month = sum(cululative_vpv_duration, na.rm = TRUE), .groups = 'drop')

# Step 3: Join the two summaries to calculate the share of friends' views
df_figure1_all <- df_friends_views %>%
  left_join(df_total_views, by = "month") %>%
  mutate(Share_friends = ifelse(is_friend == TRUE, Totalviews / Totalviews_month * 100, NA)) %>%
  filter(!is.na(Share_friends)) 


df_friends_views_unique <- df_unique  %>%
  group_by(month, is_friend) %>%
  summarise(Totalviews = sum(cululative_vpv_duration, na.rm = TRUE), .groups = 'drop')

# Step 2: Group by month to get total views for each month
df_total_views_unique <- df_unique %>%
  group_by(month) %>%
  summarise(Totalviews_month = sum(cululative_vpv_duration, na.rm = TRUE), .groups = 'drop')

# Step 3: Join the two summaries to calculate the share of friends' views
df_figure1_unique <- df_friends_views_unique %>%
  left_join(df_total_views_unique, by = "month") %>%
  mutate(Share_friends = ifelse(is_friend == TRUE, Totalviews / Totalviews_month * 100, NA)) %>%
  filter(!is.na(Share_friends)) 


png(filename = file.path(dir_output, "Figure1_dwell_time.png"), width = 800, height = 600)
ggplot() +
  geom_line(data = df_figure1_all, aes(x = month, y = Share_friends, color = "All Conent"), size = 1) +
  geom_point(data = df_figure1_all, aes(x = month, y = Share_friends, color = "All Conent")) +
  geom_line(data = df_figure1_unique, aes(x = month, y = Share_friends, color = "Organic Content"), size = 1) +
  geom_point(data = df_figure1_unique, aes(x = month, y = Share_friends, color = "Organic Content")) +
  labs(title = "Share of Dwell Time Originating from Friends",
       x= " ",
       y = "Share of Dwell Time (%)", 
       colour= " ") +
  scale_x_date(date_breaks = "1 months", date_labels = "%b %Y") +
  scale_color_manual(values = c("All Conent" = "darkred", "Organic Content" = "red")) +
  scale_y_continuous(limits = c(0, 20),   breaks = seq(0, 20, by = 2)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.grid.major.y = element_line(color = "lightgray"),  # Only horizontal grid lines at y-axis ticks
    panel.grid.major.x = element_blank(),                                         # No vertical grid lines
    panel.grid.minor = element_blank(),     axis.line = element_line(color = "black", size = 0.5),
    legend.title = element_text(size = 14),    # Increase legend title size
    legend.text = element_text(size = 15),     # Increase legend text size
    legend.key.size = unit(1.5, "lines")# Remove minor grid lines
  ) + 
  expand_limits(y = 0)
dev.off()


df_unique$year <- format(df_unique$month, "%Y")
df_all$year <- format(df_all$month, "%Y")

df_friends_views_unique <- df_unique  %>%
  group_by(year, is_friend) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE), .groups = 'drop')

# Step 2: Group by month to get total views for each month
df_total_views_unique <- df_unique %>%
  group_by(year) %>%
  summarise(Totalviews_month = sum(no_of_vpvs, na.rm = TRUE), .groups = 'drop')

# Step 3: Join the two summaries to calculate the share of friends' views
df_figure1_unique <- df_friends_views_unique %>%
  left_join(df_total_views_unique, by = "year") %>%
  mutate(Share_friends = ifelse(is_friend == TRUE, Totalviews / Totalviews_month * 100, NA)) %>%
  filter(!is.na(Share_friends)) 

write_xlsx(df_figure1_unique, file.path(dir_output, "Figure 1.1.xlsx"))
