options(scipen = 999)

# Construct the full file path using file.path()
file_path_2024 <- file.path(dir_raw, "daiquery-vpv breakdown in UK-2024-10-08 12_05am.csv")

# Read the Excel file
df_2024 <- read.csv(file_path_2024)

# Construct the full file path using file.path()
file_path_2018 <- file.path(dir_raw, "daiquery-vpv breakdown uk (2018-22)-2024-10-08 1_33am.csv")

# Read the Excel file
df_2018 <- read.csv(file_path_2018)

df <- rbind(df_2018, df_2024)

df_unique <- distinct(df)

df_all <- distinct(df)
df_all$month <- as.Date(paste0(df_all$month, "-01"), format = "%Y-%m-%d")
df_all <- df_all %>% filter(month >= as.Date("2022-04-01"))
df_all$is_friend[is.na(df_all$is_friend)] <- 0
df_friends_views <- df_all %>%
  group_by(month, is_friend) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE), .groups = 'drop')

# Step 2: Group by month to get total views for each month
df_total_views <- df_all %>%
  group_by(month) %>%
  summarise(Totalviews_month = sum(no_of_vpvs, na.rm = TRUE), .groups = 'drop')

# Step 3: Join the two summaries to calculate the share of friends' views
df_figure1_all <- df_friends_views %>%
  left_join(df_total_views, by = "month") %>%
  mutate(Share_friends = ifelse(is_friend == TRUE, Totalviews / Totalviews_month * 100, NA)) %>%
  filter(!is.na(Share_friends)) 



df_unique <- df_unique %>% filter(is.na(is_ad))
df_unique <- df_unique %>% filter(is.na(is_eqp))
df_unique$month <- as.Date(paste0(df_unique$month, "-01"), format = "%Y-%m-%d")
df_unique <- df_unique %>% filter(month >= as.Date("2022-04-01"))

df_unique$is_friend[is.na(df_unique$is_friend)] <- 0


df_friends_views_unique <- df_unique  %>%
  group_by(month, is_friend) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE), .groups = 'drop')

# Step 2: Group by month to get total views for each month
df_total_views_unique <- df_unique %>%
  group_by(month) %>%
  summarise(Totalviews_month = sum(no_of_vpvs, na.rm = TRUE), .groups = 'drop')

# Step 3: Join the two summaries to calculate the share of friends' views
df_figure1_unique <- df_friends_views_unique %>%
  left_join(df_total_views_unique, by = "month") %>%
  mutate(Share_friends = ifelse(is_friend == TRUE, Totalviews / Totalviews_month * 100, NA)) %>%
  filter(!is.na(Share_friends)) 


png(filename = file.path(dir_output, "Figure1_combined.png"), width = 800, height = 600)
ggplot() +
  geom_line(data = df_figure1_all, aes(x = month, y = Share_friends, color = "All Conent"), size = 1) +
  geom_point(data = df_figure1_all, aes(x = month, y = Share_friends, color = "All Conent")) +
  geom_line(data = df_figure1_unique, aes(x = month, y = Share_friends, color = "Organic Content"), size = 1) +
  geom_point(data = df_figure1_unique, aes(x = month, y = Share_friends, color = "Organic Content")) +
  labs(title = "Share of Views Originating from Friends",
       x= " ",
       y = "Share of Views (%)", 
       colour= " ") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_color_manual(values = c("All Conent" = "darkred", "Organic Content" = "red")) +
  scale_y_continuous(limits = c(0, 30),   breaks = seq(0, 30, by = 5)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.grid.major.y = element_line(color = "lightgray"),  # Only horizontal grid lines at y-axis ticks
    panel.grid.major.x = element_blank(),                                         # No vertical grid lines
    panel.grid.minor = element_blank(),     axis.line = element_line(color = "black", size = 0.5),
    legend.title = element_text(size = 14),    # Increase legend title size
    legend.text = element_text(size = 12),     # Increase legend text size
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

df_friends_views <- df_all %>%
  group_by(year, is_friend) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE), .groups = 'drop')

# Step 2: Group by month to get total views for each month
df_total_views <- df_all %>%
  group_by(year) %>%
  summarise(Totalviews_month = sum(no_of_vpvs, na.rm = TRUE), .groups = 'drop')

# Step 3: Join the two summaries to calculate the share of friends' views
df_figure1_all <- df_friends_views %>%
  left_join(df_total_views, by = "year") %>%
  mutate(Share_friends = ifelse(is_friend == TRUE, Totalviews / Totalviews_month * 100, NA)) %>%
  filter(!is.na(Share_friends)) 

write_xlsx(df_figure1_all, file.path(dir_output, "Figure 1.2.xlsx"))


df_feature <- df_unique %>% 
  mutate(feature = case_when(
    surface == "top_news" ~ "Feed",
    surface == "video_chaining" ~ "Watch",
    surface == "vh_live" ~ "Watch",
    surface == "photos" ~ "Full-screen photo view",
    surface == "campus_feed" ~ "Feed",
    surface == "marketplace" ~ "Marketplace",
    surface == "notifications_public_figure_minifeed" ~ "Other",
    surface == "top_of_feed_unit" ~ "Feed",
    surface == "showcase_unit" ~ "Reels",
    surface == "groups" ~ "Groups",
    surface == "pages" ~ "Pages",
    surface == "timeline" ~ "Profile",
    surface == "sfv_shorts" ~ "Reels",
    surface == "fb_stories" ~ "Stories",
    surface == "permalink" ~ "Full-screen post view",
    surface == "groups_tab" ~ "Groups",
    surface == "unknown_interaction_source" ~ "Other",
    surface == "search_results" ~ "Search",
    surface == "page_video_thumbnail" ~ "Pages",
    surface == "fb_stories_chaining" ~ "Stories",
    surface == "video_fullscreen_player" ~ "Other",
    surface == "fb_shorts_tofu" ~ "Reels",
    surface == "most_recent_feed" ~ "Other",
    surface == "news_tab" ~ "Other",
    surface == "seen_feed" ~ "Feed",
    surface == "biz_disco_feed" ~ "Other",
    surface == "snowflake_medias" ~ "Other",
    surface == "notifications_close_friend_minifeed" ~ "Other",
    surface == "notifications_public_figure_highlights_minifeed" ~ "Other",
    surface == "groups_social_learning_tab" ~ "Groups",
    surface == "video_catalog_thumbnail" ~ "Other",
    surface == "fan_subscriptions_exclusive_content_feed" ~ "Other",
    surface == "pages_social_learning_tab" ~ "Other",
    surface == "local_community_feed" ~ "Other",
    surface == "notifications_minifeed" ~ "Other",
    surface == "custom_feed" ~ "Other",
    surface == "favorites_feed" ~ "Feed",
    surface == "jewel_tray_minifeed" ~ "Other",
    surface == "shops_ratings_and_reviews" ~ "Other",
    surface == "feeds_tab" ~ "Feed",
    surface == "dessert_feed" ~ "Feed",
    surface == "media_playlist" ~ "Other",
    surface == "top_of_home_stories" ~ "Stories",
    surface == "group_admin_recommendation_feed" ~ "Other",
    surface == "profile_plus_social_learning_tab" ~ "Profile",
    surface == "fb_shorts_towu" ~ "Reels",
    surface == "creator_digest" ~ "Other",
    surface == "marketplace_pdp_recommended" ~ "Marketplace",
    surface == "notifications_close_friend_activity_minifeed" ~ "Other",
    surface == "notifications_nf_photo_story_minifeed" ~ "Other",
    surface == "notifications_nf_share_story_minifeed" ~ "Other",
    surface == "wp_key_updates" ~ "Other",
    surface == "wp_key_updates_top_of_feed_unit_card" ~ "Other",
    surface == "wp_key_updates_top_of_feed_unit_hover" ~ "Other",
    surface == "wp_key_updates_top_of_group_unit_card" ~ "Other",
    surface == "wp_key_updates_top_of_group_unit_hover" ~ "Other",
    surface == "permalink_or_photo_chaining" ~ "Other",
    surface == "ms_teams_integration_group_feed" ~ "Other",
    surface == "ms_teams_integration_news_feed" ~ "Other",
    surface == "notifications_public_figure_comment_minifeed" ~ "Other",
    TRUE ~ "Other"  # Default label if none match
  ))

df_feature_all <- df_all %>% 
  mutate(feature = case_when(
    surface == "top_news" ~ "Feed",
    surface == "video_chaining" ~ "Watch",
    surface == "vh_live" ~ "Watch",
    surface == "photos" ~ "Full-screen photo view",
    surface == "campus_feed" ~ "Feed",
    surface == "marketplace" ~ "Marketplace",
    surface == "notifications_public_figure_minifeed" ~ "Other",
    surface == "top_of_feed_unit" ~ "Feed",
    surface == "showcase_unit" ~ "Reels",
    surface == "groups" ~ "Groups",
    surface == "pages" ~ "Pages",
    surface == "timeline" ~ "Profile",
    surface == "sfv_shorts" ~ "Reels",
    surface == "fb_stories" ~ "Stories",
    surface == "permalink" ~ "Full-screen post view",
    surface == "groups_tab" ~ "Groups",
    surface == "unknown_interaction_source" ~ "Other",
    surface == "search_results" ~ "Search",
    surface == "page_video_thumbnail" ~ "Pages",
    surface == "fb_stories_chaining" ~ "Stories",
    surface == "video_fullscreen_player" ~ "Other",
    surface == "fb_shorts_tofu" ~ "Reels",
    surface == "most_recent_feed" ~ "Other",
    surface == "news_tab" ~ "Other",
    surface == "seen_feed" ~ "Feed",
    surface == "biz_disco_feed" ~ "Other",
    surface == "snowflake_medias" ~ "Other",
    surface == "notifications_close_friend_minifeed" ~ "Other",
    surface == "notifications_public_figure_highlights_minifeed" ~ "Other",
    surface == "groups_social_learning_tab" ~ "Groups",
    surface == "video_catalog_thumbnail" ~ "Other",
    surface == "fan_subscriptions_exclusive_content_feed" ~ "Other",
    surface == "pages_social_learning_tab" ~ "Other",
    surface == "local_community_feed" ~ "Other",
    surface == "notifications_minifeed" ~ "Other",
    surface == "custom_feed" ~ "Other",
    surface == "favorites_feed" ~ "Feed",
    surface == "jewel_tray_minifeed" ~ "Other",
    surface == "shops_ratings_and_reviews" ~ "Other",
    surface == "feeds_tab" ~ "Feed",
    surface == "dessert_feed" ~ "Feed",
    surface == "media_playlist" ~ "Other",
    surface == "top_of_home_stories" ~ "Stories",
    surface == "group_admin_recommendation_feed" ~ "Other",
    surface == "profile_plus_social_learning_tab" ~ "Profile",
    surface == "fb_shorts_towu" ~ "Reels",
    surface == "creator_digest" ~ "Other",
    surface == "marketplace_pdp_recommended" ~ "Marketplace",
    surface == "notifications_close_friend_activity_minifeed" ~ "Other",
    surface == "notifications_nf_photo_story_minifeed" ~ "Other",
    surface == "notifications_nf_share_story_minifeed" ~ "Other",
    surface == "wp_key_updates" ~ "Other",
    surface == "wp_key_updates_top_of_feed_unit_card" ~ "Other",
    surface == "wp_key_updates_top_of_feed_unit_hover" ~ "Other",
    surface == "wp_key_updates_top_of_group_unit_card" ~ "Other",
    surface == "wp_key_updates_top_of_group_unit_hover" ~ "Other",
    surface == "permalink_or_photo_chaining" ~ "Other",
    surface == "ms_teams_integration_group_feed" ~ "Other",
    surface == "ms_teams_integration_news_feed" ~ "Other",
    surface == "notifications_public_figure_comment_minifeed" ~ "Other",
    TRUE ~ "Other"  # Default label if none match
  ))


# Summarise total views per month and feature
df_figure2 <- df_feature %>%
  group_by(month, feature) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

# Calculate total views for each month
df_totalviews_month <- df_figure2 %>%
  group_by(month) %>%
  summarise(monthly_total = sum(Totalviews))

# Merge total views back to the original data and calculate share
df_figure2 <- df_figure2 %>%
  left_join(df_totalviews_month, by = "month") %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

# Ensure 'month' is converted to a Date format
df_figure2$month <- as.Date(paste0(df_figure2$month, "-01"), format = "%Y-%m-%d")
df_figure2 <- df_figure2 %>% select(-Totalviews, -monthly_total)

reshaped_fig2= df_figure2 %>% pivot_wider(names_from=month, values_from=share_of_views, names_prefix= "share_of_views_") 
write_xlsx(reshaped_fig2, file.path(dir_output, "Figure 2.xlsx"))



png(filename = file.path(dir_output, "Figure 2.png"), width = 800, height = 600)

ggplot(df_figure2, aes(x= month, y=share_of_views, color=factor(feature), group=feature)) +
  geom_line() +
  geom_point() +
  labs(title="Share of views of content by where the content occured",
       x= "date",
       y="proportion of views",
       color="Where the content occured") +
  scale_x_date( date_breaks = "3 month",           # Show dates with a 1-month gap
                date_labels = "%b %Y") +
  scale_color_brewer(palette = "Paired") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  expand_limits(y = 0)


dev.off()



# Filter for video posts
df_video <- df_feature %>% filter(post_type_content_type == "video")

# Summarize total views per month and feature for video content
df_video <- df_video %>% 
  group_by(month, feature) %>%
  summarise(TotalVideoTime = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

# Calculate total views for all content types per feature and month
df_total_time_feature <- df_feature %>%
  group_by(month, feature) %>%
  summarise(total_time_per_feature = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

# Merge total views per feature back to the video data
df_video <- df_video %>%
  left_join(df_total_time_feature, by = c("month", "feature")) %>%
  mutate(video_share_of_time = TotalVideoTime / total_time_per_feature) %>%
  ungroup()

# Ensure 'month' is converted to a Date format
df_video$month <- as.Date(paste0(df_video$month, "-01"), format = "%Y-%m-%d")
df_video <- df_video %>% select(-TotalVideoTime, -total_time_per_feature)

# Reshape the data for output
reshaped_fig2 <- df_video %>% pivot_wider(names_from = month, values_from = video_share_of_time, names_prefix = "video_share_of_time_") 

# Write to Excel
write_xlsx(reshaped_fig2, file.path(dir_output, "Figure 2.1.xlsx"))





# Ensure 'month' is converted to a Date format
df_feature$month <- as.Date(paste0(df_feature$month, "-01"), format = "%Y-%m-%d")
df_feature$year <- format(df_feature$month, "%Y")

#making content source variable 
df_content <- df_feature

df_content <- df_content %>%
  mutate(content_source = ifelse(is_friend == 1, 
                                 "Friends", 
                                 ifelse(is_friend == 0 & post_author_profile_type %in% c("PAGE", "ADDITIONAL_PROFILE_PLUS", "PRIMARY_PROFILE_PLUS"),
                                        "Page", 
                                        ifelse(post_author_profile_type %in% c("INSTAGRAM_USER_V2", "INSTAGRAM_USER"), 
                                               "Instagram users", 
                                               ifelse(is_friend == 0 & post_author_profile_type == "MAIN_PROFILE", 
                                                      "Unconnected users",
                                                      "Other")))))


df_figure5 <- df_content %>%
  group_by(year, content_source, feature) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

# Calculate total views for each month
df_totalviews_year_feature <- df_figure5 %>%
  group_by(year, feature) %>%
  summarise(year_total = sum(Totalviews))

# Merge total views back to the original data and calculate share
df_figure5 <- df_figure5 %>%
  left_join(df_totalviews_year_feature, by = c("year", "feature")) %>%
  mutate(share_of_views = Totalviews / year_total) %>%
  ungroup()


df_figure5 <- df_figure5 %>% select(-Totalviews, -year_total)
# Step 1: Pivot the data
reshaped_fig5 <- df_figure5 %>%
  pivot_wider(
    names_from = c(year, content_source),  # Columns from year and post_origin combinations
    values_from = share_of_views,       # Values to fill (share_of_views)
    names_sep = "_"                     # Add separator for clearer column names
  )


write_xlsx(reshaped_fig5, file.path(dir_output, "Figure 5.xlsx"))

df_figure5 <- df_content %>%
  group_by(year, content_source) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

# Calculate total views for each month
df_totalviews_year_feature <- df_figure5 %>%
  group_by(year) %>%
  summarise(year_total = sum(Totalviews))

# Merge total views back to the original data and calculate share
df_figure5 <- df_figure5 %>%
  left_join(df_totalviews_year_feature, by = c("year")) %>%
  mutate(share_of_views = Totalviews / year_total) %>%
  ungroup()


df_figure5 <- df_figure5 %>% select(-Totalviews, -year_total)

reshaped_fig5 <- df_figure5 %>%
  pivot_wider(
    names_from = c(year, content_source),  # Columns from year and post_origin combinations
    values_from = share_of_views,       # Values to fill (share_of_views)
    names_sep = "_"                     # Add separator for clearer column names
  )


write_xlsx(reshaped_fig5, file.path(dir_output, "Figure 5.1.xlsx"))

df_figure5 <- df_content %>%
  group_by(year, feature) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

# Calculate total views for each month
df_totalviews_year_feature <- df_figure5 %>%
  group_by(year) %>%
  summarise(year_total = sum(Totalviews))

# Merge total views back to the original data and calculate share
df_figure5 <- df_figure5 %>%
  left_join(df_totalviews_year_feature, by = c("year")) %>%
  mutate(share_of_views = Totalviews / year_total) %>%
  ungroup()


df_figure5 <- df_figure5 %>% select(-Totalviews, -year_total)
write_xlsx(df_figure5, file.path(dir_output, "Figure 5.2.xlsx"))


df_figure6 <- df_feature %>% filter(is_friend == 1) %>%
  group_by(year, feature, post_origin) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

# Calculate total views for each month
df_totalviews_month <- df_figure6 %>%
  group_by(year) %>%
  summarise(year_total = sum(Totalviews))

# Merge total views back to the original data and calculate share
df_figure6 <- df_figure6 %>%
  left_join(df_totalviews_month, by = "year") %>%
  mutate(share_of_views = Totalviews / year_total) %>%
  ungroup()

df_figure6 <- df_figure6 %>% filter(post_origin != "not logged" & post_origin != "Not Logged" & post_origin != "" & !is.na(post_origin))
df_figure6 <- df_figure6 %>% select(-Totalviews, -year_total)
# Step 1: Pivot the data
reshaped_fig6 <- df_figure6 %>%
  pivot_wider(
    names_from = c(year, post_origin),  # Columns from year and post_origin combinations
    values_from = share_of_views,       # Values to fill (share_of_views)
    names_sep = "_"                     # Add separator for clearer column names
  )


write_xlsx(reshaped_fig6, file.path(dir_output, "Figure 6.xlsx"))


# Summarise total views per month and feature
df_figure7 <- df_unique %>%
  group_by(month, post_type_content_type, is_friend) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

# Calculate total views for each month
df_totalviews_month <- df_figure7 %>%
  group_by(month, post_type_content_type) %>%
  summarise(monthly_total = sum(Totalviews))

#Merge total views back to the original data and calculate share
df_figure7 <- df_figure7 %>%
  left_join(df_totalviews_month, by = c("month", "post_type_content_type")) %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

# Ensure 'month' is converted to a Date format

df_figure7 <- df_figure7 %>% filter(is_friend == 1)

df_figure7 <- df_figure7 %>% select(-Totalviews, -monthly_total, -is_friend)


reshaped_fig7 <- df_figure7 %>%
  pivot_wider(
    names_from = c(month),  # Columns from year and post_origin combinations
    values_from = share_of_views,       # Values to fill (share_of_views)
    names_prefix= "share_of_views_"                    # Add separator for clearer column names
  )

write_xlsx(reshaped_fig7, file.path(dir_output, "Figure 7.xlsx"))


png(filename = file.path(dir_output, "Figure 7.png"), width = 800, height = 600)

ggplot(df_figure7, aes(x= month, y=share_of_views, color=factor(post_type_content_type), group=post_type_content_type)) +
  geom_line() +
  geom_point() +
  labs(title=" Share of content views from friends by type",
       x= "Date",
       y="proportion of views",
       color="Type") +
  scale_x_date( date_breaks = "3 month",           # Show dates with a 1-month gap
                date_labels = "%b %Y") +
  scale_color_brewer(palette = "Paired") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  expand_limits(y = 0)

dev.off()


# Summarise total views per month and feature
df_figure8 <- df_unique %>%
  group_by(month, is_friend, post_origin) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

# Calculate total views for each month
df_totalviews_month <- df_figure8 %>%
  group_by(month) %>%
  summarise(monthly_total = sum(Totalviews))

# Merge total views back to the original data and calculate share
df_figure8 <- df_figure8 %>%
  left_join(df_totalviews_month, by = "month") %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()
df_figure8 <- df_figure8 %>% filter(is_friend == 1)
df_figure8 <- df_figure8 %>% filter(post_origin == c("original", "reshare"))
df_figure8$month <- as.Date(paste0(df_figure8$month, "-01"), format = "%Y-%m-%d")

df_figure8 <- df_figure8 %>% select(-Totalviews, -monthly_total, -is_friend)


reshaped_fig8 <- df_figure8 %>%
  pivot_wider(
    names_from = c(month),  # Columns from year and post_origin combinations
    values_from = share_of_views,       # Values to fill (share_of_views)
    names_prefix= "share_of_views_"                    # Add separator for clearer column names
  )


write_xlsx(reshaped_fig8, file.path(dir_output, "Figure 8.xlsx"))

totals <- df_figure8 %>%
  group_by(month) %>%
  summarise(post_origin = "total", count = sum(share_of_views)) # Summarize counts

# Combine the original data with the totals
df_figure8 <- bind_rows(df_figure8, totals) %>%
  arrange(month, desc(post_origin)) 

df_figure8 <- df_figure8 %>%
  mutate(share_of_views = ifelse(is.na(share_of_views), count, share_of_views))


png(filename = file.path(dir_output, "Figure 8.png"), width = 800, height = 600)

ggplot(df_figure8, aes(x= month, y=share_of_views, color=factor(post_origin), group=post_origin)) +
  geom_line() +
  geom_point() +
  labs(title="Content views of content produced by Facebook “friends” as a proportion of total content views ",
       y="proportion of views",
       x= " ",
       color=" ") +
  scale_x_date( date_breaks = "2 month",           # Show dates with a 1-month gap
                date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 0.3),   breaks = seq(0, 0.3, by = 0.05)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 13),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.grid.major.y = element_line(color = "lightgray"),  # Only horizontal grid lines at y-axis ticks
        panel.grid.major.x = element_blank(),                                         # No vertical grid lines
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black", size = 0.5),
        legend.title = element_text(size = 14),    # Increase legend title size
        legend.text = element_text(size = 12),     # Increase legend text size
        legend.key.size = unit(1.5, "lines"))  +
  expand_limits(y = 0)

dev.off()

# Summarise total views per month and feature
df_figure8 <- df_unique %>%
  group_by(year, is_friend, post_origin) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

# Calculate total views for each month
df_totalviews_month <- df_figure8 %>%
  group_by(year) %>%
  summarise(monthly_total = sum(Totalviews))

# Merge total views back to the original data and calculate share
df_figure8 <- df_figure8 %>%
  left_join(df_totalviews_month, by = "year") %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()
df_figure8 <- df_figure8 %>% filter(is_friend == 1)

df_figure8 <- df_figure8 %>% select(-Totalviews, -monthly_total, -is_friend)

write_xlsx(df_figure8, file.path(dir_output, "Figure 8.1.xlsx"))

# Summarise total views per month and feature
df_figure8 <- df_all %>%
  group_by(year, is_friend, post_origin) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

# Calculate total views for each month
df_totalviews_month <- df_figure8 %>%
  group_by(year) %>%
  summarise(monthly_total = sum(Totalviews))

# Merge total views back to the original data and calculate share
df_figure8 <- df_figure8 %>%
  left_join(df_totalviews_month, by = "year") %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()
df_figure8 <- df_figure8 %>% filter(is_friend == 1)

df_figure8 <- df_figure8 %>% select(-Totalviews, -monthly_total, -is_friend)

write_xlsx(df_figure8, file.path(dir_output, "Figure 8.2.xlsx"))
