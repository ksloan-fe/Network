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

df_unique$is_friend[is.na(df_unique$is_friend)] <- 0


df_figure1 <- df_unique %>% group_by(month, is_friend) %>% summarise(Totalviews= sum(no_of_vpvs))
# Ensure 'month' is converted to a Date format
df_figure1$month <- as.Date(paste0(df_figure1$month, "-01"), format = "%Y-%m-%d")

png(filename = file.path(dir_output, "Figure 1.png"), width = 800, height = 600)

ggplot(df_figure1, aes(x= month, y=Totalviews, color=factor(is_friend), group=is_friend)) +
  geom_line() +
  geom_point() +
  labs(title="Views of content originating from friends or other sources",
       x= "date",
       y="views",
       color="Originating from") +
  scale_x_date( date_breaks = "3 month",           # Show dates with a 1-month gap
               date_labels = "%b %Y") +
  scale_color_manual(values= c("1"= "darkred", "0"="red"), labels= c("Other Source", "Friends")) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


dev.off()


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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


dev.off()

# Ensure 'month' is converted to a Date format
df_feature$month <- as.Date(paste0(df_feature$month, "-01"), format = "%Y-%m-%d")
df_feature$year <- format(df_feature$month, "%Y")


df_figure6 <- df_feature %>%
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

write_xlsx(df_figure6, file.path(dir_output, "cleaned_data.xlsx"))


# Summarise total views per month and feature
df_figure7 <- df_unique %>%
  group_by(month, post_type_content_type, is_friend) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

# Calculate total views for each month
df_totalviews_month <- df_figure7 %>%
  group_by(month, post_type_content_type) %>%
  summarise(monthly_total = sum(Totalviews))

# Merge total views back to the original data and calculate share
df_figure7 <- df_figure7 %>%
  left_join(df_totalviews_month, by = c("month", "post_type_content_type")) %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

# Ensure 'month' is converted to a Date format

df_figure7 <- df_figure7 %>% filter(is_friend == 1)

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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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

png(filename = file.path(dir_output, "Figure 8.png"), width = 800, height = 600)

ggplot(df_figure8, aes(x= month, y=share_of_views, color=factor(post_origin), group=post_origin)) +
  geom_line() +
  geom_point() +
  labs(title="Content views of content produced by Facebook “friends” as a proportion of total content views ",
       x= "Date",
       y="proportion of views",
       color="Origin:") +
  scale_x_date( date_breaks = "3 month",           # Show dates with a 1-month gap
                date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 10))  


dev.off()


