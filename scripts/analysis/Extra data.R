options(scipen = 999)

# Custom theme
custom_theme <- theme(
  axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate x-axis labels
  axis.text.y = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  panel.grid.major.y = element_line(color = "lightgray"),  # Horizontal grid lines only
  panel.grid.major.x = element_blank(),  # No vertical grid lines
  panel.grid.minor = element_blank(),  # Remove minor grid lines
  axis.line = element_line(color = "black", size = 0.5),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 15),
  legend.key.size = unit(1.5, "lines")  # Increase legend key size
)
  

theme_set(custom_theme)



file_path_2024 <- file.path(dir_raw, "daiquery-vpv breakdown in UK (anon)-2024-11-02 3_50pm.csv")

df_2024 <- read.csv(file_path_2024)

df_all <- df_2024 |>
  as_tibble() |>
  mutate(month = ymd(paste(month, "01", sep = "-"))) |>
  filter(month >= ymd("2023-10-01") & month <= as.Date("2024-06-01")) |>
  mutate(is_friend = replace_na(is_friend, 0)) |>
  mutate(is_organic = is.na(is_ad) & is.na(is_eqp) & is.na(is_qp))

df_unique <- df_all |>
  filter(is_organic)

#Slide 26

# Graph
df_figure_1 <- df_all |>
  group_by(month, is_friend) |>
  summarise(
    Totalviews = sum(cululative_vpv_duration, na.rm = TRUE),
    TotalOrganicViews = sum(cululative_vpv_duration*is_organic, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(month) |>
  mutate(
    ShareViews_month = 100 * Totalviews / sum(Totalviews),
    ShareOrganicViews_month = 100 * TotalOrganicViews / sum(TotalOrganicViews)
  ) |>
  ungroup() |>
  filter(is_friend == 1) |>
  select(month, ShareViews_month, ShareOrganicViews_month) |>
  gather(key=measure, value=views, -month)


graph_1 <- df_figure_1 |>
  ggplot(aes(x = month, y = views, colour = measure)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Share of Dwell Time Originating from Friends",
       x = " ",
       y = "Share of Dwell Time (%)",
       colour = "Measure") +  # Legend title
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_color_manual(values = c("darkred", "red"), 
                     labels = c("Organic Content", "All Content")) +  # Add custom labels to color
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme

# Print the plot
print(graph_1)

# Table
# FIXME: typo in the slides, 22.4 rather than 22.5
df_table_1a <- df_figure_1 |>
  mutate(year=year(month)) |>
  group_by(year, measure) |>
  summarise(mean_views = mean(views)) |>
  spread(key=year, value=mean_views)

# FIXME could write out this version instead?
print(df_table_1a)

#Slide 27

df_figure_2 <- df_all |>
  group_by(month, is_friend, post_origin) |>
  summarise(
    Totalviews = sum(cululative_vpv_duration, na.rm = TRUE),
    TotalOrganicviews = sum(cululative_vpv_duration*is_organic, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(month) |>
  mutate(
    monthly_share = 100 * Totalviews / sum(Totalviews),
    monthly_organic_share = 100 * TotalOrganicviews / sum(TotalOrganicviews),
    
  ) |>
  filter(is_friend == 1) |>
  mutate(
    total_share = sum(monthly_share),
    Total = sum(monthly_organic_share)) |>
  select(-Totalviews, -TotalOrganicviews) 

graph_2 <- df_figure_2 |>
  select(-monthly_share, -total_share) |>
  filter(post_origin != "") |>
  spread(key=post_origin, value=monthly_organic_share) |>
  gather(key=post_origin, value=monthly_organic_share, -month, -is_friend) |>
  ggplot(aes(month, monthly_organic_share, colour=post_origin))+
  geom_line()+
  geom_point()+
  labs(
    title = "Dwell Time of content produced by Facebook “friends” as a proportion of total Dwell Time ",
    y = "Proportion of Dwell Time",
    x = " ",
    color = " "
  )  + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme


graph_2

# Table

# FIXME: I'm getting different numbers here
# Weirdly, the 2022 numbers for organic only are correct. 
# Treatment is otherwise the same, why the difference?

df_figure_2 |>
  filter(month<ymd("2024-07-01")) |>
  mutate(year = year(month))|>
  group_by(post_origin, year) |>
  summarise(share = mean(monthly_share)) |>
  spread(key=post_origin, value=share) |>
  mutate(total = original + reshare)


df_figure_2 |>
  filter(month<ymd("2024-07-01")) |>
  mutate(year = year(month))|>
  group_by(post_origin, year) |>
  summarise(share = mean(monthly_organic_share)) |>
  spread(key=post_origin, value=share) |>
  mutate(total = original + reshare)

#Slide 28 

# Slide 13
# Table

df_feature_all <- df_all |>
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
    TRUE ~ "Other" # Default label if none match
  ))


df_feature <- df_feature_all |>
  filter(is_organic) |>
  mutate(year = year(month))

df_content <- df_feature %>%
  mutate(content_source = case_when(
    is_friend == 1 ~ "Friends",
    is_friend == 0 & post_author_profile_type %in% c("PAGE", "ADDITIONAL_PROFILE_PLUS", "PRIMARY_PROFILE_PLUS") ~ "Page",
    post_author_profile_type %in% c("INSTAGRAM_USER_V2", "INSTAGRAM_USER") ~ "Instagram users",
    is_friend == 0 & post_author_profile_type == "MAIN_PROFILE" ~ "Unconnected users",
    is_friend == 0 & post_author_profile_type == "SINGLE_OWNER_ADDITIONAL_PROFILE" ~ "Group Admin Profile",
    TRUE ~ "Other"
  ))


df_figure5 <- df_content |>
  filter(year==2024) |>
  group_by(content_source, feature) |>
  summarise(Totalviews = sum(cululative_vpv_duration, na.rm = TRUE), .groups ="drop") |>
  group_by(feature) |>
  mutate(year_total = sum(Totalviews)) |>
  mutate(share_of_views = 100*Totalviews / year_total) |>
  ungroup() 

df_fig5_maintable <- df_figure5 |>
  select(-Totalviews, -year_total) |>
  mutate(share_of_views = round(share_of_views,1)) |>
  spread(key=content_source, value=share_of_views)

print(df_fig5_maintable)

df_fig5_totals <- df_figure5 |>
  group_by(content_source) |>
  summarise(all_surfaces = 100*sum(Totalviews) / sum(year_total)) 

write_xlsx(df_fig5_maintable, file.path(dir_output, "Fig five dwell time.xlsx"))

print(df_fig5_totals)


#US and WW

file_path_US <- file.path(dir_raw, "daiquery-vpv breakdown in US (anon)-2024-11-03 3_33am.csv")

# Read the Excel file
df_us <- read.csv(file_path_US) |>
  mutate(country="US")

# Construct the full file path using file.path()
file_path_ww_2 <- file.path(dir_raw, "daiquery-vpv breakdown WW 2024 (anon)-2024-11-03 3_54am.csv")

# Read the Excel file
df_ww_2 <- read.csv(file_path_ww_2)

file_path_ww_3 <- file.path(dir_raw, "daiquery-vpv breakdown WW 2023H2-2024-11-07 2_37am.csv")

# Read the Excel file
df_ww_3 <- read.csv(file_path_ww_3)

file_path_ww_4 <- file.path(dir_raw, "daiquery-vpv breakdown WW 2022 (anon)-2024-11-03 3_52am.csv")

# Read the Excel file
df_ww_4 <- read.csv(file_path_ww_4)

file_path_ww_1 <- file.path(dir_raw, "daiquery-vpv breakdown WW 2023H1 (anon)-2024-11-03 4_26am.csv")

# Read the Excel file
df_ww_1 <- read.csv(file_path_ww_1)

df_ww <- rbind(df_ww_1, df_ww_2, df_ww_3, df_ww_4)|>
  mutate(country="WW")
df_ww <- df_ww %>% rename(cululative_vpv_duration=cumulative_vpv_duration)


rm(df_ww_1,df_ww_2,df_ww_3,df_ww_4, file_path_US, file_path_ww_1,file_path_ww_2,file_path_ww_3,file_path_ww_4)

df_figure_8 <- df_ww |>
  bind_rows(df_us) |>
  as_tibble() |>
  mutate(month = ymd(paste(month,01,sep="-"))) |>
  filter(month>=ymd("2023-10-01")) |>
  mutate(is_friend = replace_na(is_friend, 0)) |>
  mutate(is_organic = is.na(is_ad) & is.na(is_eqp) & is.na(is_qp)) |>
  group_by(country, month, post_origin, is_friend) |>
  summarise(total_views_by_origin  = sum(cululative_vpv_duration, na.rm = TRUE),
            TotalOrganicviews = sum(cululative_vpv_duration*is_organic, na.rm = TRUE), .groups = "drop") |>
  group_by(country, month) |>
  mutate(
    monthly_share = 100*total_views_by_origin / sum(total_views_by_origin, na.rm = TRUE),
    monthly_organic_share = 100 * TotalOrganicviews / sum(TotalOrganicviews)
  ) |>
  ungroup() |>
  filter(is_friend==1) |>
  mutate(year = year(month))|>
  group_by(country, post_origin, year) |>
  summarise(share = mean(monthly_share)) |>
  mutate(share = round(share, 1)) |>
  spread(key=post_origin, value=share) |>
  mutate(total = original + reshare)


print(df_figure_8)

df_figure_8_organic <- df_ww |>
  bind_rows(df_us) |>
  as_tibble() |>
  mutate(month = ymd(paste(month,01,sep="-"))) |>
  filter(month>=ymd("2023-10-01")) |>
  mutate(is_friend = replace_na(is_friend, 0)) |>
  mutate(is_organic = is.na(is_ad) & is.na(is_eqp) & is.na(is_qp)) |>
  group_by(country, month, post_origin, is_friend) |>
  summarise(total_views_by_origin  = sum(cululative_vpv_duration, na.rm = TRUE),
            TotalOrganicviews = sum(cululative_vpv_duration*is_organic, na.rm = TRUE), .groups = "drop") |>
  group_by(country, month) |>
  mutate(
    monthly_share = 100*total_views_by_origin / sum(total_views_by_origin, na.rm = TRUE),
    monthly_organic_share = 100 * TotalOrganicviews / sum(TotalOrganicviews)
  ) |>
  ungroup() |>
  filter(is_friend==1) |>
  mutate(year = year(month))|>
  group_by(country, post_origin, year) |>
  summarise(share = mean(monthly_organic_share)) |>
  mutate(share = round(share, 1)) |>
  spread(key=post_origin, value=share) |>
  mutate(total = original + reshare)


print(df_figure_8_organic)


#Dwell time by feature in 2024

dwell_byfreature <- df_feature %>% filter(year== 2024)

df_dwell <- dwell_byfreature %>%
  group_by(year, feature) %>%
  summarise(Totalviews = sum(cululative_vpv_duration, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_year_feature <- df_dwell %>%
  group_by(year) %>%
  summarise(year_total = sum(Totalviews))

df_dwell <- df_dwell %>%
  left_join(df_totalviews_year_feature, by = c("year")) %>%
  mutate(share_of_views = Totalviews / year_total) %>%
  ungroup()


df_dwell <- df_dwell %>% select(-Totalviews, -year_total)

dwell_byfreature <- df_feature_all %>% filter(year== 2024)

df_dwell <- dwell_byfreature %>%
  group_by(year, feature) %>%
  summarise(Totalviews = sum(cululative_vpv_duration, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_year_feature <- df_dwell %>%
  group_by(year) %>%
  summarise(year_total = sum(Totalviews))

df_dwell <- df_dwell %>%
  left_join(df_totalviews_year_feature, by = c("year")) %>%
  mutate(share_of_views = Totalviews / year_total) %>%
  ungroup()


df_dwell <- df_dwell %>% select(-Totalviews, -year_total)

dwell_byfreature <- df_feature %>% filter(year== 2024)

df_dwell <- dwell_byfreature %>%
  group_by(year, feature) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_year_feature <- df_dwell %>%
  group_by(year) %>%
  summarise(year_total = sum(Totalviews))

df_dwell <- df_dwell %>%
  left_join(df_totalviews_year_feature, by = c("year")) %>%
  mutate(share_of_views = Totalviews / year_total) %>%
  ungroup()


df_dwell <- df_dwell %>% select(-Totalviews, -year_total)

dwell_byfreature <- df_feature_all %>% filter(year== 2024)


df_dwell <- dwell_byfreature %>%
  group_by(year, feature) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_year_feature <- df_dwell %>%
  group_by(year) %>%
  summarise(year_total = sum(Totalviews))

df_dwell <- df_dwell %>%
  left_join(df_totalviews_year_feature, by = c("year")) %>%
  mutate(share_of_views = Totalviews / year_total) %>%
  ungroup()


df_dwell <- df_dwell %>% select(-Totalviews, -year_total)

#Feed friend Inventory


file_path_ff <- file.path(dir_raw, "daiquery-Feed Friend Inventory-2024-11-02 10_11pm.csv")

# Read the Excel file
df_ff <- read.csv(file_path_ff)

df_ff$ds <- as.Date(df_ff$ds, format = "%Y-%m-%d")
names(df_ff)

df_ff <- df_ff %>% filter(friend_feed_inventory_class_7d != "")

df_total <- df_ff %>%
  group_by(ds) %>%
  summarise(day_total = sum(no_of_users))


df_ff <- df_ff %>%
  left_join(df_total, by = c("ds")) %>%
  mutate(share_of_views = no_of_users / day_total) %>%
  ungroup()


png(filename = file.path(dir_output, "Friend Inventory.png"), width = 800, height = 600)
ggplot(df_ff, aes(x= ds, y=share_of_views, color=factor(friend_feed_inventory_class_7d), group=friend_feed_inventory_class_7d)) +
  geom_line(size=1) +
  labs(title="Share of categories of the Feed Friend Inventory over time",
       y="share",
       x= " ",
       color=" ") +
  scale_x_date( date_breaks = "7 days",           
                date_labels = "%d %b %Y") +
  scale_y_continuous(limits = c(0, 1),   breaks = seq(0, 1, by = 0.05)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.grid.major.y = element_line(color = "lightgray"),  
        panel.grid.major.x = element_blank(),                                         
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black", size = 0.5),
        legend.title = element_text(size = 14),    
        legend.text = element_text(size = 15),     
        legend.key.size = unit(1.5, "lines"))  +
  expand_limits(y = 0)

dev.off()



df_recent <- df_ff %>% filter(ds >= as.Date("2024-08-13"))


write_xlsx(df_recent, file.path(dir_output, "ff recent day.xlsx"))



