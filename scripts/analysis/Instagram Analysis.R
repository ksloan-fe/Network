file_path_insta <- file.path(dir_raw, "daiquery-IG impressions-2024-11-03 2_08am.csv")
df_insta <- read.csv(file_path_insta)
# Custom theme
custom_theme <- theme(
  axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
  axis.text.y = element_text(size = 14),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  panel.grid.major.y = element_line(color = "lightgray"),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black", size = 0.5),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 15),
  legend.key.size = unit(1.5, "lines")
)
df_feature_all <- df_insta |>
  mutate(Surface = case_when(
    product_type == "STORY" ~ "Story",
    product_type == "FEED" ~ "Feed",
    product_type == "LIVE" ~ "live",
    product_type == "CLIPS" ~ "clips",
    product_type == "IGTV" ~ "IGTV",
    product_type == "AD" ~ "ads",
    TRUE ~ "Other"
  ))
df_organic <- df_feature_all |>
  filter(Surface != "ads") |>
  mutate(month = ymd(paste(month, "01", sep = "-"))) |>
  mutate(is_reciprocal_follow = replace_na(is_reciprocal_follow, 0))
#Share of reciprocal follows
#Impressions
# Graph
df_rf_impressions <- df_organic |>
  group_by(month, is_reciprocal_follow, Surface) |>
  summarise(
    Totalviews = sum(post_impressions), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month, Surface) |>
  mutate(
    ShareViews_month = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month, is_reciprocal_follow, Surface) |>
  filter(is_reciprocal_follow== 1)
graph_1 <- df_rf_impressions |>
  ggplot(aes(x = month, y =ShareViews_month, colour = Surface)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Share of instagram views originating from reciprocal follows by product",
       x = " ",
       y = "Share of views (%)",
       colour = "Surface") +  # Legend title
  scale_x_date( date_labels = "%b %Y") +
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme
# Print the plot
print(graph_1)
df_product_share_impressions <- df_organic |>
  group_by(month, Surface) |>
  summarise(
    Totalviews = sum(post_impressions), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month) |>
  mutate(
    ShareViews_month_total = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month_total, Surface)
df_rf_multiple_impressions <- left_join(df_product_share_impressions, df_rf_impressions, by= c("month", "Surface")) |>
  mutate(multiple= ShareViews_month_total* ShareViews_month)
#Dwell time
# Graph
df_rf_dwell <- df_organic |>
  group_by(month, is_reciprocal_follow, Surface) |>
  summarise(
    Totalviews = sum(dwell_time), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month, Surface) |>
  mutate(
    ShareViews_month = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month, is_reciprocal_follow, Surface) |>
  filter(is_reciprocal_follow== 1)
graph_2 <- df_rf_dwell |>
  ggplot(aes(x = month, y =ShareViews_month, colour = Surface)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Share of instagram Dwell Time originating from reciprocal follows by product",
       x = " ",
       y = "Share of time (%)",
       colour = "Surface") +  # Legend title
  scale_x_date( date_labels = "%b %Y") +
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme
# Print the plot
print(graph_2)
df_product_share_dwell <- df_organic |>
  group_by(month, Surface) |>
  summarise(
    Totalviews = sum(dwell_time), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month) |>
  mutate(
    ShareViews_month_total = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month_total, Surface)
df_rf_dwell_multiple <- left_join(df_product_share_dwell, df_rf_dwell, by= c("month", "Surface")) |>
  mutate(multiple= ShareViews_month_total* ShareViews_month)
#Share of family or friends
#Impressions
# Graph
df_fam_impressions <- df_organic |>
  group_by(month, is_friends_and_family, Surface) |>
  summarise(
    Totalviews = sum(post_impressions), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month, Surface) |>
  mutate(
    ShareViews_month = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month, is_friends_and_family, Surface) |>
  filter(is_friends_and_family== 1)
graph_1 <- df_fam_impressions |>
  ggplot(aes(x = month, y =ShareViews_month, colour = Surface)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Share of instagram views originating from family and friends by product",
       x = " ",
       y = "Share of views (%)",
       colour = "Surface") +  # Legend title
  scale_x_date( date_labels = "%b %Y") +
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme
# Print the plot
print(graph_1)
df_fam_multiple_impressions <- left_join(df_product_share_impressions, df_fam_impressions, by= c("month", "Surface")) |>
  mutate(multiple= ShareViews_month_total* ShareViews_month)
#Dwell time
# Graph
df_fam_dwell <- df_organic |>
  group_by(month, is_friends_and_family, Surface) |>
  summarise(
    Totalviews = sum(dwell_time), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month, Surface) |>
  mutate(
    ShareViews_month = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month, is_friends_and_family, Surface) |>
  filter(is_friends_and_family== 1)
graph_2 <- df_fam_dwell |>
  ggplot(aes(x = month, y =ShareViews_month, colour = Surface)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Share of instagram Dwell Time originating from family and friends by product",
       x = " ",
       y = "Share of time (%)",
       colour = "Surface") +  # Legend title
  scale_x_date( date_labels = "%b %Y") +
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme
# Print the plot
print(graph_2)
df_fam_dwell_multiple <- left_join(df_product_share_dwell, df_fam_dwell, by= c("month", "Surface")) |>
  mutate(multiple= ShareViews_month_total* ShareViews_month)
#Share of bffs
#Impressions
# Graph
df_bff_impressions <- df_organic |>
  group_by(month, is_bff_close_friend, Surface) |>
  summarise(
    Totalviews = sum(post_impressions), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month, Surface) |>
  mutate(
    ShareViews_month = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month, is_bff_close_friend, Surface) |>
  filter(is_bff_close_friend== 1)
graph_1 <- df_bff_impressions |>
  ggplot(aes(x = month, y =ShareViews_month, colour = Surface)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Share of instagram views originating from close friends by product",
       x = " ",
       y = "Share of views (%)",
       colour = "Surface") +  # Legend title
  scale_x_date( date_labels = "%b %Y") +
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme
# Print the plot
print(graph_1)
df_bff_multiple_impressions <- left_join(df_product_share_impressions, df_bff_impressions, by= c("month", "Surface")) |>
  mutate(multiple= ShareViews_month_total* ShareViews_month)
#Dwell time
# Graph
df_bff_dwell <- df_organic |>
  group_by(month, is_bff_close_friend, Surface) |>
  summarise(
    Totalviews = sum(dwell_time), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month, Surface) |>
  mutate(
    ShareViews_month = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month, is_bff_close_friend, Surface) |>
  filter(is_bff_close_friend== 1)
graph_2 <- df_bff_dwell |>
  ggplot(aes(x = month, y =ShareViews_month, colour = Surface)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Share of instagram Dwell Time originating from close friends by product",
       x = " ",
       y = "Share of time (%)",
       colour = "Surface") +  # Legend title
  scale_x_date( date_labels = "%b %Y") +
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme
# Print the plot
print(graph_2)
df_dwell_multiple <- left_join(df_product_share_dwell, df_bff_dwell, by= c("month", "Surface")) |>
  mutate(multiple= ShareViews_month_total* ShareViews_month)


#Not split by surface 

df_bff_impressions <- df_organic |>
  group_by(month, is_bff_close_friend) |>
  summarise(
    Totalviews = sum(post_impressions), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month) |>
  mutate(
    ShareViews_month = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month, is_bff_close_friend) |>
  filter(is_bff_close_friend== 1)

graph_1 <- df_bff_impressions |>
  ggplot(aes(x = month, y =ShareViews_month)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Share of instagram views originating from close friends",
       x = " ",
       y = "Share of views (%)") +  # Legend title
  scale_x_date( date_labels = "%b %Y") +
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme
# Print the plot
print(graph_1)

df_bff_multiple_impressions <- left_join(df_product_share_impressions, df_bff_impressions, by= c("month")) |>
  mutate(multiple= ShareViews_month_total* ShareViews_month)

#Dwell time
# Graph
df_bff_dwell <- df_organic |>
  group_by(month, is_bff_close_friend) |>
  summarise(
    Totalviews = sum(dwell_time), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month) |>
  mutate(
    ShareViews_month = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month, is_bff_close_friend) |>
  filter(is_bff_close_friend== 1)

graph_2 <- df_bff_dwell |>
  ggplot(aes(x = month, y =ShareViews_month)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Share of instagram Dwell Time originating from close friends",
       x = " ",
       y = "Share of time (%)",) +  # Legend title
  scale_x_date( date_labels = "%b %Y") +
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme
# Print the plot
print(graph_2)

df_dwell_multiple <- left_join(df_product_share_dwell, df_bff_dwell, by= c("month")) |>
  mutate(multiple= ShareViews_month_total* ShareViews_month)



#Share of reciprocal follows
#Impressions
# Graph

df_rf_impressions <- df_organic |>
  group_by(month, is_reciprocal_follow) |>
  summarise(
    Totalviews = sum(post_impressions), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month) |>
  mutate(
    ShareViews_month = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month, is_reciprocal_follow) |>
  filter(is_reciprocal_follow== 1)

graph_1 <- df_rf_impressions |>
  ggplot(aes(x = month, y =ShareViews_month)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Share of instagram views originating from reciprocal follows",
       x = " ",
       y = "Share of views (%)",) +  # Legend title
  scale_x_date( date_labels = "%b %Y") +
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme
# Print the plot
print(graph_1)

df_product_share_impressions <- df_organic |>
  group_by(month) |>
  summarise(
    Totalviews = sum(post_impressions), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month) |>
  mutate(
    ShareViews_month_total = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month_total)

df_rf_multiple_impressions <- left_join(df_product_share_impressions, df_rf_impressions, by= c("month")) |>
  mutate(multiple= ShareViews_month_total* ShareViews_month)
#Dwell time
# Graph
df_rf_dwell <- df_organic |>
  group_by(month, is_reciprocal_follow) |>
  summarise(
    Totalviews = sum(dwell_time), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month) |>
  mutate(
    ShareViews_month = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month, is_reciprocal_follow) |>
  filter(is_reciprocal_follow== 1)

graph_2 <- df_rf_dwell |>
  ggplot(aes(x = month, y =ShareViews_month)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Share of instagram Dwell Time originating from reciprocal follows",
       x = " ",
       y = "Share of time (%)") +  # Legend title
  scale_x_date( date_labels = "%b %Y") +
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme
# Print the plot
print(graph_2)
df_product_share_dwell <- df_organic |>
  group_by(month) |>
  summarise(
    Totalviews = sum(dwell_time), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month) |>
  mutate(
    ShareViews_month_total = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month_total)

df_rf_dwell_multiple <- left_join(df_product_share_dwell, df_rf_dwell, by= c("month")) |>
  mutate(multiple= ShareViews_month_total* ShareViews_month)
#Share of family or friends
#Impressions
# Graph
df_fam_impressions <- df_organic |>
  group_by(month, is_friends_and_family) |>
  summarise(
    Totalviews = sum(post_impressions), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month) |>
  mutate(
    ShareViews_month = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month, is_friends_and_family) |>
  filter(is_friends_and_family== 1)
graph_1 <- df_fam_impressions |>
  ggplot(aes(x = month, y =ShareViews_month)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Share of instagram views originating from family and friends",
       x = " ",
       y = "Share of views (%)") +  # Legend title
  scale_x_date( date_labels = "%b %Y") +
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme
# Print the plot
print(graph_1)

df_fam_multiple_impressions <- left_join(df_product_share_impressions, df_fam_impressions, by= c("month")) |>
  mutate(multiple= ShareViews_month_total* ShareViews_month)
#Dwell time
# Graph
df_fam_dwell <- df_organic |>
  group_by(month, is_friends_and_family) |>
  summarise(
    Totalviews = sum(dwell_time), na.rm = TRUE,
    .groups = "drop") |>
  group_by(month) |>
  mutate(
    ShareViews_month = 100 * (Totalviews / sum(Totalviews))) |>
  ungroup() |>
  select(month, ShareViews_month, is_friends_and_family) |>
  filter(is_friends_and_family== 1)
graph_2 <- df_fam_dwell |>
  ggplot(aes(x = month, y =ShareViews_month)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Share of instagram Dwell Time originating from family and friends",
       x = " ",
       y = "Share of time (%)") +  # Legend title
  scale_x_date( date_labels = "%b %Y") +
  theme_minimal() +
  expand_limits(y = 0) +
  custom_theme  # Apply custom theme
# Print the plot
print(graph_2)
df_fam_dwell_multiple <- left_join(df_product_share_dwell, df_fam_dwell, by= c("month")) 
  







