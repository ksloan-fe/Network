options(scipen = 999)

# Construct the full file path using file.path()
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


rm(df_ww_1,df_ww_2,df_ww_3,df_ww_4, file_path_US, file_path_ww_1,file_path_ww_2,file_path_ww_3,file_path_ww_4)

df_figure_8 <- df_ww |>
  bind_rows(df_us) |>
  as_tibble() |>
  filter(post_origin!="") |>
  mutate(month = ymd(paste(month,01,sep="-"))) |>
  filter(month>=ymd("2022-04-01")) |>
  mutate(is_friend = replace_na(is_friend, 0)) |>
  mutate(is_organic = is.na(is_ad) & is.na(is_eqp) & is.na(is_qp)) |>
  group_by(country, month, post_origin, is_friend) |>
  summarise(total_views_by_origin  = sum(no_of_vpvs, na.rm = TRUE), .groups = "drop") |>
  group_by(country, month) |>
  mutate(
    monthly_share = 100*total_views_by_origin / sum(total_views_by_origin, na.rm = TRUE)
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


