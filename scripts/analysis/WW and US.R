options(scipen = 999)

# Construct the full file path using file.path()
file_path_US <- file.path(dir_raw, "daiquery-vpv breakdown in US (anon)-2024-11-03 3_33am.csv")

# Read the Excel file
df_us <- read.csv(file_path_US)

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

df_ww <- rbind(df_ww_1, df_ww_2, df_ww_3, df_ww_4)

df_us <- `daiquery.vpv.breakdown.in.US.(anon).2024.11.03.3_33am`

df_ww <- rbind(`daiquery.vpv.breakdown.WW.2023H1.(anon).2024.11.03.4_26am`, daiquery.vpv.breakdown.WW.2023H2.2024.11.07.2_37am, `daiquery.vpv.breakdown.WW.2024.(anon).2024.11.03.3_54am`)

df_all_ww <- df_ww
df_all_us <- df_us
df_all_ww$month <- as.Date(paste0(df_all_ww$month, "-01"), format = "%Y-%m-%d")
df_all_us$month <- as.Date(paste0(df_all_us$month, "-01"), format = "%Y-%m-%d")
df_all_us<- df_all_us %>% filter(month >= as.Date("2022-04-01"))
df_all_ww<- df_all_ww %>% filter(month >= as.Date("2022-04-01"))
df_all_ww<- df_all_ww %>% filter(month <= as.Date("2024-06-01"))
df_all_us<- df_all_us %>% filter(month <= as.Date("2024-06-01"))


df_unique_ww <- df_ww %>% filter(is.na(is_ad))
df_unique_ww <- df_unique_ww %>% filter(is.na(is_eqp))
df_unique_us <- df_us %>% filter(is.na(is_ad))
df_unique_us <- df_unique_us %>% filter(is.na(is_eqp))
df_unique_ww <- df_unique_ww %>% filter(is.na(is_qp))
df_unique_us <- df_unique_us %>% filter(is.na(is_qp))


df_unique_ww$month <- as.Date(paste0(df_unique_ww$month, "-01"), format = "%Y-%m-%d")
df_unique_us$month <- as.Date(paste0(df_unique_us$month, "-01"), format = "%Y-%m-%d")
df_unique_us<- df_unique_us %>% filter(month >= as.Date("2022-04-01"))
df_unique_ww<- df_unique_ww %>% filter(month >= as.Date("2022-04-01"))
df_unique_ww<- df_unique_ww %>% filter(month <= as.Date("2024-06-01"))
df_unique_us<- df_unique_us %>% filter(month <= as.Date("2024-06-01"))


df_unique_ww$is_friend[is.na(df_unique_ww$is_friend)] <- 0
df_unique_us$is_friend[is.na(df_unique_us$is_friend)] <- 0
df_all_ww$is_friend[is.na(df_all_ww$is_friend)] <- 0
df_all_us$is_friend[is.na(df_all_us$is_friend)] <- 0

df_unique_ww$year <- format(df_unique_ww$month, "%Y")
df_unique_us$year <- format(df_unique_us$month, "%Y")
df_all_us$year <- format(df_all_us$month, "%Y")
df_all_ww$year <- format(df_all_ww$month, "%Y")

#WW Tables 

df_figure8 <- df_all_ww %>%
  group_by(year, is_friend, post_origin) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_month <- df_figure8 %>%
  group_by(year) %>%
  summarise(monthly_total = sum(Totalviews))

df_figure8 <- df_figure8 %>%
  left_join(df_totalviews_month, by = "year") %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

df_figure8 <- df_figure8 %>% filter(is_friend == 1)
df_figure8 <- df_figure8 %>% select(-Totalviews, -monthly_total, -is_friend)

write_xlsx(df_figure8, file.path(dir_output, "Figure WW_all.xlsx"))

df_figure8 <- df_unique_ww %>%
  group_by(year, is_friend, post_origin) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_month <- df_figure8 %>%
  group_by(year) %>%
  summarise(monthly_total = sum(Totalviews))

df_figure8 <- df_figure8 %>%
  left_join(df_totalviews_month, by = "year") %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

df_figure8 <- df_figure8 %>% filter(is_friend == 1)
df_figure8 <- df_figure8 %>% select(-Totalviews, -monthly_total, -is_friend)

write_xlsx(df_figure8, file.path(dir_output, "Figure WW_organic.xlsx"))


#US Tables 

df_figure8 <- df_all_us %>%
  group_by(year, is_friend, post_origin) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_month <- df_figure8 %>%
  group_by(year) %>%
  summarise(monthly_total = sum(Totalviews))

df_figure8 <- df_figure8 %>%
  left_join(df_totalviews_month, by = "year") %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

df_figure8 <- df_figure8 %>% filter(is_friend == 1)
df_figure8 <- df_figure8 %>% select(-Totalviews, -monthly_total, -is_friend)

write_xlsx(df_figure8, file.path(dir_output, "Figure us_all.xlsx"))

df_figure8 <- df_unique_us %>%
  group_by(year, is_friend, post_origin) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_month <- df_figure8 %>%
  group_by(year) %>%
  summarise(monthly_total = sum(Totalviews))

df_figure8 <- df_figure8 %>%
  left_join(df_totalviews_month, by = "year") %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

df_figure8 <- df_figure8 %>% filter(is_friend == 1)
df_figure8 <- df_figure8 %>% select(-Totalviews, -monthly_total, -is_friend)

write_xlsx(df_figure8, file.path(dir_output, "Figure us_organic.xlsx"))

#US Tables total 

df_figure8 <- df_all_us %>%
  group_by(year, is_friend) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_month <- df_figure8 %>%
  group_by(year) %>%
  summarise(monthly_total = sum(Totalviews))

df_figure8 <- df_figure8 %>%
  left_join(df_totalviews_month, by = "year") %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

df_figure8 <- df_figure8 %>% filter(is_friend == 1)
df_figure8 <- df_figure8 %>% select(-Totalviews, -monthly_total, -is_friend)

write_xlsx(df_figure8, file.path(dir_output, "Figure us_all_total.xlsx"))

df_figure8 <- df_unique_us %>%
  group_by(year, is_friend) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_month <- df_figure8 %>%
  group_by(year) %>%
  summarise(monthly_total = sum(Totalviews))

df_figure8 <- df_figure8 %>%
  left_join(df_totalviews_month, by = "year") %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

df_figure8 <- df_figure8 %>% filter(is_friend == 1)
df_figure8 <- df_figure8 %>% select(-Totalviews, -monthly_total, -is_friend)

write_xlsx(df_figure8, file.path(dir_output, "Figure us_organic_total.xlsx"))

#WW Tables Total 

df_figure8 <- df_all_ww %>%
  group_by(year, is_friend) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_month <- df_figure8 %>%
  group_by(year) %>%
  summarise(monthly_total = sum(Totalviews))

df_figure8 <- df_figure8 %>%
  left_join(df_totalviews_month, by = "year") %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

df_figure8 <- df_figure8 %>% filter(is_friend == 1)
df_figure8 <- df_figure8 %>% select(-Totalviews, -monthly_total, -is_friend)


df_figure8 <- df_unique_ww %>%
  group_by(year, is_friend) %>%
  summarise(Totalviews = sum(no_of_vpvs, na.rm = TRUE)) %>%
  ungroup()

df_totalviews_month <- df_figure8 %>%
  group_by(year) %>%
  summarise(monthly_total = sum(Totalviews))

df_figure8 <- df_figure8 %>%
  left_join(df_totalviews_month, by = "year") %>%
  mutate(share_of_views = Totalviews / monthly_total) %>%
  ungroup()

df_figure8 <- df_figure8 %>% filter(is_friend == 1)
df_figure8 <- df_figure8 %>% select(-Totalviews, -monthly_total, -is_friend)
















