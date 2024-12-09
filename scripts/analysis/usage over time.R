file_path <- file.path(dir_raw,
                       "daiquery-FB time spent (video) in UK-2024-10-07 10_33pm.xlsx")
fb_data <- read_excel(file_path)

file_path_two <- file.path(dir_raw,
                           "daiquery-IG time spent (video) in UK-2024-10-07 10_37pm.xlsx")
insta_data <- read_excel(file_path_two) |>
  mutate(month = ymd(month))


fb_data |>
  rename(
    ts_total_in_days = ts_total,
    video_ts_total_in_days = video_ts_total,
    ts_notes = `...6`
  ) |>
  mutate(product = "Facebook") |>
  mutate(month = ymd(paste(year, month, "1", sep = "-"))) |>
  select(-year) |>
  bind_rows(insta_data) |>
  filter(interface %in% c("overall_blue", NA)) |>
  filter(month >= as.Date("2015-01-01")) |>
  mutate(ratio = 100 * (video_ts_total_in_days / ts_total_in_days)) |>
  ggplot(aes(month, ratio, colour = product)) +
  geom_line() +
  geom_point(size = 1) +
  theme_minimal() +
  scale_colour_manual(values = c("darkred", "red")) +
  labs(x = "", y = "Proportion of total time spent watching videos")
