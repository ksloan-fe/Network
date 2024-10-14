
# Construct the full file path using file.path()
file_path <- file.path(dir_raw, "daiquery-FB time spent (video) in UK-2024-10-07 10_33pm.xlsx")

# Read the Excel file
fb_data <- read_excel(file_path)

file_path_two <- file.path(dir_raw, "daiquery-IG time spent (video) in UK-2024-10-07 10_37pm.xlsx")

# Read the Excel file
insta_data <- read_excel(file_path_two)


fb_data$month <- as.numeric(fb_data$month)
fb_data$year <- as.numeric(fb_data$year)

fb_data <- fb_data[grepl("overall_blue", fb_data$interface, ignore.case = TRUE), ]


fb_data$date <- as.Date(paste(fb_data$year, fb_data$month, "01", sep = "-"), format = "%Y-%m-%d")

# Select only the numeric columns you want to include in the time series
numeric_columns <- fb_data[, c("ts_total", "video_ts_total")]

# Create a time series object
fb_ts_data <- ts(numeric_columns, start = c(2015, 1), frequency = 12)

ratio <- fb_ts_data[, "video_ts_total"] / fb_ts_data[, "ts_total"]
fb_ts_data <- cbind(fb_ts_data, ratio)


# Export plot as PNG
png(filename = file.path(dir_output, "facebook_video_time_series_plot.png"), width = 800, height = 600)

# Plot only the 'video_ts_total' column from the time series object
plot(fb_ts_data[, "ratio"], type = "o",          # Both line and points
     col = "darkred",       # Line color
     pch = 1,           # Point type (filled circle)
     cex = 0.5,          # Size of the points
     lwd = 2, main = "Time spent watching videos on FaceBook", ylab = "Proportion of total time spent watching videos", xlab = "Date")
dev.off()

insta_data$month <- as.Date(insta_data$month, format = "%Y-%B-%d")

numeric_columns <- insta_data[, c("ts_total_in_days", "video_ts_total_in_days")]

# Create a time series object
insta_ts_data <- ts(numeric_columns, start = c(2019, 1), frequency = 12)

ratio_insta <- insta_ts_data[, "video_ts_total_in_days"] / insta_ts_data[, "ts_total_in_days"]
insta_ts_data <- cbind(insta_ts_data, ratio_insta)


# Export plot as PNG
png(filename = file.path(dir_output, "Insta_video_time_series_plot.png"), width = 800, height = 600)

# Plot only the 'video_ts_total' column from the time series object
plot(insta_ts_data[, "ratio_insta"], type = "o",          # Both line and points
     col = "darkred",       # Line color
     pch = 1,           # Point type (filled circle)
     cex = 0.5,          # Size of the points
     lwd = 2, main = "Time spent watching videos on Instagram", ylab = "Proportion of total time spent watching videos", xlab = "Date")

dev.off()

# Export plot as PNG
png(filename = file.path(dir_output, "insta_facebook_video_time_series_plot.png"), width = 800, height = 600)

# Plot only the 'video_ts_total' column from the time series object
plot(fb_ts_data[, "ratio"], type = "o",          # Both line and points
     col = "darkred",       # Line color
     pch = 1,           # Point type (filled circle)
     cex = 0.5,          # Size of the points
     lwd = 2, main = "Time spent watching videos on Social Media Platforms", ylab = "Proportion of total time spent watching videos", xlab = "Date")
lines(insta_ts_data[, "ratio_insta"], type= "o", col= "red", pch = 1,           # Point type (filled circle)
      cex = 0.5,          # Size of the points
      lwd = 2 )

legend("topright", 
       legend = c("Facebook", "Instagram"), 
       col = c("darkred", "red"), 
       pch = c(16, 17), 
       lwd = 2)

dev.off()























