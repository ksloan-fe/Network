
file_path <- file.path(dir_raw, "daiquery-FB time spent (video) in UK-2024-10-07 10_33pm.xlsx")
fb_data <- read_excel(file_path)
file_path_two <- file.path(dir_raw, "daiquery-IG time spent (video) in UK-2024-10-07 10_37pm.xlsx")
insta_data <- read_excel(file_path_two)
fb_data$month <- as.numeric(fb_data$month)
fb_data$year <- as.numeric(fb_data$year)

#Exclude Messenger 
fb_data <-fb_data %>%filter(interface== "overall_blue")
fb_data$date <- as.Date(paste(fb_data$year, fb_data$month, "01", sep = "-"), format = "%Y-%m-%d")
fb_data <- fb_data %>% filter(date >= as.Date("2015-01-01"))

#Create ratio 

numeric_columns <- fb_data[, c("ts_total", "video_ts_total")]
fb_ts_data <- ts(numeric_columns, start = c(2015, 1), frequency = 12)
ratio <- fb_ts_data[, "video_ts_total"] / fb_ts_data[, "ts_total"]
fb_ts_data <- cbind(fb_ts_data, ratio)

#Insta Data 
insta_data$month <- as.Date(insta_data$month, format = "%Y-%B-%d")
insta_data <- insta_data %>% filter(month >= as.Date("2019-01-01"))
numeric_columns <- insta_data[, c("ts_total_in_days", "video_ts_total_in_days")]

# Create a time series object and ratio 
insta_ts_data <- ts(numeric_columns, start = c(2019, 1), frequency = 12)
ratio_insta <- insta_ts_data[, "video_ts_total_in_days"] / insta_ts_data[, "ts_total_in_days"]
insta_ts_data <- cbind(insta_ts_data, ratio_insta)

png(filename = file.path(dir_output, "facebook_video_time_series_plot.png"), width = 800, height = 600)

plot(fb_ts_data[, "ratio"], type = "o",          
     col = "darkred",      
     pch = 1,           
     cex = 0.5,         
     lwd = 2, main = "Time spent watching videos on FaceBook", ylab = "Proportion of total time spent watching videos", xlab = "Date",
     ylim = c(0, max(fb_ts_data[, "ratio"], na.rm = TRUE)))
dev.off()




png(filename = file.path(dir_output, "Insta_video_time_series_plot.png"), width = 800, height = 600)

plot(insta_ts_data[, "ratio_insta"], type = "o",          
     col = "darkred",       
     pch = 1,           
     cex = 0.5,         
     lwd = 2, main = "Time spent watching videos on Instagram", ylab = "Proportion of total time spent watching videos", xlab = "Date",
     ylim = c(0, max(insta_ts_data[, "ratio_insta"], na.rm = TRUE)))

dev.off()

png(filename = file.path(dir_output, "insta_facebook_video_time_series_plot.png"), width = 800, height = 600)

plot( fb_ts_data[, "ratio"], type = "o",          
     col = "darkred",       
     pch = 1,           
     cex = 0.5,          
     lwd = 2, 
     main = "Time spent watching videos on Social Media Platforms", 
     ylab = "Proportion of total time spent watching videos", 
     ylim = c(0, max(c(fb_ts_data[, "ratio"], insta_ts_data[, "ratio_insta"]), na.rm = TRUE)))
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")
lines(insta_ts_data[, "ratio_insta"], type= "o", col= "red", pch = 1,          
      cex = 0.5,          
      lwd = 2 )

legend("topleft", 
       legend = c("Facebook", "Instagram"), 
       col = c("darkred", "red"), 
       pch = c(1, 1), 
       lwd = 2)

dev.off()























