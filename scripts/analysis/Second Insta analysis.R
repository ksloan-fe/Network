
insta_2021 <- read.csv("C:/Users/kerry.sloan/Documents/Projects/Meta/daiquery-IG 2021-2024-12-08 4_30pm.csv")

insta_2022 <- read.csv("C:/Users/kerry.sloan/Documents/Projects/Meta/daiquery-IG 2022-2024-12-08 4_32pm.csv")

insta_2023 <- read.csv("C:/Users/kerry.sloan/Documents/Projects/Meta/daiquery-IG 2023-2024-12-08 4_31pm.csv")

insta_2024 <- read.csv("C:/Users/kerry.sloan/Documents/Projects/Meta/daiquery-IG 2024-2024-12-08 4_02pm.csv")

df_insta_appended <- rbind(insta_2021,insta_2022,insta_2023, insta_2024)

df_insta_appended <- df_insta_appended |>
  mutate(Surface = case_when(
    product_type == "STORY" ~ "Story",
    product_type == "FEED" ~ "Feed",
    product_type == "LIVE" ~ "live",
    product_type == "CLIPS" ~ "clips",
    product_type == "IGTV" ~ "IGTV",
    product_type == "AD" ~ "ads",
    TRUE ~ "Other"
  )) |> 
  mutate(month = ymd(paste(month, "01", sep = "-"))) |>
  mutate(is_reciprocal_follow = replace_na(is_reciprocal_follow, 0)) |>
  mutate(is_ig_creator= replace_na(is_ig_creator, 0)) |>
  mutate(author_is_ig_creator  = replace_na(author_is_ig_creator, 0)) |>
  mutate(author_is_business_profile = replace_na(author_is_business_profile, 0)) |>
  mutate(is_friends_and_family = replace_na(is_friends_and_family, 0)) |>
  mutate(is_business_profile = replace_na(is_business_profile, 0)) |>
  mutate(is_following= replace_na(is_following, 0)) 


         
  

df_insta_table <- df_insta_appended |> filter(is_ig_creator == 0, is_business_profile==0) |>
  group_by(month, Surface) |> 
  summarize(
    impressions_by_viewers = sum(post_impressions, na.rm=T), 
    impressions_follow_author_follow_viewer_not_buis_creator = sum(post_impressions[is_reciprocal_follow == 1 & is_following == 1 & author_is_ig_creator == 0 & author_is_business_profile == 0], na.rm=T), 
    impressions_follow_author_is_creator_is_buis = sum(post_impressions[is_following == 1 & (author_is_ig_creator == 1 | author_is_business_profile == 1)], na.rm=T), 
    impressions_not_follow_author_is_creator_is_buis = sum(post_impressions[is_following == 0 & (author_is_ig_creator == 1 | author_is_business_profile == 1)], na.rm=T), 
    impressions_not_follow_author_follow_viewer_not_buis_creator = sum(post_impressions[is_reciprocal_follow == 0 & is_following == 1 & author_is_ig_creator == 0 & author_is_business_profile == 0], na.rm=T), 
    impressions_follow_author_not_follow_viewer_not_buis_creator = sum(post_impressions[is_reciprocal_follow == 1 & is_following == 0 & author_is_ig_creator == 0 & author_is_business_profile == 0], na.rm=T),
    impressions_not_follow_author_not_follow_viewer_not_buis_creator = sum(post_impressions[is_reciprocal_follow == 0 & is_following == 0 & author_is_ig_creator == 0 & author_is_business_profile == 0], na.rm=T),
    dwell_by_viewers = sum(dwell_time, na.rm=T), 
    dwell_follow_author_follow_viewer_not_buis_creator = sum(dwell_time[is_reciprocal_follow == 1 & is_following == 1 & author_is_ig_creator == 0 & author_is_business_profile == 0], na.rm=T), 
    dwell_follow_author_is_creator_is_buis = sum(dwell_time[is_following == 1 & (author_is_ig_creator == 1 | author_is_business_profile == 1)], na.rm=T), 
    dwell_not_follow_author_is_creator_is_buis = sum(dwell_time[is_following == 0 & (author_is_ig_creator == 1 | author_is_business_profile == 1)], na.rm=T), 
    dwell_follow_not_author_follow_viewer_not_buis_creator = sum(dwell_time[is_reciprocal_follow == 0 & is_following == 1 & author_is_ig_creator == 0 & author_is_business_profile == 0], na.rm=T), 
    dwell_follow_author_not_follow_viewer_not_buis_creator = sum(dwell_time[is_reciprocal_follow == 1 & is_following == 0 & author_is_ig_creator == 0 & author_is_business_profile == 0], na.rm=T), 
    dwell_not_follow_author_not_follow_viewer_not_buis_creator = sum(dwell_time[is_reciprocal_follow == 0 & is_following == 0 & author_is_ig_creator == 0 & author_is_business_profile == 0], na.rm=T) 
    
  )




# Export the dataset as an Excel file
write_xlsx(df_insta_table, path = "C:/Users/kerry.sloan/Documents/Projects/Meta/insta_summary_table.xlsx")

#check on missing categories 

df_filtered_categories <- df_insta_appended |> filter(is_ig_creator == 0, is_business_profile==0) |>
  filter(
    is_reciprocal_follow == 1 & is_following == 1 & author_is_ig_creator == 0 & author_is_business_profile == 0 |
      is_following == 1 & (author_is_ig_creator == 1 | author_is_business_profile == 1) |
      is_following == 0 & (author_is_ig_creator == 1 | author_is_business_profile == 1) |
      is_reciprocal_follow == 0 & is_following == 1 & author_is_ig_creator == 0 & author_is_business_profile == 0 |
      is_reciprocal_follow == 1 & is_following == 0 & author_is_ig_creator == 0 & author_is_business_profile == 0 | 
      is_reciprocal_follow == 0 & is_following == 0 & author_is_ig_creator == 0 & author_is_business_profile == 0
  
  )


df_remaining <- df_insta_appended |> filter(is_ig_creator == 0, is_business_profile==0) |>
  anti_join(df_filtered_categories, by = names(df_insta_appended))

# Summarize remaining data
remaining_summary <- df_remaining |> 
  summarize(
    total_remaining_impressions = sum(post_impressions, na.rm = TRUE),
    total_remaining_dwell_time = sum(dwell_time, na.rm = TRUE),
    total_rows_remaining = n()
  )

# View remaining rows to inspect
print(df_remaining)
print(remaining_summary)