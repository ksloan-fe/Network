if (.Platform$OS.type == "unix") {
  z_dir <- "~/Z/"
} else {
  z_dir <- "Z:/Projects/"
}


proj_dir <- paste0(z_dir, "Projects-XX/PXX-XXXX/Work")

dir_raw <- paste0(proj_dir, "/raw_data")
dir_processed <- paste0(proj_dir, "/processed_data")
dir_output <- paste0(proj_dir, "/outputs")

rm(z_dir, proj_dir)
