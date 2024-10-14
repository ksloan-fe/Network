if (.Platform$OS.type == "unix") {
  z_dir <- "~/Z/"
} else {
  z_dir <- "Z:/Projects/"
}


proj_dir <- paste0(z_dir, "Projects-24/P2409-6305/Work/3.ANALYSIS/5. Interoperability/Network effects analysis/Kerry Output")

dir_raw <- paste0(proj_dir, "/raw_data")
dir_processed <- paste0(proj_dir, "/Processed Data")
dir_output <- paste0(proj_dir, "/outputs")

rm(z_dir, proj_dir)
