library(tidyverse)

# Define file paths
rds_folder <- "path/to/your/rds/files"
file_list <- list.files(rds_folder, pattern = "\\.rds$", full_names = TRUE)

process_ssm_layers <- function(file_path) {
  soil_data <- readRDS(file_path)
  loc_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Extract the water/physical data frame from your APSIM object
  # (Modify these paths depending on how your specific rds structures store tables)
  if ("Water" %in% names(soil_data)) {
    phys_df <- soil_data$Water
  } else {
    phys_df <- soil_data
  }
  
  # Map APSIM columns to SSM layout
  formatted_layers <- phys_df %>%
    mutate(
      `Layer#` = row_number(),
      DLYER    = Thickness, # Double check if SSM expects mm or cm
      SAT      = SAT,
      DUL      = DUL,
      LL       = LL15,
      ADRY     = AirDry,    # Use APSIM AirDry column if available
      iWL      = DUL,       # Initializing at Field Capacity as a baseline
      DRAINF   = 0.7,       # Default constant matching your template
      FG       = 0,         # Default constant matching your template
      BDL      = BD,        # Bulk Density
      NORG     = Carbon,    # Total Organic Carbon %
      FMIN     = 0.1,       # Default placeholder
      iNSOL    = NA         # Leave blank or calculate initial soil N if in data
    ) %>%
    select(`Layer#`, DLYER, SAT, DUL, LL, ADRY, iWL, DRAINF, FG, BDL, NORG, FMIN, iNSOL)
  
  # Create the structural metadata headers seen in the image
  header_info <- tibble(
    `Layer#` = c("<-- SoilRowNo", "Code", loc_name, "NLYER", as.character(nrow(formatted_layers))),
    DLYER    = c("", "Description:", "", "LDRAIN", "0"),
    SAT      = c("", "", "", "SALB", "0.13"),
    DUL      = c("", "", "", "U", "6"),
    LL       = c("", "", "", "CN2", "72")
  )
  
  # Bind metadata header with the actual tabular layer data
  # (Ensures a clean visual structure matching the spreadsheet layout)
  final_output <- bind_rows(header_info, formatted_layers %>% mutate(across(everything(), as.character)))
  
  return(list(location = loc_name, data = final_output))
}

# Run the pipeline across your location files
all_soils_processed <- map(file_list, ~tryCatch(process_ssm_layers(.x), error = function(e) return(NULL)))

# Example: Write out Albany_MO to a CSV matching your layout exactly
# you can easily write a loop to save all files separately
albany_data <- all_soils_processed[[1]]$data
write_csv(albany_data, paste0(all_soils_processed[[1]]$location, "_ssm_format.csv"), na = "")