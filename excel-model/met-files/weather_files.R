library(readr)
library(openxlsx)
library(dplyr)
library(stringr)

# 1. Define the exact folder where your files are located
# Use forward slashes (/) in the path to avoid Windows syntax errors in R
target_folder <- "C:\\Users\\lromano\\Box\\Lucas-MS\\soy-paper\\met-files"

# 2. Find all .met files in that specific folder
met_files <- list.files(path = target_folder, pattern = "\\.met$", full.names = TRUE)

# Print a quick confirmation check to the console
cat("Found", length(met_files), "files in the target folder.\n\n")

# 3. Loop through each file and create the two-sheet Excel file
for (file_path in met_files) {
  
  # Extract the base filename (e.g., "Keiser_AR") for labeling
  base_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Read the first few lines to find the latitude value
  raw_lines <- readLines(file_path, n = 5)
  lat_line <- raw_lines[str_detect(raw_lines, "latitude")]
  lat_val <- as.numeric(str_extract(lat_line, "-?\\d+\\.\\d+"))
  
  # Read the daily data table (skipping the top 4 metadata header lines)
  df <- read_table(file_path, skip = 4, col_names = TRUE, show_col_types = FALSE)
  
  # Drop the row containing the units text line: () () (MJ/m^2)...
  df <- df[-1, ]
  
  # Convert all weather data columns to numeric format
  df <- mutate_all(df, as.numeric)
  
  # Rename columns to uppercase standard SSM format
  colnames(df) <- c("YEAR", "DOY", "SRAD", "TMAX", "TMIN", "RAIN")
  
  # Get the data year range for the metadata block
  min_year <- min(df$YEAR, na.rm = TRUE)
  max_year <- max(df$YEAR, na.rm = TRUE)
  period_str <- paste0(min_year, "-", max_year)
  
  # 4. Build the Excel workbook sheets
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  addWorksheet(wb, "Sheet2")
  
  # Write Sheet 1 Metadata headers (Rows 1-6)
  writeData(wb, "Sheet1", x = "Location",  startCol = 1, startRow = 1)
  writeData(wb, "Sheet1", x = base_name,    startCol = 2, startRow = 1)
  
  writeData(wb, "Sheet1", x = "LAT(o):",   startCol = 1, startRow = 2)
  writeData(wb, "Sheet1", x = lat_val,      startCol = 2, startRow = 2)
  
  writeData(wb, "Sheet1", x = "LON(o):",   startCol = 1, startRow = 3)
  writeData(wb, "Sheet1", x = NA,           startCol = 2, startRow = 3) # Leave blank or fill manually
  
  writeData(wb, "Sheet1", x = "ALT(masl)", startCol = 1, startRow = 4)
  writeData(wb, "Sheet1", x = NA,           startCol = 2, startRow = 4) # Leave blank or fill manually
  
  writeData(wb, "Sheet1", x = "WINDHT(m)", startCol = 1, startRow = 5)
  writeData(wb, "Sheet1", x = 2,            startCol = 2, startRow = 5)  # Standard 2m height assumption
  
  writeData(wb, "Sheet1", x = "Period",    startCol = 1, startRow = 6)
  writeData(wb, "Sheet1", x = period_str,   startCol = 2, startRow = 6)
  
  # Write main weather data table to Sheet 1 starting at row 10
  writeData(wb, "Sheet1", df, startCol = 1, startRow = 10)
  
  # Write raw data table only to Sheet 2 starting at row 1
  writeData(wb, "Sheet2", df, startCol = 1, startRow = 1)
  
  # 5. Define output path to save right inside the same folder
  out_file_path <- file.path(target_folder, paste0("SSM_", base_name, ".xlsx"))
  saveWorkbook(wb, out_file_path, overwrite = TRUE)
  
  cat("Successfully created:", paste0("SSM_", base_name, ".xlsx"), "\n")
}
