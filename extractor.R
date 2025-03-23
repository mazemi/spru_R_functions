library(fs)
library(stringr)

root_dir <- "data"
dest_dir <- "extracted_files"

if (!dir_exists(dest_dir)) dir_create(dest_dir)

# Get all folders ending with "Round_X"
round_folders <- dir_ls(root_dir, recurse = TRUE, type = "directory")
round_folders <- round_folders[str_detect(round_folders, "Round_\\d+$")]

for (folder in round_folders) {
  round_num <- str_extract(folder, "Round_\\d+$")
  
  # target sub folder inside each Round_X directory
  target_folder <- file.path(folder, "input", "data", "clean", "curr_data")
  
  if (dir_exists(target_folder)) {
    excel_files <- dir_ls(target_folder, glob = "*.xlsx")
    
    for (i in seq_along(excel_files)) {
      # new_name <- file.path(dest_dir, paste0("file_", gsub("Round_", "", round_num), "_", i, ".xlsx"))
      new_name <- file.path(dest_dir, paste0("file_", gsub("Round_", "", round_num), ".xlsx"))
      file_copy(excel_files[i], new_name, overwrite = TRUE)
    }
  }
}

print("Excel files extracted successfully!")
