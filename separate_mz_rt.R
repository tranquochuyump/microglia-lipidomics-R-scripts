library(readxl)
library(writexl)
library(dplyr)
library(tidyr)

process_files <- function(folder_path, output_folder = file.path(folder_path, "processed")) {
  # To create the output folder in case not existent
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  
  # Only take the files with .xlsx in names to put in the list
  file_list <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)
  
  for (file in file_list) {
    df <- read_excel(file)
    
    # Check if there is any m/z column
    if (!"m/z" %in% colnames(df)) {
      warning(paste("File", basename(file), "Not found 'm/z'. Skip."))
      next
    }
    
    # Separate the column into two 
    df <- df %>%
      separate("m/z", into = c("m/z", "retention time"), sep = "__", remove = TRUE)
    
    # Generate output file 
    output_file <- file.path(output_folder, basename(file))
    write_xlsx(df, output_file)
    message(paste("Processed:", basename(file)))
  }
}
process_files("~/Desktop/Master thesis/Raw data/Annotation")
