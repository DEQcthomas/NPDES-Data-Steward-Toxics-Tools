library(readxl)
library(dplyr)
library(purrr)
library(openxlsx)
library(tidyverse)

In <- "C:/Users/cthomas/OneDrive - Oregon/Desktop/combine"
Out <-"C:/Users/cthomas/OneDrive - Oregon/Desktop/working_area/2023_Riverbend_Landfill_Split.xlsx"

#sheet number usually 1 or 6
sheet_number <- 1


# Define the folder path containing the .xlsx files
folder_path <- In


#read all files
file_list <- list.files(path = folder_path, pattern = "*", full.names = TRUE)

# Step 1: Read all Excel files and convert columns to character
data_list <- map(file_list, ~ {
  read_excel(.x, sheet = sheet_number) %>%
    mutate(across(everything(), as.character))
})

# Find the maximum number of columns among all files
max_cols <- max(map_int(data_list, ncol))

# Step 3: Pad each dataset to max columns
pad_to_max <- function(df, max_cols) {
  missing <- max_cols - ncol(df)
  if (missing > 0) {
    extra <- as.data.frame(matrix(NA, nrow(df), missing))
    colnames(extra) <- paste0("X", seq_len(missing))
    df <- cbind(df, extra)
  }
  df
}

data_list_padded <- map(data_list, ~ {
  df <- .x
  
  # Force tibble → data.frame
  df <- as.data.frame(df)
  
  # Force all columns to character
  df[] <- lapply(df, as.character)
  
  # Pad to max columns
  pad_to_max(df, max_cols)
})

# Combine by column number
#combined_data <- bind_rows(data_list_padded, .name_repair = "minimal")


# Combine by column position, ignoring names
combined_df <- do.call(rbind, lapply(data_list_padded, function(df) {
  # Reset column names to avoid name matching
  colnames(df) <- paste0("V", seq_len(ncol(df)))
  df
}))

write.xlsx(combined_df, Out)



























