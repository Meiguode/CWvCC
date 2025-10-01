# --- Load required package ---
library(openxlsx)

# --- Step 1: Read Excel file ---
input_file <- "Source_Impact_bibliometrix_2025-09-15.xlsx"
data <- read.xlsx(input_file)

# --- Step 2: Rename "Sources" to "Journal" ---
names(data)[names(data) == "Sources"] <- "Journal"

# --- Step 3: Keep and reorder columns ---
data_cleaned <- data[, c("Journal", "NP", "h_index", "TC", "PY_start")]

# --- Step 4: Arrange in descending order of NP ---
data_cleaned <- data_cleaned[order(-data_cleaned$NP), ]

# --- Step 5: Save to new Excel file ---
output_file <- "Source_Impact_bibliometrix_2025-09-15_cleaned.xlsx"
write.xlsx(data_cleaned, output_file, rowNames = FALSE)

cat("✅ File cleaned and saved as:", output_file, "\n")
