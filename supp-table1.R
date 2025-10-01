#Code for supplementary table

library(openxlsx)

input_file <- "Source_Impact_bibliometrix_2025-09-15.xlsx"
data <- read.xlsx(input_file)

names(data)[names(data) == "Sources"] <- "Journal"

data_cleaned <- data[, c("Journal", "NP", "h_index", "TC", "PY_start")]

data_cleaned <- data_cleaned[order(-data_cleaned$NP), ]

output_file <- "Source_Impact_bibliometrix_2025-09-15_cleaned.xlsx"
write.xlsx(data_cleaned, output_file, rowNames = FALSE)

cat("✅ File cleaned and saved as:", output_file, "\n")

