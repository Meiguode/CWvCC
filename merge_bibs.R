# Install packages if missing
if (!require(bibliometrix)) install.packages("bibliometrix")

library(bibliometrix)

# List of .bib files
files <- c("savedrecs.bib",
           "savedrecs (1).bib",
           "savedrecs (2).bib",
           "savedrecs (3).bib",
           "savedrecs (4).bib",
           "savedrecs (5).bib",
           "savedrecs (6).bib",
           "savedrecs (7).bib")

# Merge all files in one go
M <- convert2df(file = files, dbsource = "isi", format = "bibtex")

# Remove duplicates based on DOI (preferred) or Title if DOI is missing
dup_doi   <- duplicated(M$DI) & !is.na(M$DI)   # duplicates by DOI
dup_title <- duplicated(tolower(M$TI))         # duplicates by Title
dup_all   <- dup_doi | dup_title

M_clean <- M[!dup_all, ]

# Save as CSV (handles long text better than Excel)
write.csv(as.data.frame(M_clean),
          file = "merged_bibliography.csv",
          row.names = FALSE)

# Save as RData for biblioshiny
save(M_clean, file = "merged_bibliography.RData")

cat("✅ Done! Merged .bib files, removed duplicates, and saved as CSV + RData.\n")













USE THIS
-----------------------------------------------------------------
# Load required libraries
library(bibliometrix)

# List of .bib files
bib_files <- c(
  "savedrecs.bib",
  "savedrecs (1).bib",
  "savedrecs (2).bib",
  "savedrecs (3).bib",
  "savedrecs (4).bib",
  "savedrecs (5).bib",
  "savedrecs (6).bib",
  "savedrecs (7).bib"
)

# Initialize empty list to store data frames
bib_list <- list()

# Read each .bib file and convert to data frame
for (file in bib_files) {
  bib_df <- convert2df(file, dbsource = "wos", format = "bibtex")
  bib_list[[length(bib_list) + 1]] <- bib_df
}

# Harmonize column structure across all data frames
common_cols <- Reduce(intersect, lapply(bib_list, colnames))
bib_list_clean <- lapply(bib_list, function(df) df[, common_cols])

# Merge all cleaned data frames (no deduplication)
merged_data <- do.call(rbind, bib_list_clean)

# Save as RData
save(merged_data, file = "merged_bibliography_all.RData")

# Optional: Launch biblioshiny
# biblioshiny()  # Uncomment to launch the app
------------------------------------------------------------------















# Install required packages if needed
# install.packages("bibliometrix")
# install.packages("dplyr")

library(bibliometrix)
library(dplyr)

setwd("path/to/your/bib/files")

files <- list.files(pattern = "savedrecs.*\\.bib$")

# Convert each file into a dataframe
bib_list <- lapply(files, function(f) {
  convert2df(file = f, dbsource = "wos", format = "bibtex")
})

# Merge all dataframes safely (fills missing columns with NA)
M <- dplyr::bind_rows(bib_list)

# Remove duplicates (based on WoS unique ID)
M <- M[!duplicated(M$UT), ]

# Save as merged .bib for Biblioshiny
writeLines(df2bib(M), "merged_savedrecs.bib")

# Also save as RData for use in scripts
save(M, file = "merged_savedrecs.RData")

cat("✅ Merged dataset created: merged_savedrecs.bib and merged_savedrecs.RData\n")



library(bibliometrix)
library(dplyr)

setwd("path/to/your/bib/files")

files <- list.files(pattern = "savedrecs.*\\.bib$")

# Convert each .bib file
bib_list <- lapply(files, function(f) convert2df(file = f, dbsource = "wos", format = "bibtex"))

# Merge, allow for different columns
M <- dplyr::bind_rows(bib_list)

# Drop duplicates based on WoS ID
M <- M[!duplicated(M$UT), ]

# Reset rownames to avoid duplicate row name error in Biblioshiny
rownames(M) <- NULL

# Save for Biblioshiny
save(M, file = "merged_savedrecs.RData")

cat("✅ Done! Upload merged_savedrecs.RData into Biblioshiny.\n")




