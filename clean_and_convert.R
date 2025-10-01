# --- Load packages ---
library(stringi)
library(bibliometrix)
library(dplyr)

# --- 1. List of raw .bib files ---
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

# --- 2. Folder for cleaned raw .bib files ---
clean_folder <- "cleaned_bib_raw"
dir.create(clean_folder, showWarnings = FALSE)

# --- 3. Function to clean raw .bib files (preserve structure) ---
clean_bib_file <- function(file, out_folder){
  lines <- readLines(file, encoding = "UTF-8", warn = FALSE)
  
  # Basic cleaning: preserve raw structure
  lines <- stri_trans_general(lines, "Latin-ASCII")  # remove weird unicode
  lines <- gsub("\r", "", lines)                     # remove carriage returns
  lines <- gsub("\t", " ", lines)                   # replace tabs with spaces
  lines <- gsub("\\s+$", "", lines)                 # trim trailing spaces
  
  out_file <- file.path(out_folder, basename(file))
  writeLines(lines, out_file, useBytes = TRUE)
  return(out_file)
}

# --- 4. Apply cleaning to all raw files ---
cleaned_bib_files <- sapply(bib_files, clean_bib_file, out_folder = clean_folder)
cat("✅ Cleaned raw .bib files saved in folder:", clean_folder, "\n")

# --- 5. Convert cleaned .bib files to data frames ---
df_list <- lapply(cleaned_bib_files, function(f) convert2df(f, dbsource = "wos", format = "bibtex"))

# --- 6. Harmonize columns and merge ---
common_cols <- Reduce(intersect, lapply(df_list, colnames))
df_list <- lapply(df_list, function(df) df[, common_cols])
merged_data <- do.call(rbind, df_list)
rownames(merged_data) <- NULL
cat("✅ Converted and merged cleaned .bib files. Total records:", nrow(merged_data), "\n")

# --- 7. Deduplicate by UT and SR ---
if ("UT" %in% names(merged_data)) {
  before <- nrow(merged_data)
  merged_data <- merged_data %>% distinct(UT, .keep_all = TRUE)
  cat("Removed", before - nrow(merged_data), "duplicate rows based on UT.\n")
}
if ("SR" %in% names(merged_data)) {
  before <- nrow(merged_data)
  merged_data <- merged_data %>% distinct(SR, .keep_all = TRUE)
  cat("Removed", before - nrow(merged_data), "duplicate rows based on SR.\n")
}

# --- 8. Optional quick cleaning of DE field ---
if ("DE" %in% names(merged_data)) {
  merged_data$DE <- as.character(merged_data$DE)
  merged_data$DE[merged_data$DE == "" | tolower(merged_data$DE) %in% c("na","n/a","none")] <- NA
  merged_data$DE <- gsub("[,|/]+", ";", merged_data$DE)
  merged_data$DE <- gsub(";{2,}", ";", merged_data$DE)
  merged_data$DE <- trimws(gsub("^;|;$", "", merged_data$DE))
}

# --- 9. Clear rownames ---
rownames(merged_data) <- NULL

# --- 10. Save final cleaned and deduplicated dataset ---
save(merged_data, file = "merged_bibliography_all_cleaned.RData")
write.csv(merged_data, file = "merged_bibliography_all_cleaned.csv", row.names = FALSE)

cat("✅ Final deduplicated dataset saved as merged_bibliography_all_cleaned.RData\n")

# --- 11. Quick test: thematicMap ---
field_to_use <- "DE"
tm_try <- try({
  tm_res <- thematicMap(merged_data, field = field_to_use, minfreq = 5, stemming = FALSE, n.labels = 6, size = 0.5)
  print("✅ thematicMap executed successfully.")
}, silent = TRUE)

if (inherits(tm_try, "try-error")) {
  cat("⚠️ thematicMap still failed. Error:\n", tm_try, "\n")
} else {
  print("🎯 thematicMap ran without duplicate row.name errors.")
}

# --- 12. Quick test: thematicEvolution ---
if ("PY" %in% names(merged_data)) {
  yrs <- as.numeric(as.character(merged_data$PY))
  yrs <- yrs[!is.na(yrs)]
  if (length(yrs) >= 2) {
    years_seq <- seq(min(yrs), max(yrs), by = max(1, round((max(yrs)-min(yrs))/5)))
  } else {
    years_seq <- c(1970, 1980, 1990, 2000, 2010, 2020)
  }
} else {
  years_seq <- c(1970, 1980, 1990, 2000, 2010, 2020)
}

te_try <- try({
  te_res <- thematicEvolution(merged_data, field = field_to_use, years = years_seq, minfreq = 3)
  print("✅ thematicEvolution executed successfully.")
}, silent = TRUE)

if (inherits(te_try, "try-error")) {
  cat("⚠️ thematicEvolution failed. Error:\n", te_try, "\n")
}

















USE THIS ONE, SECOND DATASET
-----------------------------------------------------
# --- Load packages --- 
library(stringi)
library(bibliometrix)
library(dplyr)

# --- 1. List of raw .bib files ---
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

# --- 2. Folder for cleaned raw .bib files ---
clean_folder <- "cleaned_bib_raw"
dir.create(clean_folder, showWarnings = FALSE)

# --- 3. Function to clean raw .bib files (preserve structure) ---
clean_bib_file <- function(file, out_folder){
  lines <- readLines(file, encoding = "UTF-8", warn = FALSE)
  
  # Basic cleaning: preserve raw structure
  lines <- stri_trans_general(lines, "Latin-ASCII")  # remove weird unicode
  lines <- gsub("\r", "", lines)                     # remove carriage returns
  lines <- gsub("\t", " ", lines)                   # replace tabs with spaces
  lines <- gsub("\\s+$", "", lines)                 # trim trailing spaces
  
  out_file <- file.path(out_folder, basename(file))
  writeLines(lines, out_file, useBytes = TRUE)
  return(out_file)
}

# --- 4. Apply cleaning to all raw files ---
cleaned_bib_files <- sapply(bib_files, clean_bib_file, out_folder = clean_folder)
cat("✅ Cleaned raw .bib files saved in folder:", clean_folder, "\n")

# --- 5. Convert cleaned .bib files to data frames ---
df_list <- lapply(cleaned_bib_files, function(f) convert2df(f, dbsource = "wos", format = "bibtex"))

# --- 6. Harmonize columns and merge ---
common_cols <- Reduce(intersect, lapply(df_list, colnames))
df_list <- lapply(df_list, function(df) df[, common_cols])
merged_data <- do.call(rbind, df_list)
merged_data <- as.data.frame(merged_data, stringsAsFactors = FALSE)

# --- 7. Deduplicate exactly like Snippet 1 ---

# Deduplicate by UT
if ("UT" %in% colnames(merged_data)) {
  merged_data <- merged_data[!duplicated(merged_data$UT), ]
}

# Deduplicate by DOI (keep NA)
if ("DI" %in% colnames(merged_data)) {
  merged_data <- merged_data[!duplicated(merged_data$DI) | is.na(merged_data$DI), ]
}

# Deduplicate by SR
if ("SR" %in% colnames(merged_data)) {
  merged_data <- merged_data[!duplicated(merged_data$SR), ]
}

# Reset rownames
rownames(merged_data) <- seq_len(nrow(merged_data))

# --- 8. Optional quick cleaning of DE field ---
if ("DE" %in% colnames(merged_data)) {
  merged_data$DE <- as.character(merged_data$DE)
  merged_data$DE[merged_data$DE == "" | tolower(merged_data$DE) %in% c("na","n/a","none")] <- NA
  merged_data$DE <- gsub("[,|/]+", ";", merged_data$DE)
  merged_data$DE <- gsub(";{2,}", ";", merged_data$DE)
  merged_data$DE <- trimws(gsub("^;|;$", "", merged_data$DE))
}

# --- 9. Save final cleaned and deduplicated dataset ---
save(merged_data, file = "merged_bibliography_all_cleaned.RData")
write.csv(merged_data, file = "merged_bibliography_all_cleaned.csv", row.names = FALSE)

cat("✅ Final deduplicated dataset saved as merged_bibliography_all_cleaned.RData\n")

# --- 10. Quick test: thematicMap ---
field_to_use <- "DE"
tm_try <- try({
  tm_res <- thematicMap(merged_data, field = field_to_use, minfreq = 5, stemming = FALSE, n.labels = 6, size = 0.5)
  print("✅ thematicMap executed successfully.")
}, silent = TRUE)

if (inherits(tm_try, "try-error")) {
  cat("⚠️ thematicMap still failed. Error:\n", tm_try, "\n")
} else {
  print("🎯 thematicMap ran without duplicate row.name errors.")
}

# --- 11. Quick test: thematicEvolution ---
if ("PY" %in% colnames(merged_data)) {
  yrs <- as.numeric(as.character(merged_data$PY))
  yrs <- yrs[!is.na(yrs)]
  if (length(yrs) >= 2) {
    years_seq <- seq(min(yrs), max(yrs), by = max(1, round((max(yrs)-min(yrs))/5)))
  } else {
    years_seq <- c(1970, 1980, 1990, 2000, 2010, 2020)
  }
} else {
  years_seq <- c(1970, 1980, 1990, 2000, 2010, 2020)
}

te_try <- try({
  te_res <- thematicEvolution(merged_data, field = field_to_use, years = years_seq, minfreq = 3)
  print("✅ thematicEvolution executed successfully.")
}, silent = TRUE)

if (inherits(te_try, "try-error")) {
  cat("⚠️ thematicEvolution failed. Error:\n", te_try, "\n")
}
----------------------------------------------------------------

























LAST ONE I USED WHICH WORKED PERFECT
-------------------------------
# --- Load packages --- 
library(stringi)
library(bibliometrix)
library(dplyr)

# --- 1. List of raw .bib files ---
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

# --- 2. Folder for cleaned raw .bib files ---
clean_folder <- "cleaned_bib_raw"
dir.create(clean_folder, showWarnings = FALSE)

# --- 3. Function to clean raw .bib files (preserve structure) ---
clean_bib_file <- function(file, out_folder){
  lines <- readLines(file, encoding = "UTF-8", warn = FALSE)
  
  # Basic cleaning: preserve raw structure
  lines <- stri_trans_general(lines, "Latin-ASCII")  # remove weird unicode
  lines <- gsub("\r", "", lines)                     # remove carriage returns
  lines <- gsub("\t", " ", lines)                   # replace tabs with spaces
  lines <- gsub("\\s+$", "", lines)                 # trim trailing spaces
  
  out_file <- file.path(out_folder, basename(file))
  writeLines(lines, out_file, useBytes = TRUE)
  return(out_file)
}

# --- 4. Apply cleaning to all raw files ---
cleaned_bib_files <- sapply(bib_files, clean_bib_file, out_folder = clean_folder)
cat("✅ Cleaned raw .bib files saved in folder:", clean_folder, "\n")

# --- 5. Convert cleaned .bib files to data frames ---
df_list <- lapply(cleaned_bib_files, function(f) convert2df(f, dbsource = "wos", format = "bibtex"))

# --- 6. Harmonize columns and merge ---
common_cols <- Reduce(intersect, lapply(df_list, colnames))
df_list <- lapply(df_list, function(df) df[, common_cols])
merged_data <- do.call(rbind, df_list)
merged_data <- as.data.frame(merged_data, stringsAsFactors = FALSE)

# --- 7. Deduplicate exactly like Snippet 1 ---

# Deduplicate by UT
if ("UT" %in% colnames(merged_data)) {
  merged_data <- merged_data[!duplicated(merged_data$UT), ]
}

# Deduplicate by DOI (keep NA)
if ("DI" %in% colnames(merged_data)) {
  merged_data <- merged_data[!duplicated(merged_data$DI) | is.na(merged_data$DI), ]
}

# Deduplicate by SR
if ("SR" %in% colnames(merged_data)) {
  merged_data <- merged_data[!duplicated(merged_data$SR), ]
}

# Reset rownames
rownames(merged_data) <- seq_len(nrow(merged_data))

# --- 8. Optional quick cleaning of DE field ---
if ("DE" %in% colnames(merged_data)) {
  merged_data$DE <- as.character(merged_data$DE)
  merged_data$DE[merged_data$DE == "" | tolower(merged_data$DE) %in% c("na","n/a","none")] <- NA
  merged_data$DE <- gsub("[,|/]+", ";", merged_data$DE)
  merged_data$DE <- gsub(";{2,}", ";", merged_data$DE)
  merged_data$DE <- trimws(gsub("^;|;$", "", merged_data$DE))
}

# --- 9. Save final cleaned and deduplicated dataset ---
save(merged_data, file = "merged_bibliography_all_cleaned.RData")
write.csv(merged_data, file = "merged_bibliography_all_cleaned.csv", row.names = FALSE)

cat("✅ Final deduplicated dataset saved as merged_bibliography_all_cleaned.RData\n")

# --- 10. Quick test: thematicMap ---
field_to_use <- "DE"
tm_try <- try({
  tm_res <- thematicMap(merged_data, field = field_to_use, minfreq = 5, stemming = FALSE, n.labels = 6, size = 0.5)
  print("✅ thematicMap executed successfully.")
}, silent = TRUE)

if (inherits(tm_try, "try-error")) {
  cat("⚠️ thematicMap still failed. Error:\n", tm_try, "\n")
} else {
  print("🎯 thematicMap ran without duplicate row.name errors.")
}

# --- 11. Quick test: thematicEvolution ---
if ("PY" %in% colnames(merged_data)) {
  yrs <- as.numeric(as.character(merged_data$PY))
  yrs <- yrs[!is.na(yrs)]
  if (length(yrs) >= 2) {
    years_seq <- seq(min(yrs), max(yrs), by = max(1, round((max(yrs)-min(yrs))/5)))
  } else {
    years_seq <- c(1970, 1980, 1990, 2000, 2010, 2020)
  }
} else {
  years_seq <- c(1970, 1980, 1990, 2000, 2010, 2020)
}

te_try <- try({
  te_res <- thematicEvolution(merged_data, field = field_to_use, years = years_seq, minfreq = 3)
  print("✅ thematicEvolution executed successfully.")
}, silent = TRUE)

if (inherits(te_try, "try-error")) {
  cat("⚠️ thematicEvolution failed. Error:\n", te_try, "\n")
}
