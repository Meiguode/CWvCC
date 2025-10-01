#Code for preprocessing the merged dataset for use in VoSViewer 
library(stringi)
library(bibliometrix)
library(dplyr)

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

clean_folder <- "cleaned_bib_raw"
dir.create(clean_folder, showWarnings = FALSE)

clean_bib_file <- function(file, out_folder){
  lines <- readLines(file, encoding = "UTF-8", warn = FALSE)
  
  lines <- stri_trans_general(lines, "Latin-ASCII")
  lines <- gsub("\r", "", lines)
  lines <- gsub("\t", " ", lines)
  lines <- gsub("\\s+$", "", lines)
  
  out_file <- file.path(out_folder, basename(file))
  writeLines(lines, out_file, useBytes = TRUE)
  return(out_file)
}

cleaned_bib_files <- sapply(bib_files, clean_bib_file, out_folder = clean_folder)
cat("✅ Cleaned raw .bib files saved in folder:", clean_folder, "\n")

df_list <- lapply(cleaned_bib_files, function(f) convert2df(f, dbsource = "wos", format = "bibtex"))

common_cols <- Reduce(intersect, lapply(df_list, colnames))
df_list <- lapply(df_list, function(df) df[, common_cols])
merged_data <- do.call(rbind, df_list)
merged_data <- as.data.frame(merged_data, stringsAsFactors = FALSE)

if ("UT" %in% colnames(merged_data)) {
  merged_data <- merged_data[!duplicated(merged_data$UT), ]
}

if ("DI" %in% colnames(merged_data)) {
  merged_data <- merged_data[!duplicated(merged_data$DI) | is.na(merged_data$DI), ]
}

if ("SR" %in% colnames(merged_data)) {
  merged_data <- merged_data[!duplicated(merged_data$SR), ]
}

rownames(merged_data) <- seq_len(nrow(merged_data))

if ("DE" %in% colnames(merged_data)) {
  merged_data$DE <- as.character(merged_data$DE)
  merged_data$DE[merged_data$DE == "" | tolower(merged_data$DE) %in% c("na","n/a","none")] <- NA
  merged_data$DE <- gsub("[,|/]+", ";", merged_data$DE)
  merged_data$DE <- gsub(";{2,}", ";", merged_data$DE)
  merged_data$DE <- trimws(gsub("^;|;$", "", merged_data$DE))
}

save(merged_data, file = "merged_bibliography_all_cleaned.RData")
write.csv(merged_data, file = "merged_bibliography_all_cleaned.csv", row.names = FALSE)
