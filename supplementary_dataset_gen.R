#Code for preprocessing the merged dataset for use in VoSViewer
library(bibliometrix)
library(dplyr)

tmp <- load("merged_bibliography_all.RData")
data <- get(tmp[1])
data <- as.data.frame(data, stringsAsFactors = FALSE)

if ("DE" %in% names(data)) {
  data$DE <- as.character(data$DE)
  data$DE[data$DE == "" | tolower(data$DE) %in% c("na","n/a","none")] <- NA
  data$DE <- gsub("[,|/]+", ";", data$DE)
  data$DE <- gsub(";{2,}", ";", data$DE)
  data$DE <- trimws(gsub("^;|;$", "", data$DE))
}

if ("UT" %in% names(data)) {
  before <- nrow(data)
  data <- data %>% distinct(UT, .keep_all = TRUE)
  after <- nrow(data)
  cat("Removed", before - after, "duplicate rows based on UT.\n")
}

if ("SR" %in% names(data)) {
  before <- nrow(data)
  data <- data %>% distinct(SR, .keep_all = TRUE)
  after <- nrow(data)
  cat("Removed", before - after, "duplicate rows based on SR.\n")
}

rownames(data) <- NULL

save(data, file = "merged_bibliography_all_cleaned.RData")
cat("✅ Final deduplicated dataset saved to merged_bibliography_all_cleaned.RData\n")
