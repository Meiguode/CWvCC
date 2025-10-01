# --- Load packages ---
library(bibliometrix)
library(dplyr)

# --- Load cleaned data ---
tmp <- load("merged_bibliography_all.RData")
data <- get(tmp[1])
data <- as.data.frame(data, stringsAsFactors = FALSE)

# --- OPTIONAL: Clean DE again quickly ---
if ("DE" %in% names(data)) {
  data$DE <- as.character(data$DE)
  data$DE[data$DE == "" | tolower(data$DE) %in% c("na","n/a","none")] <- NA
  data$DE <- gsub("[,|/]+", ";", data$DE)
  data$DE <- gsub(";{2,}", ";", data$DE)
  data$DE <- trimws(gsub("^;|;$", "", data$DE))
}

# --- REMOVE duplicates by UT first (if present) ---
if ("UT" %in% names(data)) {
  before <- nrow(data)
  data <- data %>% distinct(UT, .keep_all = TRUE)
  after <- nrow(data)
  cat("Removed", before - after, "duplicate rows based on UT.\n")
}

# --- REMOVE duplicates by SR (Source Reference) if still present ---
if ("SR" %in% names(data)) {
  before <- nrow(data)
  data <- data %>% distinct(SR, .keep_all = TRUE)
  after <- nrow(data)
  cat("Removed", before - after, "duplicate rows based on SR.\n")
}

# --- FINAL safety: clear rownames ---
rownames(data) <- NULL

# --- Save cleaned data ---
save(data, file = "merged_bibliography_all_cleaned.RData")
cat("✅ Final deduplicated dataset saved to merged_bibliography_all_cleaned.RData\n")

# --- Quick test: thematicMap ---
field_to_use <- "DE"
tm_try <- try({
  tm_res <- thematicMap(data, field = field_to_use, minfreq = 5, stemming = FALSE, n.labels = 6, size = 0.5)
  print("✅ thematicMap executed successfully.")
}, silent = TRUE)

if (inherits(tm_try, "try-error")) {
  cat("⚠️ thematicMap still failed. Error:\n", tm_try, "\n")
} else {
  print("🎯 thematicMap ran without duplicate row.name errors.")
}

# --- Quick test: thematicEvolution ---
if ("PY" %in% names(data)) {
  yrs <- as.numeric(as.character(data$PY))
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
  te_res <- thematicEvolution(data, field = field_to_use, years = years_seq, minfreq = 3)
  print("✅ thematicEvolution executed successfully.")
}, silent = TRUE)

if (inherits(te_try, "try-error")) {
  cat("⚠️ thematicEvolution failed. Error:\n", te_try, "\n")
}
