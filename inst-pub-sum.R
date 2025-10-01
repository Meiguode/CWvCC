library(dplyr)
library(stringr)
library(readr)
library(tidyr)

# -------------------------------
# 1. Load data
# -------------------------------
inst_raw <- read_csv("institution_summary.csv", show_col_types = FALSE)

# -------------------------------
# 2. Institution keyword regex
# -------------------------------
inst_keyword_re <- "(UNIVERSITY|COLLEGE|INSTITUTE|INSTITUT|HOSPITAL|LABORATORY|\\bLAB\\b|CENTER|CENTRE|DEPARTMENT|\\bDEPT\\b|SCHOOL|FACULTY|CNRS|CSIC|CINVESTAV|\\bUNIV\\b|NATIONAL|NATL|INRA|CSIR|TECHNICAL|POLY|MEDICAL|ACADEMY|ACAD|MUSEUM|RES INST|RESEARCH CTR|RES CTR|GEOLOGICAL SURVEY|GEOL SURVEY|STATE KEY LAB)\\b"

# -------------------------------
# 3. Cleaning function
# -------------------------------
clean_text <- function(x) {
  x %>%
    str_replace_all("[\\\\/]+", " ") %>%      # slashes
    str_replace_all("\\$+", " ") %>%         # dollar signs
    str_replace_all("\\s+", " ") %>%         # multiple spaces
    str_trim()
}

# -------------------------------
# 4. Extract institution names
# -------------------------------
inst <- inst_raw %>%
  mutate(
    aff_raw = if_else(is.na(sample_aff), "", sample_aff),
    aff_clean = clean_text(aff_raw),
    
    # Remove common author roles
    aff_no_roles = aff_clean %>%
      str_remove_all("\\(.*?author.*?\\)") %>%
      str_remove_all("\\(.*?corresponding.*?\\)") %>%
      str_remove_all("\\(.*?present address.*?\\)"),
    
    aff_up = str_to_upper(aff_no_roles)
  ) %>%
  # Extract the institution plus words before and after keyword
  mutate(inst_extracted = str_extract(
    aff_up,
    paste0("[A-Z &]{0,50}", inst_keyword_re, "[A-Z &]{0,50}")
  )) %>%
  # Clean up extracted text
  mutate(inst_clean = inst_extracted %>%
           str_replace_all("[\\.:;\"`'\\\\/\\$]", " ") %>%
           str_replace_all("\\s+", " ") %>%
           str_trim()) %>%
  filter(!is.na(inst_clean) & inst_clean != "")

# -------------------------------
# 5. Create unique institutions with publication counts
# -------------------------------
institution_summary <- inst %>%
  group_by(inst_clean) %>%
  summarise(publications = n(), .groups = "drop") %>%
  arrange(desc(publications))

# -------------------------------
# 6. Diagnostics
# -------------------------------
cat("Total rows in file:", nrow(inst_raw), "\n")
cat("Rows with institutions extracted:", nrow(inst), "\n")
cat("Unique institutions after cleaning:", nrow(institution_summary), "\n")

# -------------------------------
# 7. Save outputs
# -------------------------------
write_csv(institution_summary, "institution_publication_summary.csv")
