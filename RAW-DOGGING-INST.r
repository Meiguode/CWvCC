#STEP 1 IN ALL AND ALL
library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# -------------------------------
# 1. Load data
# -------------------------------
load("merged_bibliography_all_cleaned.RData")
df <- merged_data
cat("Total rows in dataset:", nrow(df), "\n")

# -------------------------------
# 2. Extract affiliations with paper ID
# -------------------------------
affiliations_df <- df %>%
  mutate(.paper_id = row_number()) %>%    # create paper ID
  select(.paper_id, DOI = DI, affiliations)   # pair with DOI if you want

# -------------------------------
# 3. Split multiple affiliations into separate rows
# -------------------------------
aff_long <- affiliations_df %>%
  filter(!is.na(affiliations) & affiliations != "") %>%   # remove empty
  mutate(affiliations = str_replace_all(affiliations, "\\s*;;\\s*", ";")) %>%  # fix double semicolons
  separate_rows(affiliations, sep = ";") %>%              # split on semicolon
  mutate(affiliations = str_squish(affiliations))        # remove extra spaces

# -------------------------------
# 4. Save full list of institutions per paper
# -------------------------------
write_csv(aff_long, "all_affiliations_split.csv")
cat("Saved all_affiliations_split.csv with each institution on a separate row.\n")

# -------------------------------
# 5. Optional: inspect
# -------------------------------
print(head(aff_long, 20))





#STEP 2 IN ALL AND ALL
library(dplyr)
library(readr)

# -------------------------------
# 1. Count occurrences of each institution
# -------------------------------
institution_counts <- aff_long %>%
  group_by(affiliations) %>%
  summarise(Occurrences = n(), .groups = "drop") %>%
  arrange(desc(Occurrences))   # most frequent first

# Save frequency-sorted CSV
write_csv(institution_counts, "institution_occurrences_sorted.csv")
cat("Saved institution_occurrences_sorted.csv (most frequent at top).\n")

# -------------------------------
# 2. Alphabetical list of institutions
# -------------------------------
institution_alpha <- institution_counts %>%
  arrange(affiliations)        # alphabetical order

# Save alphabetical CSV
write_csv(institution_alpha, "institution_occurrences_alphabetical.csv")
cat("Saved institution_occurrences_alphabetical.csv (alphabetical order).\n")

# -------------------------------
# 3. Optional: preview top and bottom
# -------------------------------
cat("Top 10 most frequent institutions:\n")
print(head(institution_counts, 10))

cat("\nFirst 10 institutions alphabetically:\n")
print(head(institution_alpha, 10))
























#USE THIS ONE!!!
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(openxlsx)


# -------------------------------
# 1. Load dataset
# -------------------------------
load("merged_bibliography_all_cleaned.RData")  # loads merged_data
df <- merged_data

# -------------------------------
# 2. Institution keyword regex
# -------------------------------
inst_keyword_re <- "(UNIVERSITY|COLLEGE|INSTITUTE|INSTITUT|HOSPITAL|LABORATORY|\\bLAB\\b|CENTER|CENTRE|DEPARTMENT|\\bDEPT\\b|SCHOOL|FACULTY|CNRS|CSIC|CINVESTAV|\\bUNIV\\b|NATIONAL|NATL|INRA|CSIR|TECHNICAL|POLY|MEDICAL|ACADEMY|ACAD|MUSEUM|RES INST|RESEARCH CTR|RES CTR|GEOLOGICAL SURVEY|GEOL SURVEY|STATE KEY LAB)\\b"

# -------------------------------
# 3. Cleaning function
# -------------------------------
clean_text <- function(x) {
  x %>%
    str_replace_all("[\\\\/]+", " ") %>%      # remove slashes
    str_replace_all("\\$+", " ") %>%         # remove dollar signs
    str_replace_all("\\s+", " ") %>%         # multiple spaces -> one
    str_trim()
}

# -------------------------------
# 4. Prepare raw affiliations
# -------------------------------
# Use C1 column (full author affiliation column)
inst <- df %>%
  mutate(.paper_id = row_number(),
         aff_raw = as.character(C1)) %>%
  filter(!is.na(aff_raw) & aff_raw != "") %>%
  # Split multiple affiliations per paper
  mutate(aff_list = str_split(aff_raw, ";|\\|")) %>%
  tidyr::unnest(cols = c(aff_list)) %>%
  mutate(aff_list = str_squish(aff_list),
         aff_clean = clean_text(aff_list),
         # Remove author roles like (corresponding author)
         aff_no_roles = aff_clean %>%
           str_remove_all("\\(.*?author.*?\\)") %>%
           str_remove_all("\\(.*?corresponding.*?\\)") %>%
           str_remove_all("\\(.*?present address.*?\\)"),
         aff_up = str_to_upper(aff_no_roles)) %>%
  # Extract only institution part using keywords
  mutate(inst_extracted = str_extract(
    aff_up,
    paste0("[A-Z &]{0,50}", inst_keyword_re, "[A-Z &]{0,50}")
  )) %>%
  mutate(inst_clean = inst_extracted %>%
           str_replace_all("[\\.:;\"`'\\\\/\\$]", " ") %>%
           str_replace_all("\\s+", " ") %>%
           str_trim()) %>%
  filter(!is.na(inst_clean) & inst_clean != "")

# -------------------------------
# 5. Count publications per institution
# -------------------------------
institution_summary <- inst %>%
  group_by(inst_clean) %>%
  summarise(publications = n(), .groups = "drop") %>%
  arrange(desc(publications))

# -------------------------------
# 6. Save outputs
# -------------------------------
write_csv(institution_summary, "institution_publication_summary.csv")
openxlsx::write.xlsx(institution_summary, "institution_publication_summary.xlsx")

# -------------------------------
# 7. Optional diagnostics
# -------------------------------
cat("Total rows in dataset:", nrow(df), "\n")
cat("Rows with institutions extracted:", nrow(inst), "\n")
cat("Unique institutions after cleaning:", nrow(institution_summary), "\n")

cat("\nTop 10 most frequent institutions:\n")
print(head(institution_summary, 10))

cat("\nFirst 10 institutions alphabetically:\n")
print(head(institution_summary %>% arrange(inst_clean), 10))
