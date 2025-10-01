#Code to extract institutions and their number of publication
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(openxlsx)

load("merged_bibliography_all_cleaned.RData")
df <- merged_data

inst_keyword_re <- "(UNIVERSITY|COLLEGE|INSTITUTE|INSTITUT|HOSPITAL|LABORATORY|\\bLAB\\b|CENTER|CENTRE|DEPARTMENT|\\bDEPT\\b|SCHOOL|FACULTY|CNRS|CSIC|CINVESTAV|\\bUNIV\\b|NATIONAL|NATL|INRA|CSIR|TECHNICAL|POLY|MEDICAL|ACADEMY|ACAD|MUSEUM|RES INST|RESEARCH CTR|RES CTR|GEOLOGICAL SURVEY|GEOL SURVEY|STATE KEY LAB)\\b"

clean_text <- function(x) {
  x %>%
    str_replace_all("[\\\\/]+", " ") %>%
    str_replace_all("\\$+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

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

institution_summary <- inst %>%
  group_by(inst_clean) %>%
  summarise(publications = n(), .groups = "drop") %>%
  arrange(desc(publications))

write_csv(institution_summary, "institution_publication_summary.csv")
openxlsx::write.xlsx(institution_summary, "institution_publication_summary.xlsx")
