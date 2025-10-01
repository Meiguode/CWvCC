#Preprocessing of bib files for analysis on Biblioshiny
library(bibliometrix)
#WoS supports downloads of only 500 documents at a time, hence the 8 files
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

bib_list <- list()

for (file in bib_files) {
  bib_df <- convert2df(file, dbsource = "wos", format = "bibtex")
  bib_list[[length(bib_list) + 1]] <- bib_df
}

common_cols <- Reduce(intersect, lapply(bib_list, colnames))
bib_list_clean <- lapply(bib_list, function(df) df[, common_cols])

merged_data <- do.call(rbind, bib_list_clean)

save(merged_data, file = "merged_bibliography_all.RData")
#To launch the app - Biblioshiny()
