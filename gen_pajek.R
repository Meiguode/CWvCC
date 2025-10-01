# Install if missing
if (!require(bibliometrix)) install.packages("bibliometrix")
if (!require(igraph)) install.packages("igraph")

library(bibliometrix)
library(igraph)

cat("🔄 Loading merged dataset...\n")
load("merged_bibliography_all.RData")  # loads M_clean
M <- M_clean
cat("✅ Dataset loaded with", nrow(M), "records\n")

# --- Extract metadata for countries and institutions ---
cat("🔄 Extracting metadata fields...\n")
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")  # Countries
M <- metaTagExtraction(M, Field = "AU_UN", sep = ";")  # Institutions
cat("✅ Metadata extraction done. Columns now:", paste(colnames(M), collapse=", "), "\n")

# Helper function to build and save a Pajek network
save_pajek <- function(mat, filename, type_label) {
  cat("🔄 Building igraph object for", type_label, "...\n")
  g <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE, diag = FALSE)
  cat("   → Nodes:", vcount(g), "| Edges:", ecount(g), "\n")
  cat("🔄 Writing to Pajek file:", filename, "\n")
  write.graph(g, file = filename, format = "pajek")
  cat("✅ Saved Pajek file:", filename, "\n\n")
}

# ==========================
# 1. Keyword co-occurrence network
# ==========================
cat("=== Keywords Network ===\n")
Net_kw <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
save_pajek(Net_kw, "network_keywords.net", "Keywords")

# ==========================
# 2. Country collaboration network
# ==========================
cat("=== Countries Network ===\n")
Net_ctry <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
save_pajek(Net_ctry, "network_countries.net", "Countries")

# ==========================
# 3. Institution collaboration network
# ==========================
cat("=== Institutions Network ===\n")
Net_inst <- biblioNetwork(M, analysis = "collaboration", network = "universities", sep = ";")
save_pajek(Net_inst, "network_institutions.net", "Institutions")

# ==========================
# 4. Co-citation network (References)
# ==========================
cat("=== Co-citation Network ===\n")
Net_cocit <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
save_pajek(Net_cocit, "network_cocitation.net", "Co-citation")

cat("🎉 All Pajek .net files are ready! Import them into VOSviewer.\n")


















# Load required libraries
library(bibliometrix)

# If needed, load your merged data
load("merged_bibliography_all.RData")  # Uncomment if not already loaded

# Create bibliometric network list
networks <- biblioNetwork(merged_data, analysis = "collaboration", network = "countries", sep = ";")
write.graph(networks, file = "country_collaboration.net", format = "pajek")

networks <- biblioNetwork(merged_data, analysis = "collaboration", network = "universities", sep = ";")
write.graph(networks, file = "institution_collaboration.net", format = "pajek")

networks <- biblioNetwork(merged_data, analysis = "co-citation", network = "references", sep = ";")
write.graph(networks, file = "cocitation_references.net", format = "pajek")

networks <- biblioNetwork(merged_data, analysis = "co-occurrences", network = "keywords", sep = ";")
write.graph(networks, file = "keyword_cooccurrence.net", format = "pajek")













# Install if missing
if (!require(bibliometrix)) install.packages("bibliometrix")
if (!require(igraph)) install.packages("igraph")

library(bibliometrix)
library(igraph)

cat("🔄 Loading merged dataset...\n")
load("merged_bibliography_all.RData")  # loads M_clean
M <- M_clean
cat("✅ Dataset loaded with", nrow(M), "records\n")

# --- Extract metadata for countries and institutions ---
cat("🔄 Extracting metadata fields...\n")
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")  # Countries
M <- metaTagExtraction(M, Field = "AU_UN", sep = ";")  # Institutions
cat("✅ Metadata extraction done. Columns now:", paste(colnames(M), collapse=", "), "\n")

# Helper function to build and save a Pajek network
save_pajek <- function(mat, filename, type_label) {
  cat("🔄 Building igraph object for", type_label, "...\n")
  g <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE, diag = FALSE)
  cat("   → Nodes:", vcount(g), "| Edges:", ecount(g), "\n")
  cat("🔄 Writing to Pajek file:", filename, "\n")
  write.graph(g, file = filename, format = "pajek")
  cat("✅ Saved Pajek file:", filename, "\n\n")
}

# ==========================
# 1. Keyword co-occurrence network
# ==========================
cat("=== Keywords Network ===\n")
Net_kw <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
save_pajek(Net_kw, "network_keywords.net", "Keywords")

# ==========================
# 2. Country collaboration network
# ==========================
cat("=== Countries Network ===\n")
Net_ctry <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
save_pajek(Net_ctry, "network_countries.net", "Countries")

# ==========================
# 3. Institution collaboration network
# ==========================
cat("=== Institutions Network ===\n")
Net_inst <- biblioNetwork(M, analysis = "collaboration", network = "universities", sep = ";")
save_pajek(Net_inst, "network_institutions.net", "Institutions")

# ==========================
# 4. Co-citation network (References)
# ==========================
cat("=== Co-citation Network ===\n")
Net_cocit <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")
save_pajek(Net_cocit, "network_cocitation.net", "Co-citation")

cat("🎉 All Pajek .net files are ready! Import them into VOSviewer.\n")















































library(bibliometrix)
library(igraph)

# Load your cleaned M
load("merged_savedrecs_clean.RData")
M <- as.data.frame(M, stringsAsFactors = FALSE)
rownames(M) <- seq_len(nrow(M))

# Extract countries
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

# Build network
vos_data <- biblioNetwork(M, analysis = "collaboration", network = "countries")
net <- graph_from_adjacency_matrix(vos_data, mode = "undirected", weighted = TRUE, diag = FALSE)

# Write VOSviewer-style Pajek .net
net_file <- "vos_country_collab.net"
con <- file(net_file, "w")

# Write vertices
writeLines(paste0("*Vertices ", vcount(net)), con)
for (i in 1:vcount(net)) {
  label <- V(net)$name[i]
  writeLines(paste(i, shQuote(label)), con)
}

# Write edges (with weights)
writeLines("*Edges", con)
edges <- as_data_frame(net, what = "edges")
for (i in 1:nrow(edges)) {
  from <- match(edges$from[i], V(net)$name)
  to   <- match(edges$to[i], V(net)$name)
  w    <- edges$weight[i]
  writeLines(paste(from, to, w), con)
}
close(con)

cat("✅ Wrote Pajek file:", net_file, "\n")



library(bibliometrix)
library(igraph)
library(dplyr)
library(tidyr)
library(stringr)

# ---- Load cleaned merged data ----
load("merged_savedrecs_clean.RData")   # Object: M
M <- as.data.frame(M, stringsAsFactors = FALSE)
rownames(M) <- seq_len(nrow(M))

# -----------------------------
# Helper: write Pajek .net file
# -----------------------------
write_pajek <- function(net, file) {
  con <- file(file, "w")
  writeLines(paste0("*Vertices ", vcount(net)), con)
  for (i in 1:vcount(net)) {
    label <- V(net)$name[i]
    writeLines(paste(i, shQuote(label)), con)
  }
  writeLines("*Edges", con)
  edges <- as_data_frame(net, what = "edges")
  for (i in 1:nrow(edges)) {
    from <- match(edges$from[i], V(net)$name)
    to   <- match(edges$to[i], V(net)$name)
    w    <- edges$weight[i]
    writeLines(paste(from, to, w), con)
  }
  close(con)
  cat("✅ Wrote Pajek file:", file, "\n")
}

# ====================================================
# 1. Collaboration network of top institutes (Fig. 6)
# ====================================================
M <- metaTagExtraction(M, Field = "AU_UN", sep = ";")

inst_net <- biblioNetwork(M, analysis = "collaboration", network = "universities")

# Convert to igraph
g_inst <- graph_from_adjacency_matrix(inst_net, mode = "undirected", weighted = TRUE, diag = FALSE)

# Keep top 40 institutes
deg <- degree(g_inst, mode = "all")
top40 <- names(sort(deg, decreasing = TRUE))[1:40]
g_inst_top <- induced_subgraph(g_inst, vids = top40)

write_pajek(g_inst_top, "vos_inst_collab.net")

# ====================================================
# 2. Co-citation network (Fig. 7)
# ====================================================
# Build co-citation matrix
cocit_mat <- biblioNetwork(M, analysis = "co-citation", network = "references")

# Convert to igraph
g_cocit <- graph_from_adjacency_matrix(cocit_mat, mode = "undirected", weighted = TRUE, diag = FALSE)

# Keep most cited references (e.g., top 500 by degree or strength)
strengths <- strength(g_cocit)
top_refs <- names(sort(strengths, decreasing = TRUE))[1:500]
g_cocit_top <- induced_subgraph(g_cocit, vids = top_refs)

write_pajek(g_cocit_top, "vos_cocitation.net")

# ====================================================
# 3. Keyword co-occurrence network (Fig. 8)
# ====================================================
M <- metaTagExtraction(M, Field = "ID", sep = ";")
M <- metaTagExtraction(M, Field = "DE", sep = ";")

kw_net <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords")

g_kw <- graph_from_adjacency_matrix(kw_net, mode = "undirected", weighted = TRUE, diag = FALSE)

# Filter keywords by frequency (min degree >= 5)
deg_kw <- degree(g_kw)
keep_kw <- names(deg_kw[deg_kw >= 5])
g_kw_top <- induced_subgraph(g_kw, vids = keep_kw)

write_pajek(g_kw_top, "vos_keywords.net")







































library(bibliometrix)
library(igraph)

# Load your cleaned M
load("merged_bibliography_all_cleaned.RData")
M <- as.data.frame(M, stringsAsFactors = FALSE)
rownames(M) <- seq_len(nrow(M))

# Extract countries
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

# Build network
vos_data <- biblioNetwork(M, analysis = "collaboration", network = "countries")
net <- graph_from_adjacency_matrix(vos_data, mode = "undirected", weighted = TRUE, diag = FALSE)

# Write VOSviewer-style Pajek .net
net_file <- "vos_country_collab.net"
con <- file(net_file, "w")

# Write vertices
writeLines(paste0("*Vertices ", vcount(net)), con)
for (i in 1:vcount(net)) {
  label <- V(net)$name[i]
  writeLines(paste(i, shQuote(label)), con)
}

# Write edges (with weights)
writeLines("*Edges", con)
edges <- as_data_frame(net, what = "edges")
for (i in 1:nrow(edges)) {
  from <- match(edges$from[i], V(net)$name)
  to   <- match(edges$to[i], V(net)$name)
  w    <- edges$weight[i]
  writeLines(paste(from, to, w), con)
}
close(con)

cat("✅ Wrote Pajek file:", net_file, "\n")
























library(bibliometrix)
library(igraph)

# Load your cleaned M
load("merged_savedrecs_clean.RData")
M <- as.data.frame(M, stringsAsFactors = FALSE)
rownames(M) <- seq_len(nrow(M))

# Extract countries
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

# Build network
vos_data <- biblioNetwork(M, analysis = "collaboration", network = "countries")
net <- graph_from_adjacency_matrix(vos_data, mode = "undirected", weighted = TRUE, diag = FALSE)

# Write VOSviewer-style Pajek .net
net_file <- "vos_country_collab.net"
con <- file(net_file, "w")

# Write vertices
writeLines(paste0("*Vertices ", vcount(net)), con)
for (i in 1:vcount(net)) {
  label <- V(net)$name[i]
  writeLines(paste(i, shQuote(label)), con)
}

# Write edges (with weights)
writeLines("*Edges", con)
edges <- as_data_frame(net, what = "edges")
for (i in 1:nrow(edges)) {
  from <- match(edges$from[i], V(net)$name)
  to   <- match(edges$to[i], V(net)$name)
  w    <- edges$weight[i]
  writeLines(paste(from, to, w), con)
}
close(con)

cat("✅ Wrote Pajek file:", net_file, "\n")





























USE THIS ONE
--------------------------------------------------
# Load required libraries
library(bibliometrix)
library(igraph)

# Load your cleaned bibliographic data
load("merged_bibliography_all_cleaned.RData")

# Convert to data frame if necessary
M <- as.data.frame(M, stringsAsFactors = FALSE)
rownames(M) <- seq_len(nrow(M))

# Extract country information from authors
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

# Build country collaboration network
vos_data <- biblioNetwork(M, analysis = "collaboration", network = "countries")

# Convert to igraph object
net <- graph_from_adjacency_matrix(vos_data, mode = "undirected", weighted = TRUE, diag = FALSE)

# Prepare the Pajek .net file
net_file <- "vos_country_collab.net"
con <- file(net_file, "w", encoding = "UTF-8")

# Write vertices
writeLines(paste0("*Vertices ", vcount(net)), con)
for (i in seq_len(vcount(net))) {
  label <- V(net)$name[i]
  # Use shQuote to handle spaces and special characters in country names
  writeLines(paste(i, shQuote(label)), con)
}

# Write edges with weights
writeLines("*Edges", con)
edges <- igraph::as_data_frame(net, what = "edges")
for (i in seq_len(nrow(edges))) {
  from <- match(edges$from[i], V(net)$name)
  to   <- match(edges$to[i], V(net)$name)
  w    <- edges$weight[i]
  writeLines(paste(from, to, w), con)
}

# Close the file connection
close(con)

cat("VOSviewer Pajek .net file has been successfully created:", net_file, "\n")
--------------------------------------------------------


























# ========================================
# 📚 Load required libraries
# ========================================
library(tidyr)
library(stringr)
library(bibliometrix)
library(igraph)

# ========================================
# 📝 Load cleaned merged data
# ========================================
load("merged_bibliography_all_cleaned.RData")   # Object: M
M <- as.data.frame(M, stringsAsFactors = FALSE)
rownames(M) <- seq_len(nrow(M))

# ========================================
# 🧰 Helper: Write Pajek .net file
# ========================================
write_pajek <- function(net, file) {
  con <- file(file, "w", encoding = "UTF-8")
  
  # Write vertices
  writeLines(paste0("*Vertices ", vcount(net)), con)
  for (i in seq_len(vcount(net))) {
    label <- V(net)$name[i]
    writeLines(paste(i, shQuote(label)), con)
  }
  
  # Write edges with weights
  writeLines("*Edges", con)
  edges <- igraph::as_data_frame(net, what = "edges")  # ✅ Fixed here
  for (i in seq_len(nrow(edges))) {
    from <- match(edges$from[i], V(net)$name)
    to   <- match(edges$to[i], V(net)$name)
    w    <- edges$weight[i]
    writeLines(paste(from, to, w), con)
  }
  
  close(con)
  cat("✅ Wrote Pajek file:", file, "\n")
}

# ====================================================
# 1. 🏫 Collaboration network of top institutes (Fig. 6)
# ====================================================
M_inst <- metaTagExtraction(M, Field = "AU_UN", sep = ";")

inst_net <- biblioNetwork(M_inst, analysis = "collaboration", network = "universities")
g_inst <- graph_from_adjacency_matrix(inst_net, mode = "undirected", weighted = TRUE, diag = FALSE)

# Keep top 40 institutes by degree
deg <- degree(g_inst, mode = "all")
top40 <- names(sort(deg, decreasing = TRUE))[1:40]
g_inst_top <- induced_subgraph(g_inst, vids = top40)

write_pajek(g_inst_top, "vos_inst_collab.net")

# ====================================================
# 2. 📑 Co-citation network (Fig. 7)
# ====================================================
cocit_mat <- biblioNetwork(M, analysis = "co-citation", network = "references")
g_cocit <- graph_from_adjacency_matrix(cocit_mat, mode = "undirected", weighted = TRUE, diag = FALSE)

# Keep top 500 references by strength (or degree)
strengths <- strength(g_cocit)
top_refs <- names(sort(strengths, decreasing = TRUE))[1:500]
g_cocit_top <- induced_subgraph(g_cocit, vids = top_refs)

write_pajek(g_cocit_top, "vos_cocitation.net")

# ====================================================
# 3. 📝 Keyword co-occurrence network (Fig. 8)
# ====================================================
M_kw <- metaTagExtraction(M, Field = "ID", sep = ";")
M_kw <- metaTagExtraction(M_kw, Field = "DE", sep = ";")

kw_net <- biblioNetwork(M_kw, analysis = "co-occurrences", network = "keywords")
g_kw <- graph_from_adjacency_matrix(kw_net, mode = "undirected", weighted = TRUE, diag = FALSE)

# Filter keywords by frequency (min degree ≥ 5)
deg_kw <- degree(g_kw)
keep_kw <- names(deg_kw[deg_kw >= 5])
g_kw_top <- induced_subgraph(g_kw, vids = keep_kw)

write_pajek(g_kw_top, "vos_keywords.net")















#COMBINED PAJEK
-----------------------------------------------------
# ========================================
# 📚 Load required libraries
# ========================================
library(tidyr)
library(stringr)
library(bibliometrix)
library(igraph)

# ========================================
# 📝 Load cleaned merged data
# ========================================
load("merged_bibliography_all_cleaned.RData")   # Object: M
M <- as.data.frame(M, stringsAsFactors = FALSE)
rownames(M) <- seq_len(nrow(M))

# ========================================
# 🧰 Helper: Write Pajek .net file
# ========================================
write_pajek <- function(net, file) {
  con <- file(file, "w", encoding = "UTF-8")
  
  # Write vertices
  writeLines(paste0("*Vertices ", vcount(net)), con)
  for (i in seq_len(vcount(net))) {
    label <- V(net)$name[i]
    writeLines(paste(i, shQuote(label)), con)
  }
  
  # Write edges with weights
  writeLines("*Edges", con)
  edges <- igraph::as_data_frame(net, what = "edges")
  for (i in seq_len(nrow(edges))) {
    from <- match(edges$from[i], V(net)$name)
    to   <- match(edges$to[i], V(net)$name)
    w    <- edges$weight[i]
    writeLines(paste(from, to, w), con)
  }
  
  close(con)
  cat("✅ Wrote Pajek file:", file, "\n")
}

# ====================================================
# 1. 🌍 Country collaboration network
# ====================================================
M_country <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
country_net <- biblioNetwork(M_country, analysis = "collaboration", network = "countries")
g_country <- graph_from_adjacency_matrix(country_net, mode = "undirected", weighted = TRUE, diag = FALSE)

write_pajek(g_country, "vos_country_collab.net")

# ====================================================
# 2. 🏫 Collaboration network of top institutes (Top 40)
# ====================================================
M_inst <- metaTagExtraction(M, Field = "AU_UN", sep = ";")
inst_net <- biblioNetwork(M_inst, analysis = "collaboration", network = "universities")
g_inst <- graph_from_adjacency_matrix(inst_net, mode = "undirected", weighted = TRUE, diag = FALSE)

# Keep top 40 institutes by degree
deg_inst <- degree(g_inst, mode = "all")
top40 <- names(sort(deg_inst, decreasing = TRUE))[1:40]
g_inst_top <- induced_subgraph(g_inst, vids = top40)

write_pajek(g_inst_top, "vos_inst_collab.net")

# ====================================================
# 3. 📑 Co-citation network (Top 500 references)
# ====================================================
cocit_mat <- biblioNetwork(M, analysis = "co-citation", network = "references")
g_cocit <- graph_from_adjacency_matrix(cocit_mat, mode = "undirected", weighted = TRUE, diag = FALSE)

# Keep top 500 references by strength
strengths <- strength(g_cocit)
top_refs <- names(sort(strengths, decreasing = TRUE))[1:500]
g_cocit_top <- induced_subgraph(g_cocit, vids = top_refs)

write_pajek(g_cocit_top, "vos_cocitation.net")

# ====================================================
# 4. 📝 Keyword co-occurrence network (min degree ≥ 5)
# ====================================================
M_kw <- metaTagExtraction(M, Field = "ID", sep = ";")
M_kw <- metaTagExtraction(M_kw, Field = "DE", sep = ";")
kw_net <- biblioNetwork(M_kw, analysis = "co-occurrences", network = "keywords")
g_kw <- graph_from_adjacency_matrix(kw_net, mode = "undirected", weighted = TRUE, diag = FALSE)

deg_kw <- degree(g_kw)
keep_kw <- names(deg_kw[deg_kw >= 5])
g_kw_top <- induced_subgraph(g_kw, vids = keep_kw)

write_pajek(g_kw_top, "vos_keywords.net")

##########################################################
# 3. Keyword co-occurrence network (Fig. 8)
# ====================================================
M <- metaTagExtraction(M, Field = "ID", sep = ";")
M <- metaTagExtraction(M, Field = "DE", sep = ";")

kw_net <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords")

g_kw <- graph_from_adjacency_matrix(kw_net, mode = "undirected", weighted = TRUE, diag = FALSE)

# Filter keywords by frequency (min degree >= 5)
deg_kw <- degree(g_kw)
keep_kw <- names(deg_kw[deg_kw >= 5])
g_kw_top <- induced_subgraph(g_kw, vids = keep_kw)

write_pajek(g_kw_top, "vos_keywords.net")