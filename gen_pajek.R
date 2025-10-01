#Code for generating pajek files for clusters/network plot generation in VOSViewer
library(tidyr)
library(stringr)
library(bibliometrix)
library(igraph)

load("merged_bibliography_all_cleaned.RData") #Supplementary dataset
M <- as.data.frame(M, stringsAsFactors = FALSE)
rownames(M) <- seq_len(nrow(M))

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

M_country <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
country_net <- biblioNetwork(M_country, analysis = "collaboration", network = "countries")
g_country <- graph_from_adjacency_matrix(country_net, mode = "undirected", weighted = TRUE, diag = FALSE)

write_pajek(g_country, "vos_country_collab.net")

M_inst <- metaTagExtraction(M, Field = "AU_UN", sep = ";")
inst_net <- biblioNetwork(M_inst, analysis = "collaboration", network = "universities")
g_inst <- graph_from_adjacency_matrix(inst_net, mode = "undirected", weighted = TRUE, diag = FALSE)

# Keep top 40 institutes by degree
deg_inst <- degree(g_inst, mode = "all")
top40 <- names(sort(deg_inst, decreasing = TRUE))[1:40]
g_inst_top <- induced_subgraph(g_inst, vids = top40)

write_pajek(g_inst_top, "vos_inst_collab.net")

cocit_mat <- biblioNetwork(M, analysis = "co-citation", network = "references")
g_cocit <- graph_from_adjacency_matrix(cocit_mat, mode = "undirected", weighted = TRUE, diag = FALSE)

# Keep top 500 references by strength
strengths <- strength(g_cocit)
top_refs <- names(sort(strengths, decreasing = TRUE))[1:500]
g_cocit_top <- induced_subgraph(g_cocit, vids = top_refs)

write_pajek(g_cocit_top, "vos_cocitation.net")

M_kw <- metaTagExtraction(M, Field = "ID", sep = ";")
M_kw <- metaTagExtraction(M_kw, Field = "DE", sep = ";")
kw_net <- biblioNetwork(M_kw, analysis = "co-occurrences", network = "keywords")
g_kw <- graph_from_adjacency_matrix(kw_net, mode = "undirected", weighted = TRUE, diag = FALSE)

deg_kw <- degree(g_kw)
keep_kw <- names(deg_kw[deg_kw >= 5])
g_kw_top <- induced_subgraph(g_kw, vids = keep_kw)

write_pajek(g_kw_top, "vos_keywords.net")

M <- metaTagExtraction(M, Field = "ID", sep = ";")
M <- metaTagExtraction(M, Field = "DE", sep = ";")

kw_net <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords")

g_kw <- graph_from_adjacency_matrix(kw_net, mode = "undirected", weighted = TRUE, diag = FALSE)

# Filter keywords by frequency (min degree >= 5)
deg_kw <- degree(g_kw)
keep_kw <- names(deg_kw[deg_kw >= 5])
g_kw_top <- induced_subgraph(g_kw, vids = keep_kw)


write_pajek(g_kw_top, "vos_keywords.net")
