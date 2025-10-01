#Code for generating cluster similarity heatmap

library(igraph)
library(pheatmap)

vertex_lines <- readLines("Vertices-only.TXT")
vertex_lines <- vertex_lines[vertex_lines != ""]
vertex_lines <- vertex_lines[-1]  # remove header

vertices <- data.frame(id=integer(), name=character(),
                       x=numeric(), y=numeric(),
                       stringsAsFactors=FALSE)

for(line in vertex_lines){
  first_space <- regexpr(" ", line)
  id <- as.integer(substr(line, 1, first_space-1))
  
  quote_pos <- gregexpr('"', line)[[1]]
  name <- substr(line, quote_pos[1]+1, quote_pos[2]-1)
  
  rest <- trimws(substr(line, quote_pos[2]+1, nchar(line)))
  coords <- as.numeric(strsplit(rest, "\\s+")[[1]])
  x <- coords[1]
  y <- coords[2]
  
  vertices <- rbind(vertices,
                    data.frame(id=id, name=name, x=x, y=y, stringsAsFactors=FALSE))
}

if(any(duplicated(vertices$name))){
  vertices$name <- paste0(vertices$name, "_", vertices$id)
}

edge_lines <- readLines("Edges-only.TXT")
edge_lines <- edge_lines[edge_lines != ""]
edge_lines <- edge_lines[-1]  # remove header

edges <- data.frame(from=integer(), to=integer(), weight=numeric(),
                    stringsAsFactors=FALSE)
for(line in edge_lines){
  parts <- strsplit(line, "\\s+")[[1]]
  edges <- rbind(edges,
                 data.frame(from=as.integer(parts[1]),
                            to=as.integer(parts[2]),
                            weight=as.numeric(parts[3]),
                            stringsAsFactors=FALSE))
}

edges$from_name <- vertices$name[match(edges$from, vertices$id)]
edges$to_name   <- vertices$name[match(edges$to, vertices$id)]

g <- graph_from_data_frame(d = data.frame(from=edges$from_name,
                                          to=edges$to_name,
                                          weight=edges$weight),
                           vertices = vertices[, c("name","x","y")],
                           directed = FALSE)

cl <- cluster_louvain(g, weights = E(g)$weight)
V(g)$cluster <- membership(cl)

cluster_sizes <- sort(table(V(g)$cluster), decreasing = TRUE)
top5_clusters <- as.numeric(names(cluster_sizes)[1:5])

edges$cluster_from <- V(g)$cluster[match(edges$from_name, V(g)$name)]
edges$cluster_to   <- V(g)$cluster[match(edges$to_name, V(g)$name)]

cluster_sims <- aggregate(weight ~ cluster_from + cluster_to,
                          data = edges, FUN = mean)

all_clusters <- sort(unique(V(g)$cluster))
sim_matrix <- matrix(0, nrow=length(all_clusters), ncol=length(all_clusters))
rownames(sim_matrix) <- as.character(all_clusters)
colnames(sim_matrix) <- as.character(all_clusters)

for(i in 1:nrow(cluster_sims)){
  cf <- as.character(cluster_sims$cluster_from[i])
  ct <- as.character(cluster_sims$cluster_to[i])
  val <- cluster_sims$weight[i]
  sim_matrix[cf, ct] <- val
  sim_matrix[ct, cf] <- val
}

top5_char <- as.character(top5_clusters)
sim_matrix_top5 <- sim_matrix[top5_char, top5_char, drop=FALSE]

cluster_labels <- paste0("C", 1:5)
rownames(sim_matrix_top5) <- cluster_labels
colnames(sim_matrix_top5) <- cluster_labels

glow_palette <- colorRampPalette(c("#fefefe", "#7a7a7a"))(100)

sim_matrix_scaled <- (sim_matrix_top5 / max(sim_matrix_top5))^1.5 * max(sim_matrix_top5)

tiff(filename="Top5_Cluster_Similarity_Heatmap.tiff", width=1800, height=1600, res=200)
pheatmap(sim_matrix_scaled,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         display_numbers = TRUE,
         number_format = "%.2f",
         color = glow_palette,
         border_color = NA,
         main = "Top 5 Cluster Similarity Scores",
         fontsize_number = 18,   # Larger similarity score numbers
         fontsize_row = 16,      # Larger row labels (C1-C5)
         fontsize_col = 16,      # Larger column labels (C1-C5)
         legend = FALSE)
dev.off()

