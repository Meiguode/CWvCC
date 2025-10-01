#TREEMAP Figure
library(readxl)
library(treemapify)
library(dplyr)
library(stringr)
library(ggplot2)

df_raw <- read_excel("Research_Cateogries.XLSX", sheet = 1, col_names = TRUE)
names(df_raw) <- make.names(names(df_raw))
df <- df_raw %>% select(Name, Number)
df$Number <- as.numeric(gsub(",", "", df$Number))

name_wrap_width <- 18
df <- df %>%
  mutate(label_name = str_wrap(Name, width = name_wrap_width),
         label_value = format(Number, big.mark = ","))

excel_cols <- c("#5B9BD5","#ED7D31","#A5A5A5","#FFC000",
                "#4472C4","#70AD47","#255E91","#9C27B0")
if(nrow(df) > length(excel_cols)) {
  excel_cols <- colorRampPalette(excel_cols)(nrow(df))
}

# Order largest first
df <- df %>% arrange(desc(Number))

p <- ggplot(df, aes(area = Number, fill = factor(Name), label = label_name)) +
  geom_treemap(color = "white", size = 1.2) +   # thicker borders = visible spacing
  geom_treemap_text(aes(label = label_value),
                    colour = "white", fontface = "bold",
                    place = "topright",
                    size = 14,    # noticeably bigger
                    reflow = TRUE,
                    grow = FALSE) +
  geom_treemap_text(aes(label = label_name),
                    colour = "white", fontface = "bold",  # made bold too
                    place = "bottomleft",
                    size = 16,    # noticeably bigger
                    reflow = TRUE,
                    grow = FALSE,
                    min.size = 3) +
  scale_fill_manual(values = excel_cols, guide = "none") +
  theme(legend.position = "none") +
  scale_y_reverse(expand = c(0,0))   # flip vertically

print(p)

ggsave("treemap.tiff", plot = p, width = 10, height = 7, units = "in", dpi = 300, device = "tiff")

++
