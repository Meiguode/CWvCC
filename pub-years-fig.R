#Code to generate plot of publication per year figure
library(openxlsx)
library(ggplot2)
library(cowplot)
library(dplyr)
library(grid)

data1 <- read.xlsx("general_climate_years_py.xlsx")
colnames(data1) <- c("Year", "Publications")
data1$Cumulative <- cumsum(data1$Publications)
data1$Type <- "General Climate Change"

data2 <- read.xlsx("conti_climate_years_py.xlsx")
colnames(data2) <- c("Year", "Publications")
data2$Cumulative <- cumsum(data2$Publications)
data2$Type <- "CWvCC"

make_plot <- function(data, bar_color, line_color, show_x = TRUE) {
  # Normalize cumulative to fit on same axis as bars
  scale_factor <- max(data$Publications) / max(data$Cumulative)

  p <- ggplot(data, aes(x = factor(Year))) +
    geom_col(aes(y = Publications, fill = "Annual Publications"), 
             width = 0.6, alpha = 0.8) +
    geom_line(aes(y = Cumulative * scale_factor, color = "Cumulative Publications"),
              size = 0.9, group = 1) +
    geom_point(aes(y = Cumulative * scale_factor, color = "Cumulative Publications"),
               size = 1.2) +
    scale_y_continuous(
      name = NULL,  # Left y-axis label added separately
      sec.axis = sec_axis(~ . / scale_factor, name = NULL)
    ) +
    scale_x_discrete(breaks = data$Year) +
    scale_fill_manual(
      name = NULL,
      values = c("Annual Publications" = bar_color),
      labels = ifelse(unique(data$Type) == "CWvCC",
                      "Annual Publications in CWvCC",
                      "Annual Publications in General Climate Change")
    ) +
    scale_color_manual(
      name = NULL,
      values = c("Cumulative Publications" = line_color),
      labels = ifelse(unique(data$Type) == "CWvCC",
                      "Cumulative publications for CWvCC",
                      "Cumulative publications for General Climate Change")
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(size = 13),
      axis.title.y.right = element_text(size = 13),
      plot.title = element_blank(),
      legend.position = c(0.02, 0.98),   # Top-left corner
      legend.justification = c(0, 1),
      legend.box = "vertical",
      legend.text = element_text(size = 10, lineheight = 0.8)  # Compact legend
    )

  if (!show_x) {
    p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
  return(p)
}

deep_purple <- "#5E3C99"

p_top <- make_plot(data2, deep_purple, "red", show_x = TRUE)
p_bottom <- make_plot(data1, "gray60", "black", show_x = TRUE)

y_left_label <- ggdraw() + 
  draw_label("Annual publication number", 
             fontface = 'bold', 
             angle = 90, 
             size = 14)

y_right_label <- ggdraw() + 
  draw_label("Cumulative", 
             fontface = 'bold', 
             angle = -90, 
             size = 14)

combined_plots <- plot_grid(p_top, p_bottom, ncol = 1, align = "v", axis = "lr")

combined <- plot_grid(
  y_left_label, 
  combined_plots, 
  y_right_label,
  ncol = 3, 
  rel_widths = c(0.05, 1, 0.05)
)

ggsave("climate_vs_conti_publications.svg",
       plot = combined, width = 13, height = 8, units = "in", dpi = 300,
       device = "svg", bg = "white")

ggsave("climate_vs_conti_publications.tiff",
       plot = combined, width = 13, height = 8, units = "in", dpi = 600,
       device = "tiff", bg = "white")

p_top_single <- p_top +
  labs(y = "Annual publication number") +
  scale_y_continuous(
    name = "Annual publication number",
    sec.axis = sec_axis(~ . * max(data2$Cumulative) / max(data2$Publications),
                        name = "Cumulative")
  )

ggsave("conti_publications_top_plot.svg",
       plot = p_top_single, width = 12, height = 4, units = "in", dpi = 300,
       device = "svg", bg = "white")

ggsave("conti_publications_top_plot.tiff",
       plot = p_top_single, width = 12, height = 4, units = "in", dpi = 600,
       device = "tiff", bg = "white")

