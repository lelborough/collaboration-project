
# Gene plot using Times New Roman
# Email by 06/04/2023

# Packages
pacman::p_load(tidyverse, readxl, showtext, patchwork, latex2exp, ggrepel)

## Add Times New Roman font
font_add(
  family = "times",
  regular = here::here(
    "figs","times.ttf"
  )
)
theme_set(theme_bw())

# Read in data
data <- read_excel(
  here::here(
    "data", "2023-03-01_Gene-Expression-KB.xlsx"
  )
)

## Clean data
# Remove -99 NA value
data <- data %>% filter(gene_expression > 0)
data <- rename(data,c('Concentration'='conc'))

# Format gene line names and treatment names
data$gene_line <- substr(data$gene_line, 4, nchar(data$gene_line))
data <- data %>% 
  mutate(treatment = ifelse(treatment == "Activating_Factor_42","Activating factor 42","Placebo"))

# Manual colour palette
colours <- c("#78A8D1", "#D6C099")

showtext_auto()

# Wild type plot
p1 <- data %>% 
  filter(cell_line == "Wild_type") %>% # wild type only
  ggplot(aes(Concentration, gene_expression, fill = treatment)) + # colour by treatment
  geom_point(col = "black", shape = 21, size = 3) + # black outlines
  geom_label_repel(aes(label = ifelse(Concentration == 10, gene_line,'')),
                   min.segment.length = 0, 
                   nudge_x = 0.8, 
                   show.legend = F,
                   family = "times",
                   size = 20) + # labels for gene line
  labs(x = TeX(r'($\mu$g/ml)'), y = "Gene Expression", fill = "Treatment") + # labels
  scale_fill_manual(values = colours) + # individual colours
  scale_x_continuous(breaks = c(0:10), limits = c(0, 11)) + # x axis
  ggtitle('Wild-type') # title

# Cell type 101 plot
p2 <- data %>% 
  filter(cell_line == "Cell_type_101") %>% # cell type 101 only
  ggplot(aes(Concentration, gene_expression, fill = treatment)) + # colour by treatment
  geom_point(col = "black", shape = 21, size = 3) + # black outlines
  geom_label_repel(aes(label = ifelse(Concentration == 10, gene_line,'')),
                   min.segment.length = 0, 
                   nudge_x = 0.8, 
                   show.legend = F, 
                   family = "times",
                   size = 20) + # labels for gene line
  labs(x = TeX(r'($\mu$g/ml)'), y = "Gene Expression", fill = "Treatment") + 
  ggtitle('Cell-type 101') + # title
  scale_x_continuous(breaks = c(0:10), limits = c(0, 11))+ # x axis
  scale_fill_manual(values = colours) # manual colours

# Combine plots - text size = 60 to save properly in ggsave()
plot <- p1 + p2 + plot_layout(guides = "collect") + # combine plots and combine treatment legend
  plot_annotation(tag_levels = 'A') & 
  theme(legend.position = "bottom", 
        text = element_text(family = "times", size = 60)) # add labels and put legend at bottom


# Uncomment to see plot, text is large so that it can be read in the saved version
# plot


# Save using specified dimensions and file type
ggsave(
  filename = here::here("figures", "2023_04_03-gene_figure.tiff"), 
  plot = plot, 
  width = 9, 
  height = 6,
  units = "in",
  dpi = 500
)