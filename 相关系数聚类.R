# ====================== 0. 清空环境 & 加载包 ======================
rm(list = ls())
library(pheatmap)
library(dplyr)
library(ggplot2)

# ====================== 1. 定义输入/输出路径 ======================
input_file <- "C:\\Users\\cy\\Desktop\\定量结果\\生物学重复评估\\sample_correlation_matrix.csv"
output_dir <- "C:\\Users\\cy\\Desktop\\定量结果\\生物学重复评估"
heatmap_png <- file.path(output_dir, "correlation_clustered_heatmap_improved.png")
heatmap_pdf <- file.path(output_dir, "correlation_clustered_heatmap_improved.pdf")

# ====================== 2. 读取相关系数矩阵 ======================
cor_mat <- read.csv(
  file = input_file,
  row.names = 1,          
  check.names = FALSE     
)
cor_mat <- as.matrix(cor_mat)  

# ====================== 3. 自动生成样本分组注释 ======================
sample_names <- rownames(cor_mat)
group_info <- case_when(
  grepl("^WT_", sample_names) ~ "WT",
  grepl("^tfc161_", sample_names) ~ "tfc161",
  grepl("^OE-12_", sample_names) ~ "OE-12",
  grepl("^OE-2_", sample_names) ~ "OE-2",
  TRUE ~ "Other"
)
annocol <- data.frame(Group = group_info, row.names = sample_names)
group_colors <- c(
  WT = "#3498DB",
  tfc161 = "#E74C3C",
  `OE-12` = "#27AE60",
  `OE-2` = "#F1C40F"
)
annotation_colors <- list(Group = group_colors)

# ====================== 4. 绘制 PNG（单元格数字变大、变深） ======================
pheatmap(
  mat = cor_mat,
  annotation_col = annocol,
  annotation_row = annocol,
  annotation_colors = annotation_colors,
  
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  clustering_method = "complete",
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  
  color = colorRampPalette(c("#2E86AB", "white", "#A23B72"))(100),
  breaks = seq(0.92, 1, length.out = 101),
  legend_breaks = c(0.92, 0.94, 0.96, 0.98, 1.00),
  legend_labels = c("0.92", "0.94", "0.96", "0.98", "1.00"),
  
  fontsize_row = 10,
  fontsize_col = 10,
  angle_col = 0,
  
  cellheight = 20,
  cellwidth = 50,
  margins = c(10, 10),
  border_color = NA,
  
  display_numbers = TRUE,
  number_format = "%.3f",
  fontsize_number = 8,          # 增大数字字号
  number_color = "black",        # 显式黑色（最深）
  
  legend = TRUE,
  width = 12,
  height = 5,
  res = 600,
  
  main = "Sample Correlation Clustered Heatmap",
  filename = heatmap_png
)

# ====================== 5. 绘制 PDF（同样修改） ======================
pheatmap(
  mat = cor_mat,
  annotation_col = annocol,
  annotation_row = annocol,
  annotation_colors = annotation_colors,
  
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  clustering_method = "ward.D2",
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  
  color = colorRampPalette(c("#2E86AB", "white", "#A23B72"))(100),
  breaks = seq(0.92, 1, length.out = 101),
  legend_breaks = c(0.92, 0.94, 0.96, 0.98, 1.00),
  legend_labels = c("0.92", "0.94", "0.96", "0.98", "1.00"),
  
  fontsize_row = 10,
  fontsize_col = 10,
  angle_col = 0,
  
  cellheight = 20,
  cellwidth = 50,
  margins = c(10, 10),
  border_color = NA,
  
  display_numbers = TRUE,
  number_format = "%.3f",
  fontsize_number = 8,          # 增大数字字号
  number_color = "black",        # 显式黑色
  
  legend = TRUE,
  width = 12,
  height = 5,
  
  main = "Sample Correlation Clustered Heatmap",
  filename = heatmap_pdf
)

# ====================== 6. 完成提示 ======================
cat("====================== 分析完成 ======================\n")
cat("热图已保存至：\n", heatmap_png, "\n", heatmap_pdf)