library(pheatmap)
rm(list = ls())  
setwd("C:\\Users\\cy\\Desktop\\定量结果")
rm(list = ls())

# ====================== 2. 定义文件======================
expr_path    <- "rpkm.csv"  # 替换为你的RPKM实际文件名
group_path   <- "group.csv"
output_dir   <- "C:\\Users\\cy\\Desktop\\定量结果\\表达模式热图"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("创建输出文件夹：", output_dir, "\n")
}

pdf_output   <- file.path(output_dir, "基因表达模式图.pdf")
png_output   <- file.path(output_dir, "基因表达模式图.png")

# ====================== 3. 读取表达矩阵（RPKM数据） ======================
expr_matrix <- read.csv(
  file = expr_path,
  row.names = 1,        
  header = TRUE,
  sep = ",",            
  check.names = FALSE   
)
expr_matrix <- as.matrix(expr_matrix)
expr_matrix <- expr_matrix[rowSums(expr_matrix) > 0, ]

# ====================== 4. 读取分组注释 ======================
sample_annot <- read.csv(
  file = group_path,
  row.names = 1,        
  header = TRUE,
  sep = ",",
  check.names = FALSE
)
sample_annot <- as.data.frame(sample_annot)

# ====================== 5. 匹配样本 ======================
common_samples <- intersect(colnames(expr_matrix), rownames(sample_annot))
expr_matrix    <- expr_matrix[, common_samples]
sample_annot   <- sample_annot[common_samples, , drop = FALSE]

# ====================== 6. 自定义颜色 ======================
heat_colors <- colorRampPalette(c("#2C7FB8", "#FFFFFF", "#D7301F"))(200)
annot_colors <- list(
  Group = c(
    A = "#3498DB",
    B = "#E74C3C",
    C = "#27AE60",
    D = "#FFFF00"
  )
)

# ====================== 7. 绘制热图（列聚类，使用complete方法） ======================
pheatmap(
  mat = expr_matrix,
  filename = pdf_output,
  width = 11,
  height = 8,
  scale = "row",
  cluster_rows = TRUE,
  cluster_cols = TRUE,                      # 改为 TRUE，开启列聚类
  clustering_method_rows = "ward.D2",       # 基因行聚类使用 ward.D2
  clustering_method_cols = "complete",      # 样本列聚类使用 complete
  annotation_col = sample_annot,
  annotation_colors = annot_colors,
  show_rownames = FALSE,
  show_colnames = TRUE,
  color = heat_colors,
  breaks = seq(-3, 3, length.out = 201),
  treeheight_row = 25,
  treeheight_col = 20,
  fontsize = 9,
  border_color = NA,
  main = "Hierarchical clustering heatmap of gene expression patterns",
  angle_col = 0
)

pheatmap(
  mat = expr_matrix,
  filename = png_output,
  width = 11,
  height = 8,
  res = 300,
  scale = "row",
  cluster_rows = TRUE,
  cluster_cols = TRUE,                      # 改为 TRUE，开启列聚类
  clustering_method_rows = "ward.D2",       # 基因行聚类使用 ward.D2
  clustering_method_cols = "complete",      # 样本列聚类使用 complete
  annotation_col = sample_annot,
  annotation_colors = annot_colors,
  show_rownames = FALSE,
  show_colnames = TRUE,
  color = heat_colors,
  breaks = seq(-3, 3, length.out = 201),
  treeheight_row = 25,
  treeheight_col = 20,
  fontsize = 9,
  border_color = NA,
  main = "Hierarchical clustering heatmap of gene expression patterns",
  angle_col = 0
)

cat("热图已生成！\n")
cat("PDF路径：", pdf_output, "\n")
cat("PNG路径：", png_output, "\n")