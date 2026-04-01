# ====================== 1. 设置工作目录 & 清空环境 ======================
setwd("C:\\Users\\cy\\Desktop\\定量结果")  # 切换到你的数据目录（可根据实际情况修改）
rm(list = ls())  # 清除环境变量

# ====================== 2. 加载所需包 ======================
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)

# ====================== 3. 定义输入文件路径（改为csv） ======================
gene_expr_file_path <- "rpkm.csv"  

# ====================== 4. 定义输出路径（自动创建“生物学重复评估”目录） ======================
output_dir <- "C:\\Users\\cy\\Desktop\\定量结果\\生物学重复评估"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)  # 加recursive=TRUE，支持创建多级目录
}
heatmap_png <- paste0(output_dir, "\\rpkm_correlation_positive_gradient.png")
heatmap_pdf <- paste0(output_dir, "\\rpkm_correlation_positive_gradient.pdf")

# ====================== 5. 读取表达量文件（改为读取csv，修复参数错误） ======================
gene_expr_df <- read.csv(
  file = gene_expr_file_path,
  check.names = FALSE  # 保留原始列名（避免R自动修改特殊字符）
)

# ====================== 6. 关键修复：处理基因ID列和非数值数据 ======================
# 【请根据你的实际列名修改！】这里默认基因ID列是ID/GeneID/gene_id
gene_id_col <- c("ID", "GeneID", "gene_id")
actual_gene_id_col <- intersect(gene_id_col, colnames(gene_expr_df))
if (length(actual_gene_id_col) == 0) {
  stop("未找到基因ID列！请检查rpkm.csv的列名")
}
cat("识别到的基因ID列：", actual_gene_id_col, "\n")

sample_expr_cols <- setdiff(colnames(gene_expr_df), actual_gene_id_col)
if (length(sample_expr_cols) == 0) {
  stop("未找到样本列！请检查rpkm.csv文件")
}
selected_columns <- gene_expr_df[, sample_expr_cols, drop = FALSE]

# ====================== 7. 强制转换为数值型 ======================
convert_to_numeric <- function(x) {
  x[x %in% c("NA", "", " ", "NULL")] <- NA
  num_x <- as.numeric(as.character(x))
  num_x[is.na(num_x)] <- 0
  return(num_x)
}
numeric_columns <- as.data.frame(lapply(selected_columns, convert_to_numeric))
colnames(numeric_columns) <- sample_expr_cols

# ====================== 8. 数据预处理 ======================
log_transformed_data <- numeric_columns %>%
  mutate_all(~ log2(. + 1))

# ====================== 9. 计算相关系数 ======================
cor_matrix <- cor(log_transformed_data)
cor_matrix_melt <- melt(cor_matrix)
colnames(cor_matrix_melt) <- c("Var1", "Var2", "value")

# ====================== 【新增】导出相关系数矩阵为CSV文件 ======================
cor_matrix_csv <- paste0(output_dir, "\\sample_correlation_matrix.csv")
write.csv(cor_matrix, file = cor_matrix_csv, row.names = TRUE, fileEncoding = "UTF-8")
cat("相关系数矩阵已导出至：", cor_matrix_csv, "\n")

# ====================== 10. 绘制热图 ======================
cor_heatmap <- ggplot(cor_matrix_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.1) +
  geom_text(
    aes(label = sprintf("%.2f", value)),
    color = "black", 
    size = 3.5,
    fontface = "bold"
  ) +
  scale_fill_gradientn(
    colors = c("#0000FF","#87CEFA","#FFFFFF","#FFB6C1","#FF0000"),
    values = scales::rescale(c(0.92, 0.94, 0.96, 0.98, 1)),
    limits = c(0.92, 1),
    name = "Pearson\nCorrelation",
    guide = guide_colorbar(barwidth = 1.5, barheight = 15)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  labs(
    title = "Sample RPKM Expression Correlation Heatmap",
    x = "Sample", 
    y = "Sample"
  )

# ====================== 11. 保存图片 ======================
ggsave(heatmap_png, cor_heatmap, width = 12, height = 10, dpi = 300)
ggsave(heatmap_pdf, cor_heatmap, width = 12, height = 10)

# ====================== 12. 输出结果 ======================
cat("====================== 分析完成 ======================\n")
cat("相关系数矩阵：\n")
print(cor_matrix)
cat("\n热图已保存至：\n", heatmap_png, "\n", heatmap_pdf)