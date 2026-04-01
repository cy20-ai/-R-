# 清空环境
rm(list = ls())

# 1. 加载必需包
# 首次运行请先执行：install.packages(c("dplyr","tidyr","ggplot2","ggrepel","readxl"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(readxl)   # 保留 readxl 包，不影响 CSV 读取

# ====================== 2. 路径设置（将输入文件改为 CSV） ======================
gene_rpkm_file_path <- "C:\\Users\\cy\\Desktop\\定量结果\\rpkm.csv"   # 改为 CSV 文件路径
pca_png <- "C:\\Users\\cy\\Desktop\\定量结果\\生物学重复评估\\pca.png"
pca_pdf <- "C:\\Users\\cy\\Desktop\\定量结果\\生物学重复评估\\pca.pdf"

# ====================== 3. 读取 CSV 文件（仅此处修改） ======================
gene_rpkm_df <- read.csv(
  file = gene_rpkm_file_path,
  stringsAsFactors = FALSE   # 保证字符列不转为因子，与 read_excel 行为一致
)

# 验证数据
cat("===== 数据读取成功 =====\n")
cat("基因数量：", nrow(gene_rpkm_df), "\n")
cat("总列数：", ncol(gene_rpkm_df), "\n")
cat("列名：\n")
print(colnames(gene_rpkm_df))

# ====================== 4. 数据预处理（完全不变） ======================
id_column <- "GeneID"
sample_cols <- setdiff(colnames(gene_rpkm_df), id_column)

# 1. 提取样本列 + 转换数值
rpkm_data <- gene_rpkm_df[, sample_cols] %>%
  mutate(across(everything(), as.numeric))

# 2. 自动计算分组样本数，确定低表达过滤阈值N（分组样本数最小值）
sample_names <- sample_cols
sample_group <- case_when(
  grepl("WT", sample_names) ~ "WT",
  grepl("tfc161", sample_names) ~ "tfc161",
  grepl("OE-12", sample_names) ~ "OE-12",
  grepl("OE-2", sample_names) ~ "OE-2",
  TRUE ~ "Other"
)
group_size <- table(sample_group)
N <- min(group_size)
cat("\n===== 低表达过滤参数 =====\n")
cat("分组样本数：\n")
print(group_size)
cat("过滤阈值N（至少在N个样本中表达）：", N, "\n")

# 3. 过滤低表达基因：保留至少N个样本中表达量>0的基因
expressed_count <- rowSums(rpkm_data > 0)
rpkm_filtered_low <- rpkm_data[expressed_count >= N, ]
cat("低表达过滤后剩余基因：", nrow(rpkm_filtered_low), "\n")

# 4. Log2标准化（适配PCA正态分布假设）
log_rpkm <- log2(rpkm_filtered_low + 1)

# 5. 方差筛选：保留前50%方差最高的基因
gene_variance <- apply(log_rpkm, 1, var)  # 计算每个基因的表达方差
top_var_index <- order(gene_variance, decreasing = TRUE)[1:ceiling(length(gene_variance)*0.5)]
log_filtered <- log_rpkm[top_var_index, ]

cat("\n===== 最终用于PCA的基因数据 =====\n")
cat("方差筛选后剩余基因：", nrow(log_filtered), "\n")

# ====================== 5. PCA分析（完全不变，仍使用 scale = FALSE） ======================
pca_result <- prcomp(t(log_filtered), scale. = FALSE, center = TRUE)

# 整理PCA结果
pca_df <- as.data.frame(pca_result$x[, 1:2])
pca_df$SampleID <- rownames(pca_df)

# 分组规则（完全不变）
pca_df <- pca_df %>%
  mutate(Group = case_when(
    grepl("WT", SampleID) ~ "WT",
    grepl("tfc161", SampleID) ~ "tfc161",
    grepl("OE-12", SampleID) ~ "OE-12",
    grepl("OE-2", SampleID) ~ "OE-2",
    TRUE ~ "Other"
  ))

cat("\n===== 样本分组 =====\n")
print(table(pca_df$Group))

# 计算PCA解释度
var_pct <- round(summary(pca_result)$importance[2, 1:2] * 100, 2)

# ====================== 6. 绘制 PCA 图（完全不变） ======================
p <- ggplot(pca_df, aes(PC1, PC2, color = Group)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text_repel(aes(label = SampleID), size = 3.5) +
  labs(
    x = paste0("PC1 (", var_pct[1], "%)"),
    y = paste0("PC2 (", var_pct[2], "%)"),
    title = "Sample PCA Analysis"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# 显示图片
print(p)

# ====================== 7. 保存图片（完全不变） ======================
ggsave(pca_png, p, width = 10, height = 7, dpi = 300)
ggsave(pca_pdf, p, width = 10, height = 7)

cat("\n✅ PCA 分析完成！图片已保存！")