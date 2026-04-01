# 设置工作目录（保留原路径）
setwd("E:\\DESeq2")

# 清空环境变量
rm(list = ls())

# 加载所需包
library(DESeq2)
library(dplyr)
library(readxl)  # 新增：读取XLSX文件

# ========== 关键修改1：添加创建输出文件夹的代码，避免路径不存在报错 ==========
output_dir <- "E:\\DESeq2\\WT_VS_tfc161DEG_list"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("创建输出文件夹：", output_dir, "\n")
}

# 设置输入文件路径
gene_count_path <- "E:\\DESeq2\\WT_VS_tfc161.csv"

# 设置输出文件路径（完全不变）
Dse2_result_path <- "E:\\DESeq2\\WT_VS_tfc161DEG_list\\01.Dse2_result.csv"
TvsC_deg_all_path <- "E:\\DESeq2\\WT_VS_tfc161DEG_list\\02.Deg_all.csv"
TvsC_deg_down_path <- "E:\\DESeq2\\WT_VS_tfc161DEG_list\\03.Deg_down.csv"
TvsC_deg_up_path <- "E:\\DESeq2\\WT_VS_tfc161DEG_list\\04.Deg_up.csv"


# 读取CSV文件
gene_count <- read.csv(gene_count_path, header = TRUE, row.names = NULL, check.names = FALSE)

# 提取样本名称
sample_group <- "sample_group6.csv"
sample_group_data <- read.csv(sample_group, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
sample_list <- sample_group_data$sample_name  # 关键修改：数字1 → 字母l

# 提取分组名称
groups <- unique(sample_group_data$group_name) # 假设“group”是分组信息的列名
treatment_group <- groups[1]
control_group <- groups[2]

# 筛选出gene_id列和样本名称列（使用修正后的sample_list）
selected_columns <- c("gene_id", sample_list)
gene_count_selected <- gene_count[, selected_columns]

# 将gene_count_selected数据框的gene_id作为行名
rownames(gene_count_selected) <- gene_count_selected$gene_id
gene_count_selected$gene_id <- NULL

# 构建colData
colData <- data.frame(report_sample = sample_group_data$sample_name,
                      group = sample_group_data$group)

# 并将group列转换为因子
colData$group <- as.factor(colData$group)

# 替换NA值为0
gene_count_selected[is.na(gene_count_selected)] <- 0

# 创建DESeqDataset对象
dds <- DESeqDataSetFromMatrix(countData = gene_count_selected,
                              colData = colData,
                              design = ~ group)
# 执行DESeq分析
dds <- DESeq(dds)

# 获取归一化后的count值
norm_counts <- counts(dds, normalized=TRUE)

# 取消行名
norm_counts <- data.frame(gene_id = rownames(norm_counts), norm_counts, row.names = NULL)

# 提取出差异分析结果pvalue,log2FoldChange
res <- results(dds, contrast = c("group", treatment_group, control_group)) # 注意这里修改分组

# 提取所需的统计量（清理冗余代码，保留核心逻辑）
res_table <- as.data.frame(res)
# 提取 padj,p-value,log2Foldchange（仅保留一次，删除重复代码）
res_table <- res_table[, c("log2FoldChange", "pvalue", "padj")]

# 取消行名
res_table <- data.frame(gene_id = rownames(res_table), res_table, row.names = NULL)

# 使用merge函数合并数据框
merged_diff <- merge(norm_counts, res_table, by = "gene_id", all = TRUE)

# 写入文件
write.csv(merged_diff, Dse2_result_path, row.names = FALSE)

# 以下是设置差异基因的筛选条件
# 重新读取Dse2_result，清除掉其中pvalue和padj为空的行
Dse2_result <- read.csv(Dse2_result_path, header = TRUE, check.names = FALSE)
Dse2_result <- Dse2_result[!is.na(Dse2_result$pvalue), ]
Dse2_result <- Dse2_result[!is.na(Dse2_result$padj), ]

# 创建TvsC_deg_all文件，pvalue<0.05且|log2Foldchange|>1为差异基因
TvsC_deg_all <- Dse2_result[Dse2_result$pvalue < 0.05 & abs(Dse2_result$log2FoldChange) > 1, ]
# ========== 关键修改2：修正变量名（a11 → all），解决“找不到对象”错误 ==========
write.csv(TvsC_deg_all, TvsC_deg_all_path, row.names = FALSE)

# 创建TvsC_deg_down文件，TvsC_deg_all中log2Foldchange<-1的部分为下调基因
TvsC_deg_down <- TvsC_deg_all[TvsC_deg_all$log2FoldChange < -1, ]
write.csv(TvsC_deg_down, TvsC_deg_down_path, row.names = FALSE)

# 创建TvsC_deg_up文件，TvsC_deg_all中log2Foldchange>1的部分为上调基因
TvsC_deg_up <- TvsC_deg_all[TvsC_deg_all$log2FoldChange > 1, ]
# ========== 关键修改3：使用修正后的变量名TvsC_deg_up_path（统一大小写） ==========
write.csv(TvsC_deg_up, TvsC_deg_up_path, row.names = FALSE)

cat("差异基因分析完成，结果已保存至", output_dir, "\n")

