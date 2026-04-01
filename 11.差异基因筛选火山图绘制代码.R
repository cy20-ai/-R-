
setwd("E:\\DESeq2") 
library(ggplot2)rm(list = ls())  
library(dplyr)

# 1. 定义文件路径（输出文件添加分组标识，更规范）
deseq2_result_path <- "WT_VS_tfc161DEG_list\\01.Dse2_result.csv"
output_dir <- "E:\\DESeq2\\火山图1"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE) 
volcano_pdf_path <- file.path(output_dir, "WT_VS_tfc161_volcano.pdf")
volcano_png_path <- file.path(output_dir, "WT_VS_tfc161_volcano.png")

# 2. 读取数据（保留逗号分隔，确保列拆分正确）
data <- read.csv(
  deseq2_result_path, 
  header = TRUE,
  check.names = FALSE,  # 保留原始列名
  sep = ","
)

# 3. 查看数据列名 + 手动指定核心列（更稳定，避免自动匹配出错）
cat("数据列名：\n")
print(colnames(data))

# 【关键可选】：如果有padj列，建议用padj（校正后p值，更严格），否则用pvalue
# p_col <- "padj"  # 推荐使用，注释掉则用pvalue
p_col <- "pvalue"       
fc_col <- "log2FoldChange"  # 手动指定差异倍数列，确保准确

# 4. 数据清洗（核心：过滤NA、极端值，确保计算正常）
data <- data %>%
  # 过滤p值和差异倍数的NA行
  filter(!is.na(.data[[p_col]]), !is.na(.data[[fc_col]])) %>%
  # 过滤p值=0的情况（避免-log10(0)出现Inf，导致图形异常）
  filter(.data[[p_col]] > 0) %>%
  # 提前计算纵坐标，便于后续检查
  mutate(y = -log10(.data[[p_col]]))

# 打印核心数据的统计信息，确认分布合理
cat("\n", p_col, "列的数值统计：\n")
print(summary(data[[p_col]]))
cat("\n纵坐标（-log10(", p_col, ")）的统计信息：\n")
print(summary(data$y))

# 5. 基因分类（筛选规则：p<0.05 且 |log2FC|>3，与竖线保持一致）
data$color <- case_when(
  data[[p_col]] < 0.05 & data[[fc_col]] > 1 ~ "up",    # 上调
  data[[p_col]] < 0.05 & data[[fc_col]] < -1 ~ "down",  # 下调
  TRUE ~ "no"                                           # 无差异
)

# 6. 统计分类数量（查看结果）
color_counts <- table(data$color)
cat("\n基因分类数量：\n")
print(color_counts)

# 拼接图例标签（带数量，更直观）
color_labels <- c(
  up = paste0("Up (", color_counts["up"], ")"),
  down = paste0("Down (", color_counts["down"], ")"),
  no = paste0("No (", color_counts["no"], ")")
)

# 7. 绘制火山图（核心优化视觉效果）
volcano_plot <- ggplot(data, aes(x = .data[[fc_col]], y = y, color = color)) +
  # 优化散点：增大尺寸、降低透明度，减少重叠
  geom_point(alpha = 0.7, size = 1.8) +  
  # 显著性线：加粗+黑色虚线，更醒目（竖线对应±3，与分类规则一致）
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_vline(xintercept = c(-3, 3), linetype = "dashed", color = "black", linewidth = 0.8) +
  # 自定义颜色：更美观的配色方案
  scale_color_manual(
    values = c(
      "up" = "#E74C3C",       # 亮红色（上调）
      "down" = "#2C7FB8",     # 深蓝色（下调，替代普通blue）
      "no" = "#BDC3C7"        # 浅灰色（无差异，更柔和）
    ),
    breaks = c("up", "down", "no"),
    labels = color_labels
  ) +
  # 轻微扩展纵坐标范围，避免点贴边（替代强制ylim，更自然）
  expand_limits(y = c(0, max(data$y) + 0.5)) +
  # 简洁主题
  theme_minimal() +
  # 标签设置（优化图例标题，更清晰）
  labs(
    x = "log2(Fold Change)",
    y = paste0("-log10(", p_col, ")"),
    color = paste0(p_col, " < 0.05\n|log2(FC)| > 1"),
    title = "Volcano Plot of DEGs (WT_VS_tfc161)"
  ) +
  # 主题优化：增大字体、调整图例位置、简化背景
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # 标题加粗+增大
    axis.title = element_text(size = 14),  # 坐标轴标签增大
    axis.text = element_text(size = 12),   # 坐标轴刻度增大
    legend.position = "right",             # 图例位置
    legend.title = element_text(size = 12),# 图例标题字号
    legend.text = element_text(size = 11), # 图例标签字号
    panel.grid = element_blank(),          # 移除网格线，背景更干净
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6)  # 边框加粗
  )

# 8. 保存图片（增大尺寸，高分辨率，白色背景）
ggsave(volcano_pdf_path, plot = volcano_plot, width = 10, height = 8, dpi = 300, bg = "white")
ggsave(volcano_png_path, plot = volcano_plot, width = 10, height = 8, dpi = 300, bg = "white")

# 输出提示
cat("\n火山图已保存至：\n")
cat("PDF：", volcano_pdf_path, "\n")
cat("PNG：", volcano_png_path, "\n")

