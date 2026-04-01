# 加载包
library(tidyverse)

# 结果文件路径
kegg_path <- "E:/index/KEGG/KEGG富集结果.csv"
# 图片保存目录
plot_dir <- "E:/index/KEGG/"
# 展示通路数量
show_cat <- 6
kegg_df <- read.csv(kegg_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# 按差异基因数k从大到小排序，取前N个通路
top_kegg <- kegg_df %>%
  arrange(desc(差异基因数k)) %>%
  slice_head(n = show_cat)

# ========== 按Count排序绘制条形图 ==========
# 保存PDF
pdf(paste0(plot_dir,"KEGG条形图.pdf"), width=10, height=8)
print(
  ggplot(top_kegg, aes(x = 差异基因数k, y = reorder(通路名称, 差异基因数k))) +
    # 条形图核心
    geom_col(aes(fill = `校正后P值`), alpha = 0.8) +
    #配色：red→#A23B72→#2E86AB + 0.00-0.05标尺
    scale_fill_gradientn(
      colors = colorRampPalette(c("red", "#A23B72", "#2E86AB"))(100),
      limits = c(0.00, 0.05),
      breaks = c(0.00, 0.01, 0.02, 0.03,0.04,0.05),
      name = "adjusted P-value"
    ) +
    scale_x_continuous(
      breaks = seq(0, 60, 5), 
      limits = c(0, 60),
      expand = c(0, 0)  # 消除X轴两端的空白边距
    ) +
    labs(x = "Count", y = "KEGG Pathway") +
    theme_bw() +
    # Y轴文字 加粗+放大
    theme(axis.text.y = element_text(face = "bold", size = 9)) 
)
dev.off()

# 保存高清PNG
png(paste0(plot_dir,"KEGG条形图.png"), width=10, height=8, units="in", res=300)
print(
  ggplot(top_kegg, aes(x = 差异基因数k, y = reorder(通路名称, 差异基因数k))) +
    geom_col(aes(fill = `校正后P值`), alpha = 0.8) +
    # red→#A23B72→#2E86AB + 0.00-0.05标尺
    scale_fill_gradientn(
      colors = colorRampPalette(c("red", "#A23B72", "#2E86AB"))(100),
      limits = c(0.00, 0.05),
      breaks = c(0.00, 0.01, 0.02, 0.03,0.04,0.05),
      name ="adjusted P-value"
    ) +
    # 删除左右两侧多余小格强制显示60刻度
    scale_x_continuous(
      breaks = seq(0, 60, 5), 
      limits = c(0, 60),
      expand = c(0, 0)  # 消除X轴两端的空白边距
    ) +
    labs(x = "Count", y = "KEGG Pathway") +
    theme_bw() +
    # Y轴文字 加粗+放大
    theme(axis.text.y = element_text(face = "bold", size = 9))
)
dev.off()

cat("条形图绘制完成！按差异基因数排序，已保存至：", plot_dir)

