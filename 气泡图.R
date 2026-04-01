# 加载包
library(tidyverse)

# 结果文件路径
kegg_path <- "E:\\index\\KEGG\\论文图表\\KEGG富集结果.csv"
# 图片保存目录
plot_dir <- "E:/index/KEGG/"
# 展示通路数量
show_cat <- 6
kegg_df <- read.csv(kegg_path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)

# 按【差异基因数k】从大到小排序，取前N个通路
top_kegg <- kegg_df %>%
  arrange(desc(差异基因数k)) %>%
  slice_head(n = show_cat)
# 保存PDF
pdf(paste0(plot_dir,"KEGG条形图_Count排序.pdf"), width=10, height=8)
print(
  ggplot(top_kegg, aes(x = 差异基因数k, y = reorder(通路名称, 差异基因数k))) +
    # 条形图核心
    geom_col(aes(fill = 校正后P值), alpha = 0.8) +
    # 蓝红渐变配色
    scale_fill_gradient(low = "red", high = "blue", name = "校正后P值") +
    labs(x = "差异基因数", y = "KEGG通路") +
    theme_bw()
)
dev.off()

# 保存高清PNG
png(paste0(plot_dir,"KEGG条形图_Count排序.png"), width=10, height=8, units="in", res=300)
print(
  ggplot(top_kegg, aes(x = 差异基因数k, y = reorder(通路名称, 差异基因数k))) +
    geom_col(aes(fill = 校正后P值), alpha = 0.8) +
    scale_fill_gradient(low = "red", high = "blue", name = "校正后P值") +
    labs(x = "差异基因数", y = "KEGG通路") +
    theme_bw()
)
dev.off()

cat("条形图绘制完成！按差异基因数排序，已保存至：", plot_dir)

条形图也类似改

