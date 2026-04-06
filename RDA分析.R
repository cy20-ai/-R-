rm(list = ls())
# 设置工作目录（替换为你的数据文件所在文件夹路径，斜杠用/或\\）
setwd("C:\\Users\\cy\\Desktop\\生态\\RDA")

# 加载字体配置包，解决绘图中文/字体显示问题
library(showtext)
showtext_auto()
# 导入Times New Roman字体（需本地有对应ttf文件，无则注释此段）
font_add("Times New Roman",
         regular    = "times.ttf",    # 正体
         italic     = "timesi.ttf",   # 斜体
         bold       = "timesbd.ttf",  # 粗体
         bolditalic = "timesbi.ttf")  # 粗斜体

# 加载分析所需核心包
library(vegan)   # 冗余分析(RDA)/DCA分析
library(ggplot2) # 绘图
library(ggrepel) # 避免标签重叠
library(ggtext)  # 支持html格式标签
library(cowplot) # 绘图辅助
library(dplyr)   # 数据处理

# 1. 导入数据（行名设为第1列，需保证3个csv文件与工作目录一致）
otu <- read.csv("RDA_otu.csv", row.names = 1)  # 物种/线虫数据（响应变量）
grp <- read.csv('RDA_group.csv', row.names = 1)# 分组数据（如处理组C/EA/ET）
env <- read.csv("RDA_env.csv", row.names = 1)  # 环境因子（理化指标：pH/SWC/TC等）

# 2. 缺失值处理：检查并将NA替换为0
if (!all(complete.cases(otu))) {
  otu[is.na(otu)] <- 0
}

# 3. 对响应变量做Hellinger转化（vegan包RDA分析专用，消除物种数据异方差）
otu_hellinger <- decostand(otu, method = "hellinger")

# 4. DCA分析：判断适合RDA/CCA，DCA1>4用CCA，≤4用RDA
choose_ordination <- function(otu_data) {
  dca <- decorana(otu_data)  # 做DCA分析
  # 提取DCA1轴的长度
  len <- apply(dca$rproj, 2, function(x) diff(range(x)))["DCA1"]
  # 打印结果并判断排序方法
  cat("DCA1轴长度 =", round(len, 2), "->", ifelse(len > 4, "建议使用CCA", "建议使用RDA"), "\n")
  invisible(len)
}
# 运行DCA判断函数
choose_ordination(otu_hellinger)

# 5. 环境因子共线性检验（VIF>10需剔除高共线性因子，避免结果偏差）
vif.cca(rda(otu_hellinger ~ ., env))

# 6. 执行RDA分析（全环境因子建模，若有高共线性需手动剔除，如~pH+SWC+TC）
rda_result <- rda(otu_hellinger ~ ., env)

# 7. 提取RDA分析结果（物种/样方/环境因子坐标+统计结果）
rda_summary <- append(scores(rda_result), summary(rda_result))

# 8. 制作RDA图统计标签（调整R²+P值，支持html格式，绘图用）
make_rda_label <- function(rda_result) {
  # 提取调整后R²（保留4位小数）
  adj.r2 <- sprintf("%.4f", RsquareAdj(rda_result)$adj.r.squared)
  # 提取RDA整体显著性P值
  pvalue <- anova(rda_result)$`Pr(>F)`[1]
  # 格式化P值显示（<0.001特殊显示，其余保留3位）
  format_p <- function(p) {
    ifelse(p < 0.001, "< 0.001", paste0("= ", sprintf("%.3f", p)))
  }
  # 拼接标签（P<0.05则加粗，支持斜体/上标）
  paste0(
    "<i>adj.R</i><sup>2</sup> = ", adj.r2, "<br>",
    ifelse(pvalue < 0.05, "<b>", ""),
    "<i>P</i> ", format_p(pvalue),
    ifelse(pvalue < 0.05, "</b>", "")
  )
}
# 生成统计标签
label_html <- make_rda_label(rda_result)

# 9. 提取各要素坐标并缩放（仅为绘图美观，不影响分析结果，可按需调整缩放系数）
species_sc <- as.data.frame(rda_summary$species[, 1:2]) * 4   # 物种/线虫坐标（缩放4倍）
samples_sc <- as.data.frame(rda_summary$sites[, 1:2]) * 2.5   # 样方坐标（缩放2.5倍）
env_sc <- as.data.frame(rda_summary$biplot[, 1:2]) * 2.5     # 环境因子坐标（缩放2.5倍）
grp$group <- factor(grp$group, levels = c("CK", "EA", "ET")) # 改位置
color <- c("#F8B6BF", "#A0CBEB", "#F5BE81")

RDA_plot <- ggplot() +
  geom_point(data = samples_sc, aes(x = RDA1, y = RDA2, shape = grp$group, color = grp$group), size = 6, alpha = 0.9) +
  scale_color_manual(values = color) + # 更改颜色
  stat_ellipse(aes(samples_sc$RDA1, samples_sc$RDA2, color = grp$group, group = grp$group),
               level = 0.95, linetype = 2, size = 1, show.legend = F, alpha = 0.9
  ) + ### 注意，是在这里添加椭圆
  geom_segment(data = species_sc, aes(x = 0, y = 0, xend = RDA1, yend = RDA2), arrow = arrow(angle = 22.5, length = unit(0.35, "cm"), type = "closed"), linetype = 1, size = 0.8, colour = "#3C779F") +
  geom_segment(data = env_sc, aes(x = 0, y = 0, xend = RDA1, yend = RDA2), arrow = arrow(angle = 22.5, length = unit(0.35, "cm"), type = "closed"), linetype = 1, size = 1.2, colour = "#A35C72") +
  geom_text_repel(data = species_sc, aes(RDA1, RDA2, label = row.names(species_sc)), max.overlaps = 10, family = "Times New Roman", fontface = "italic", angle = 0, vjust = 0.5, hjust = 0.5, size = 6, colour = "#3C779F") +
  geom_text_repel(data = env_sc, aes(RDA1, RDA2, label = row.names(env_sc)), family = "Times New Roman", vjust = 0.5, size = 8, colour = "#A35C72") +
  labs(
    x = paste("RDA 1 (", sprintf("%.2f", 100 * rda_summary$concont[[1]][2, 1]), "%)", sep = ""),
    y = paste("RDA 2 (", sprintf("%.2f", 100 * rda_summary$concont[[1]][2, 2]), "%)", sep = "")
  ) +
  geom_hline(yintercept = 0, linetype = 3, size = 1) +
  geom_vline(xintercept = 0, linetype = 3, size = 1) +
  guides(
    shape = guide_legend(title = NULL), color = guide_legend(title = NULL),
    fill = guide_legend(title = NULL, direction = "horizontal")
  ) + # 图例横向排列，去除图例标题
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.1, 0.8),
    legend.title = element_text(face = "bold", family = "Times New Roman", colour = "black", size = 15),
    legend.text = element_text(face = "bold", family = "Times New Roman", colour = "black", size = 15),
    plot.margin = unit(c(1, 2, 1, 1), "lines"),
    axis.ticks.length = unit(-0.10, "cm"), # 设置坐标轴刻度线朝内
    axis.title.x = element_text(size = 25, family = "Times New Roman", color = "black", face = "bold", vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(size = 25, family = "Times New Roman", color = "black", face = "bold", vjust = 0.5, hjust = 0.5),
    axis.text.x = element_text(size = 20, family = "Times New Roman", color = "black", face = "bold", angle = 0, vjust = 0.5, hjust = 0.5),
    axis.text.y = element_text(size = 20, family = "Times New Roman", color = "black", face = "bold", vjust = 0.5, hjust = 0.5)
  )+
  annotate(
    "richtext",
    x = 0.6, y = 5,
    label = label_html,
    hjust = 0, vjust = 1,
    size = 6, family = "Times New Roman",
    fill = NA,           # 背景透明
    label.color = NA
  )

RDA_plot
ggsave("RDA_plot_full.pdf", width = 8, height = 6)  # 保存全模型RDA图

envfit <- envfit(rda_result, env, permutations = 1000)
env_stats <- data.frame(
  r2 = envfit$vectors$r,
  p  = envfit$vectors$pvals
)
cor_com <- data.frame(tax = rownames(env_sc), r2 = env_stats[rownames(env_sc), 'r2'], p = env_stats[rownames(env_sc), 'p'])

y_breaks <- round(seq(0, 0.5, length.out = 6), 1)
bar_plot <- ggplot(cor_com, aes(x = reorder(tax, r2), y = r2), size = 2) +
  geom_bar(stat = "identity", width = 0.6, fill = "#CFEAF1") +
  geom_hline(yintercept = y_breaks[-1], color = "gray65", linetype = "solid") +
  # 修改：将 expand = c(0,0) 改为 expansion(mult = c(0, 0.1))，避免星号被裁剪
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = y_breaks) +
  geom_text(aes(y = r2 + 0.05, label = ifelse(p > 0.05, "", "*")), size = 12, fontface = "bold") +
  xlab("Environmental factor") +
  ylab(expression(r^"2")) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(colour = NA),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.y = element_line(linewidth = 1),
    axis.ticks = element_blank(),
    legend.position = "none", # 去除图例
    plot.margin = unit(c(1, 2, 1, 1), "lines"),
    axis.title.x = element_text(family = "Times New Roman", face = "bold", size = 25, colour = "black"),
    axis.title.y = element_text(family = "Times New Roman", face = "bold", size = 25, colour = "black", margin = margin(r = 8)),
    axis.text.x = element_text(size = 20, family = "Times New Roman", colour = "black", vjust = 0.5, hjust = 0.5), # x轴标签字体字号颜色位置
    axis.text.y = element_text(size = 20, family = "Times New Roman", colour = "black", vjust = 0.5, hjust = 0.5, margin = margin(r = 15))
  ) + # y轴标签字体字号颜色位置
  coord_flip()
bar_plot
ggsave("bar_plot_full.pdf", width = 8, height = 6)  # 保存全模型条形图

## ordiR2step前向选择简化模型
(R2all  <- RsquareAdj(rda_result)$adj.r.squared)   # 记录全模型 R²adj
mod.step <- ordiR2step(rda(otu_hellinger ~ 1, env),
                       scope = formula(rda_result),
                       R2scope = R2all,   # 关键：上限
                       direction = "forward",
                       permutations = 1000,
                       trace = TRUE)

plot(mod.step)
rda_summary.step <- append(scores(mod.step), summary(mod.step)) # 查看分析结果
label_html.step <- make_rda_label(mod.step)
species_sc.step <- as.data.frame(rda_summary.step$species[, 1:2]) * 4 # 提取响应变量坐标，乘以4是使图美观，不影响分析,可根据出图结果，对画图数据做一定的放大或缩小，下同
samples_sc.step <- as.data.frame(rda_summary.step$sites[, 1:2]) * 2.5 # 提取样方坐标
env_sc.step <- as.data.frame(rda_summary.step$biplot[, 1:2]) * 2.5 # 提取解释变量坐标
#最简模型绘图
RDA_plot.step <- ggplot() +
  geom_point(data = samples_sc.step, aes(x = RDA1, y = RDA2, shape = grp$group, color = grp$group), size = 6, alpha = 0.9) +
  scale_color_manual(values = color) + # 更改颜色
  stat_ellipse(aes(samples_sc.step$RDA1, samples_sc.step$RDA2, color = grp$group, group = grp$group),
               level = 0.95, linetype = 2, size = 1, show.legend = F, alpha = 0.9
  ) + ### 注意，是在这里添加椭圆
  geom_segment(data = species_sc.step, aes(x = 0, y = 0, xend = RDA1, yend = RDA2), arrow = arrow(angle = 22.5, length = unit(0.35, "cm"), type = "closed"), linetype = 1, size = 0.8, colour = "#3C779F") +
  geom_segment(data = env_sc.step, aes(x = 0, y = 0, xend = RDA1, yend = RDA2), arrow = arrow(angle = 22.5, length = unit(0.35, "cm"), type = "closed"), linetype = 1, size = 1.2, colour = "#A35C72") +
  geom_text_repel(data = species_sc.step, aes(RDA1, RDA2, label = row.names(species_sc.step)), max.overlaps = 10, family = "Times New Roman", fontface = "italic", angle = 0, vjust = 0.5, hjust = 0.5, size = 6, colour = "#3C779F") +
  geom_text_repel(data = env_sc.step, aes(RDA1, RDA2, label = row.names(env_sc.step)), family = "Times New Roman", vjust = 0.5, size = 8, colour = "#A35C72") +
  labs(
    x = paste("RDA 1 (", sprintf("%.2f", 100 * rda_summary.step$concont[[1]][2, 1]), "%)", sep = ""),
    y = paste("RDA 2 (", sprintf("%.2f", 100 * rda_summary.step$concont[[1]][2, 2]), "%)", sep = "")
  ) +
  geom_hline(yintercept = 0, linetype = 3, size = 1) +
  geom_vline(xintercept = 0, linetype = 3, size = 1) +
  guides(
    shape = guide_legend(title = NULL), color = guide_legend(title = NULL),
    fill = guide_legend(title = NULL, direction = "horizontal")
  ) + # 图例横向排列，去除图例标题
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.1, 0.8),
    legend.title = element_text(face = "bold", family = "Times New Roman", colour = "black", size = 15),
    legend.text = element_text(face = "bold", family = "Times New Roman", colour = "black", size = 15),
    plot.margin = unit(c(1, 2, 1, 1), "lines"),
    axis.ticks.length = unit(-0.10, "cm"), # 设置坐标轴刻度线朝内
    axis.title.x = element_text(size = 25, family = "Times New Roman", color = "black", face = "bold", vjust = 0.5, hjust = 0.5),
    axis.title.y = element_text(size = 25, family = "Times New Roman", color = "black", face = "bold", vjust = 0.5, hjust = 0.5),
    axis.text.x = element_text(size = 20, family = "Times New Roman", color = "black", face = "bold", angle = 0, vjust = 0.5, hjust = 0.5),
    axis.text.y = element_text(size = 20, family = "Times New Roman", color = "black", face = "bold", vjust = 0.5, hjust = 0.5)
  )+
  annotate(
    "richtext",
    x = 1, y = 5,
    label = label_html.step,
    hjust = 0, vjust = 1,
    size = 6, family = "Times New Roman",
    fill = NA,           # 背景透明
    label.color = NA
  )
RDA_plot.step
ggsave("RDA_plot_step.pdf", width = 8, height = 6)  # 保存简化模型RDA图

# 置换检验
envfit.step <- envfit(mod.step, env, permutations = 1000)
env_stats.step <- data.frame(
  r2 = envfit.step$vectors$r,
  p  = envfit.step$vectors$pvals
)
cor_com.step <- data.frame(tax = rownames(env_sc.step), r2 = env_stats.step[rownames(env_sc.step), 'r2'], p = env_stats.step[rownames(env_sc.step), 'p'])

y_breaks <- round(seq(0, 0.6, length.out = 4), 1)
bar_plot.step <- ggplot(cor_com.step, aes(x = reorder(tax, r2), y = r2), size = 2) +
  geom_bar(stat = "identity", width = 0.5, fill = "#CFEAF1") +
  geom_hline(yintercept = y_breaks[-1], color = "gray65", linetype = "solid") +
  # 修改：将 expand = c(0,0) 改为 expansion(mult = c(0, 0.1))，避免星号被裁剪
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = y_breaks) +
  geom_text(aes(y = r2 + 0.05, label = ifelse(p > 0.05, "", "*")), size = 12, fontface = "bold") +
  xlab("Environmental factor") +
  ylab(expression(r^"2")) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(colour = NA),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.y = element_line(linewidth = 1),
    axis.ticks = element_blank(),
    legend.position = "none", # 去除图例
    plot.margin = unit(c(1, 2, 1, 1), "lines"),
    axis.title.x = element_text(family = "Times New Roman", face = "bold", size = 25, colour = "black"),
    axis.title.y = element_text(family = "Times New Roman", face = "bold", size = 25, colour = "black", margin = margin(r = 8)),
    axis.text.x = element_text(size = 20, family = "Times New Roman", colour = "black", vjust = 0.5, hjust = 0.5), # x轴标签字体字号颜色位置
    axis.text.y = element_text(size = 20, family = "Times New Roman", colour = "black", vjust = 0.5, hjust = 0.5, margin = margin(r = 15))
  ) + # y轴标签字体字号颜色位置
  coord_flip()
bar_plot.step
ggsave("bar_plot_step.pdf", width = 8, height = 6)  # 保存简化模型条形图

plot_grid(RDA_plot, bar_plot, RDA_plot.step, bar_plot.step,
          nrow = 2, ncol = 2,
          labels = c("(a)", "(b)", "(c)", "(d)"),
          label_size = 18, label_x = 0.01, label_y = 0.99,
          align = "vh")

ggsave("RDA_plot.pdf", width = 18, height = 14) # 保存图片为pdf

