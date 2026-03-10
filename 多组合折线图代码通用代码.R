library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(scales)

# 读取并预处理数据（与原逻辑完全一致）
raw_data <- read_csv(
  file = "C:/Users/cy/Desktop/论文/数据/SR.csv",
  col_names = TRUE,
  show_col_types = FALSE
)

date_columns <- setdiff(colnames(raw_data), "type")

long_data <- raw_data %>%
  pivot_longer(cols = all_of(date_columns), names_to = "sampling_date", values_to = "soil_r_value") %>%
  filter(!is.na(soil_r_value)) %>%
  mutate(
    type = factor(type, levels = unique(raw_data$type)),
    sampling_date = factor(sampling_date, levels = date_columns)
  )

# 分组统计（自动计算Mean、std、n、SE）
statistics <- long_data %>%
  group_by(type, sampling_date) %>%
  summarise(
    mean_soil_r = mean(soil_r_value, na.rm = TRUE),
    std_soil_r = sd(soil_r_value, na.rm = TRUE),
    repeat_count = n(),
    se_soil_r = std_soil_r / sqrt(repeat_count),
    .groups = "drop"
  )

# 新增：查看数据范围（可选，用于调试，可删除）
cat("土壤温度均值范围：\n")
cat("最小值：", min(statistics$mean_soil_r, na.rm = TRUE), "\n")
cat("最大值：", max(statistics$mean_soil_r, na.rm = TRUE), "\n")
# 考虑误差棒后的整体范围（更精准适配y轴）
# ========== 关键修复：给se_soil_r加上statistics$，指定所属数据集 ==========
y_min <- min(statistics$mean_soil_r - statistics$se_soil_r, na.rm = TRUE)
y_max <- max(statistics$mean_soil_r + statistics$se_soil_r + 10, na.rm = TRUE)
cat("考虑误差棒后的y轴范围：", y_min, "~", y_max, "\n")

# 保存统计数据为CSV文件
write_csv(
  x = statistics,
  file = "C:/Users/cy/Desktop/论文/整合数据/mean SR Std数据.csv"
)

# 绘制图表（核心：y轴自动适配数据范围）
final_plot <- ggplot(
  statistics,
  aes(
    x = sampling_date,
    y = mean_soil_r,
    group = type,
    color = type,
    shape = type
  )
) +
  geom_point(size = 3, fill = NA, stroke = 1.1) +
  geom_line(linewidth = 1.2) +
  geom_errorbar(
    aes(ymin = mean_soil_r - se_soil_r, ymax = mean_soil_r + se_soil_r),
    width = 0.08,
    linewidth = 0.3,
    na.rm = TRUE
  ) +
  labs(
    x = "Date",
    # 使用expression实现上标格式（m⁻² s⁻¹）
    y = expression(Soil~R~(mmol~m^-2~s^-1)),
    color = "Type",
    shape = "Type"
  ) +
  # 核心修改：y轴自动适配数据范围，保留间隔1.00和两位小数
  scale_y_continuous(
    # 方案1：完全自动适配（包含误差棒的整体范围）
    # limits = c(floor(y_min), ceiling(y_max)),
    # 方案2：强制y轴从0开始，上限自动适配（科研图表常用，推荐）
    limits = c(0, ceiling(y_max)),
    # 动态生成刻度：间隔1.00，匹配数据范围
    breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1.00),
    labels = number_format(accuracy = 0.01),  # 保留两位小数
    expand = c(0, 0)  # 移除y轴上下的空白
  ) +
  scale_color_manual(
    values = c(
      "MP" = "#ff7f0e",
      "WP" = "#1f77b4",
      "DG" = "#2ca02c",
      "GS" = "#d62728"
    ),
    labels = c("MP", "WP", "DG", "GS")
  ) +
  scale_shape_manual(
    values = c("MP" = 16, "WP" = 15, "DG" = 17, "GS" = 18),
    labels = c("MP", "WP", "DG", "GS")
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12, color = "black", face = "plain"),
    # y轴刻度文字size为10
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 13, color = "black", face = "bold"),
    legend.title = element_text(size = 12, color = "black", face = "bold"),
    legend.text = element_text(size = 11, color = "black"),
    legend.position = "top",
    legend.box = "horizontal",
    plot.subtitle = element_text(size = 10, color = "gray60", hjust = 0),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", linewidth = 1),
    # 可选：设置字体支持特殊符号（进一步避免编码问题）
    # text = element_text(family = "Arial Unicode MS")  # Windows系统推荐
  )

# 保存图片
ggsave(
  filename = "C:/Users/cy/Desktop/论文/图片/多组合折线图SR.png",
  plot = final_plot,
  width = 14,
  height = 7,
  dpi = 300,
  device = "png",
  bg = "white"
)

ggsave(
  filename = "C:/Users/cy/Desktop/论文/图片/多组合折线图SR.pdf",
  plot = final_plot,
  width = 14,
  height = 7,
  device = "pdf",
  bg = "white"
)

# 预览图表
print(final_plot)