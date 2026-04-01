library(ggplot2)
library(dplyr)

ego_result <- read.csv("E:\\index\\GO\\GO_论文标准结果.csv", 
                       stringsAsFactors = FALSE, 
                       check.names = FALSE)  # 强制保留原始列名，不报错

if(nrow(ego_result) > 0){
  # 总气泡图：横轴改为GeneRatio，调大气泡大小
  p1 <- ego_result %>%
    arrange(pvalue) %>%  # 按显著性排序，保留原逻辑
    slice_head(n = 15) %>% # 取前15个，保留原逻辑
    # y轴改为GeneRatio，Description按GeneRatio排序
    ggplot(aes(x = reorder(Description, GeneRatio), y = GeneRatio)) +
    # 保留size=Count，新增scale_size_continuous调大尺寸
    geom_point(aes(size = Count, color = pvalue)) + 
    coord_flip() + # 保留横向展示
    # 调大气泡大小范围，range可根据需求调整
    scale_size_continuous(range = c(4, 16), name = "Count") +
    scale_color_gradient(low = "red", high = "blue", name = "pvalue") + # 保留颜色逻辑，加图例名
    # y轴标签改为GeneRatio
    labs(title = "GO富集总气泡图", x = "", y = "GeneRatio") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p1)
  ggsave("GO总气泡图.png", p1, width = 12, height = 8, dpi = 300)
  # BP单独气泡图
  if(sum(ego_result$Cluster == "BP") > 0){
    p2 <- ego_result %>%
      filter(Cluster == "BP") %>%
      arrange(pvalue) %>% slice_head(n=10) %>%
      ggplot(aes(x=reorder(Description,Count), y=Count)) +
      geom_point(aes(size=Count, color=pvalue)) + coord_flip() +
      labs(title="BP富集气泡图",x="",y="Count") + theme_bw()
    print(p2)
    ggsave("GO_BP气泡图.png",p2,width=12,height=8,dpi=300)
  }
  
  # CC单独气泡图
  if(sum(ego_result$Cluster == "CC") > 0){
    p3 <- ego_result %>%
      filter(Cluster == "CC") %>%
      arrange(pvalue) %>% slice_head(n=10) %>%
      ggplot(aes(x=reorder(Description,Count), y=Count)) +
      geom_point(aes(size=Count, color=pvalue)) + coord_flip() +
      labs(title="CC富集气泡图",x="",y="Count") + theme_bw()
    print(p3)
    ggsave("GO_CC气泡图.png",p3,width=12,height=8,dpi=300)
  }
  
  # MF单独气泡图
  if(sum(ego_result$Cluster == "MF") > 0){
    p4 <- ego_result %>%
      filter(Cluster == "MF") %>%
      arrange(pvalue) %>% slice_head(n=10) %>%
      ggplot(aes(x=reorder(Description,Count), y=Count)) +
      geom_point(aes(size=Count, color=pvalue)) + coord_flip() +
      labs(title="MF富集气泡图",x="",y="Count") + theme_bw()
    print(p4)
    ggsave("GO_MF气泡图.png",p4,width=12,height=8,dpi=300)
  }
  
  # 条形图（完全不变）
  p_bar <- ego_result %>%
    arrange(desc(Count)) %>% slice_head(n=15) %>%
    ggplot(aes(x = Count, y = reorder(Description, Count))) +
    geom_bar(stat = "identity", aes(fill = pvalue)) +
    scale_fill_gradientn(colors=c("#2E86AB","#A23B72","red")) +
    labs(title = "GO富集条形图", x = "Count", y = "") +
    theme_minimal()
  
  print(p_bar)
  ggsave("GO富集条形图.png", p_bar, width = 12, height = 8, dpi = 300)
  
} else {
  cat("⚠️ 无显著富集结果\n")
}

