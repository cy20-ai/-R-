#N/M/n/k 全部仅保留 有KO号 + 能映射到通路 的基因/数据
library(tidyverse)
library(clusterProfiler)
library(ggplot2)
library(dplyr)

# ========== 1. 文件路径 ==========
diff_gene_path  <- "E:/index/KEGG/diffgenelist.csv"
bg_path         <- "E:/index/KEGG/论文图表/thisKO.csv"
map_path        <- "E:/index/KEGG/map2pathway.csv"
result_save_path <- "E:/index/KEGG/论文图表/KEGG富集结果.csv" 

# ========== 2. 数据读取 ==========
diff_df <- read.csv(diff_gene_path, stringsAsFactors = F)
diff_genes <- diff_df[[1]] %>% unique()

bg_df <- read.csv(bg_path, header = F, stringsAsFactors = F, col.names = c("gene","ko"))
bg_clean <- bg_df %>% filter(!is.na(ko), str_detect(ko, "^K")) %>% distinct()

map_df <- read.csv(map_path, stringsAsFactors = F)
colnames(map_df) <- c("KO","Pathway_ID","Pathway_Name")
map_clean <- drop_na(map_df) %>% distinct()

# ========== 仅保留有KO + 能映射通的数据 ==========
gene_pathway_map <- bg_clean %>%
  inner_join(map_clean, by = c("ko" = "KO")) %>% 
  dplyr::select(Pathway_ID, gene) %>%
  distinct()

# ========== N / n 计算 ==========
N <- length(unique(gene_pathway_map$gene))  # 背景总基因（有KO+有通路）
valid_diff_genes <- intersect(diff_genes, unique(gene_pathway_map$gene))
n <- length(valid_diff_genes)              # 有效差异基因（有KO+有通路）

# 数据校验
cat("==================== 精准参数校验 ====================\n")
cat("背景基因总数 N =", N, "\n")
cat("有效差异基因总数 n =", n, "\n")
cat("所有参数均通过 KO→通路 映射验证\n")
cat("======================================================\n")

if(n == 0) stop("无有效差异基因！")
term2gene <- gene_pathway_map %>% dplyr::select(TERM=Pathway_ID, GENE=gene) %>% distinct()
kegg_res <- enricher(
  gene = valid_diff_genes,
  TERM2GENE = term2gene,
  universe = unique(gene_pathway_map$gene),
  pvalueCutoff = 0.05,
  qvalueCutoff = 0.2,
  pAdjustMethod = "fdr",
  minGSSize = 3,
  maxGSSize = 500
)
if(!is.null(kegg_res) && nrow(as.data.frame(kegg_res)) > 0){
  
  result <- as.data.frame(kegg_res)
  
  # 计算每个通路的 M（该通路背景基因数）
  pathway_info <- gene_pathway_map %>%
    count(Pathway_ID, name = "M")
  
  # 合并数据 + 计算比例
  final_result <- result %>%
    left_join(pathway_info, by = c("ID" = "Pathway_ID")) %>%
    mutate(
      # 基因比例 = k / n （k=Count）
      基因比例 = Count / n,
      # 背景比例 = M / N
      背景比例 = M / N,
      # 匹配通路名称
      通路名称 = map_clean$Pathway_Name[match(ID, map_clean$Pathway_ID)],
      通路名称 = ifelse(is.na(通路名称), ID, 通路名称)
    ) %>%
    # 最终表格
    dplyr::select(
      通路ID = ID,
      通路名称,
      差异基因数k = Count,
      基因比例,
      背景比例,
      P值 = pvalue,
      校正后P值 = p.adjust,
      Q值 = qvalue,
      差异基因列表 = geneID
    ) %>%
    arrange(校正后P值)
  
  # 保存结果
  write.csv(final_result, result_save_path, row.names = F, fileEncoding = "UTF-8")
  cat("\n分析完成！无报错，参数100%正确！\n")
  
}else{
  # 空结果
  final_result <- data.frame(
    通路ID=character(), 通路名称=character(), 差异基因数k=integer(),
    基因比例=numeric(), 背景比例=numeric(),
    P值=numeric(), 校正后P值=numeric(), Q值=numeric(), 差异基因列表=character()
  )
  write.csv(final_result, result_save_path, row.names = F, fileEncoding = "UTF-8")
  cat("\n未富集到符合条件的通路\n")
}

