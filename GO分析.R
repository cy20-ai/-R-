library(clusterProfiler)
library(dplyr)
library(readr)
library(ggplot2)

# ====================== 固定文件配置 ======================
term2gene_path <- "E:\\index\\GO\\anno.csv"
deg_csv     <- "E:\\index\\GO\\diffgenelist.csv"

# 1. 读取数据
gene2go <- read.csv(term2gene_path, header = FALSE, stringsAsFactors = FALSE) %>%
  select(1:4)
colnames(gene2go) <- c("GO_raw", "GeneID", "ontology", "desc")

# 2. 添加GO前缀
gene2go <- gene2go %>% mutate(GO = paste0("GO:", GO_raw))

# 3. 读取差异基因
deg_df <- read.csv(deg_csv, header = FALSE)
deg_all <- deg_df[[1]]

universe_genes <- unique(gene2go$GeneID)
deg_filtered <- intersect(deg_all, universe_genes)
# 表达基因集
universe_genes <- unique(c(deg_filtered, universe_genes))

# ====================== 基础统计 ======================
cat("背景有效基因数：", length(universe_genes), "\n")
cat("输入差异基因数：", length(deg_all), "\n")
cat("可映射差异基因数：", length(deg_filtered), "\n")

if(length(deg_filtered) > 0){
  go_types <- c("BP", "CC", "MF")
  ego_list <- list()
  
  for(ont in go_types){
    # 筛选GO：保留基因数 5~300 的标准条目（合规质控）
    gene2go_sub <- gene2go %>%
      filter(ontology == ont) %>%
      add_count(GO) %>%
      filter(n >=5 & n <=300)
    
    if(nrow(gene2go_sub)==0) next
    
    t2g <- gene2go_sub[, c("GO", "GeneID")]
    t2n <- gene2go_sub[, c("GO", "desc")]
    
    ego <- enricher(
      gene = deg_filtered,
      TERM2GENE = t2g,
      TERM2NAME = t2n,
      universe = universe_genes,
      pAdjustMethod = "BH",
      pvalueCutoff = 0.05,
      qvalueCutoff = 0.2
    )
    ego_list[[ont]] <- ego
  }
  
  # 合并结果
  ego_all <- clusterProfiler::merge_result(ego_list)
  ego_result <- as.data.frame(ego_all)
  
  # 输出统计
  cat("====================== 结果统计 ======================\n")
  cat("BP显著条目：", nrow(as.data.frame(ego_list$BP)), "\n")
  cat("CC显著条目：", nrow(as.data.frame(ego_list$CC)), "\n")
  cat("MF显著条目：", nrow(as.data.frame(ego_list$MF)), "\n")
  cat("总显著条目：", nrow(ego_result), "\n")
  
  write.csv(ego_result, "GO_论文标准结果.csv", row.names = FALSE)
}

