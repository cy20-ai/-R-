#!/bin/bash

# ====================== 配置参数（根据实际情况修改）======================
# 1. 输入：上一步Hisat2比对生成的SAM文件目录（即之前的OUTPUT_DIR）
INPUT_SAM_DIR="/mnt/e/alignment"
# 2. 输出：表达量计数结果的输出目录
OUTPUT_COUNT_DIR="/mnt/e/expression_counts"
# 3. 参考注释文件路径（GTF/GFF3格式，必须与参考基因组对应）
GTF_FILE="/mnt/e/index/huayaoindex.gtf"  # 替换为你的GTF文件实际路径
# 4. 线程数（HTSeq的--nprocesses参数，可选）
THREADS=6
# 5. 链特异性参数（关键！根据你的测序建库类型选择：no/yes/reverse）
#    - no：非链特异性测序（最常用，若不确定先选no）
#    - yes：链特异性测序（正向）
#    - reverse：链特异性测序（反向）
STRANDNESS="no"
# 6. SAM文件后缀（与上一步输出一致）
SAM_SUFFIX=".sam"

# ====================== 核心逻辑 ======================
# 创建输出目录（不存在则自动生成）
mkdir -p "${OUTPUT_COUNT_DIR}"

# 循环遍历INPUT_SAM_DIR下的所有SAM文件
for sam_file in "${INPUT_SAM_DIR}"/*${SAM_SUFFIX}; do
    # 跳过非文件（如目录）或空匹配（当目录下无SAM文件时）
    [ -f "$sam_file" ] || continue

    # 提取样本名（移除路径和SAM后缀，例如：/mnt/e/alignment/sample.sam → sample）
    sample=$(basename "$sam_file" "${SAM_SUFFIX}")
    # 定义输出count文件路径
    count_out="${OUTPUT_COUNT_DIR}/${sample}_counts.txt"

    echo "===== 开始处理：$sample ====="
    # 核心：HTSeq-count 处理双端SAM文件，统计基因表达量
    # 参数说明：
    # -f sam：输入文件格式为SAM
    # -r pos：按基因组位置排序（Hisat2输出的SAM是按read名排序，pos表示按位置，不影响结果但HTSeq会提示，可改为name）
    # -s：链特异性参数（根据建库类型调整）
    # --nprocesses：线程数
    # 最后两个参数：输入SAM文件、参考GTF文件
    htseq-count -f sam -r name -s "${STRANDNESS}" --nprocesses "${THREADS}" "${sam_file}" "${GTF_FILE}" > "${count_out}"

    # 检查当前样本是否处理成功
    if [ $? -eq 0 ]; then
        echo "✅ 成功：${count_out} 已生成"
    else
        echo "❌ 失败：$sample 处理出错"
    fi
    echo "----------------------------------------"
done

echo "所有样本处理完毕！结果在：${OUTPUT_COUNT_DIR}"
