import re
from pathlib import Path

def get_gene_exon_length(annotation_file):
    """
    解析GTF/GFF/GFF3注释文件，计算每个gene_id的外显子总长度（去重后）
    :param annotation_file: 注释文件路径
    :return: dict {gene_id: 外显子总长度（bp）}
    """
    gene_exons = {}  # gene_id -> {chrom: [(start, end), ...]}
    annotation_file = Path(annotation_file)
    if not annotation_file.exists():
        raise FileNotFoundError(f"注释文件不存在：{annotation_file}")
    
    annotation_type = annotation_file.suffix.lower().lstrip('.')
    print(f"[INFO] 正在解析{annotation_type.upper()}文件，计算基因外显子长度...")

    with open(annotation_file, "r", encoding="utf-8") as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            # 跳过注释行和空行
            if not line or line.startswith("#"):
                continue

            parts = line.split("\t")
            if len(parts) < 9:
                print(f"[WARNING] 跳过无效行{line_num}：{line[:50]}...")
                continue

            seqid, source, feature, start, end, score, strand, phase, attributes = parts

            # 只处理外显子（exon）特征
            if feature.lower() != "exon":
                continue

            # 解析gene_id（兼容GTF/GFF/GFF3）
            gene_id = None
            if annotation_type == "gtf":
                # GTF格式：gene_id "LOC_Os01g01010";
                gtf_pattern = re.compile(r'gene_id\s+["\']([^"\';]+)["\';]')
                match = gtf_pattern.search(attributes)
                if match:
                    gene_id = match.group(1)
            elif annotation_type in ["gff", "gff3"]:
                # GFF/GFF3格式：ID=exon1;Parent=transcript1;gene=LOC_Os01g01010
                attr_dict = {}
                for attr in attributes.split(';'):
                    if '=' in attr:
                        k, v = attr.strip().split('=', 1)
                        attr_dict[k.lower()] = v
                if "gene" in attr_dict:
                    gene_id = attr_dict["gene"]
                elif "parent" in attr_dict:
                    # 若无gene字段，用Parent替代（可根据需求调整）
                    gene_id = attr_dict["parent"]

            if not gene_id:
                continue

            # 转换坐标为整数，确保start < end
            try:
                start = int(start)
                end = int(end)
                if start > end:
                    start, end = end, start
            except ValueError:
                print(f"[WARNING] 跳过无效坐标（行{line_num}）：{start}-{end}")
                continue

            # 存储外显子区域（按染色体分组）
            if gene_id not in gene_exons:
                gene_exons[gene_id] = {}
            if seqid not in gene_exons[gene_id]:
                gene_exons[gene_id][seqid] = []
            gene_exons[gene_id][seqid].append((start, end))

    # 合并重叠外显子并计算总长度
    gene_lengths = {}
    for gene_id, chrom_exons in gene_exons.items():
        total_length = 0
        for chrom, exons in chrom_exons.items():
            # 按start排序
            exons_sorted = sorted(exons, key=lambda x: x[0])
            merged = []
            for exon in exons_sorted:
                if not merged:
                    merged.append(exon)
                else:
                    last_s, last_e = merged[-1]
                    curr_s, curr_e = exon
                    # 重叠或相邻则合并
                    if curr_s <= last_e + 1:
                        merged[-1] = (last_s, max(last_e, curr_e))
                    else:
                        merged.append(exon)
            # 累加长度
            total_length += sum(e - s + 1 for s, e in merged)
        gene_lengths[gene_id] = total_length

    print(f"[INFO] 完成！共计算{len(gene_lengths)}个基因的外显子长度")
    return gene_lengths

def read_single_count_file(count_file):
    """
    读取单个count文件，提取gene_id和count（跳过统计行）
    :param count_file: 单个count文件路径
    :return: tuple (count_data: {gene_id: count}, total_reads: 总有效reads数)
    """
    count_file = Path(count_file)
    if not count_file.exists():
        raise FileNotFoundError(f"Count文件不存在：{count_file}")
    
    count_data = {}
    separator_pattern = re.compile(r'\s+')  # 处理空格/制表符分隔
    print(f"[INFO] 正在读取count文件：{count_file}")

    with open(count_file, "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            # 跳过统计行（以_/__开头）和空行
            if not line or line.startswith("_") or line.startswith("__"):
                continue
            parts = separator_pattern.split(line)
            if len(parts) >= 2:
                gene_id = parts[0]
                try:
                    count = int(parts[1])
                except ValueError:
                    count = 0
                count_data[gene_id] = count

    # 计算总有效reads数（所有基因的count之和）
    total_reads = sum(count_data.values())
    print(f"[INFO] 读取到{len(count_data)}个基因的count数据，总有效reads数：{total_reads}")
    return count_data, total_reads

def calculate_rpkm_single(count_data, gene_lengths, total_reads):
    """
    为单个样本计算RPKM
    :param count_data: {gene_id: count}
    :param gene_lengths: {gene_id: length}
    :param total_reads: 总有效reads数
    :return: list of dict，每个dict包含gene_id, count, length, rpkm
    """
    rpkm_list = []
    missing_length_genes = 0
    print(f"[INFO] 正在计算RPKM...")

    for gene_id, count in count_data.items():
        # 获取基因长度，无则设为0
        length = gene_lengths.get(gene_id, 0)
        if length == 0:
            missing_length_genes += 1
            rpkm = 0.0
        else:
            # RPKM公式：(count × 10^9) / (total_reads × length)
            rpkm = (count * 10**9) / (total_reads * length) if total_reads != 0 else 0.0
        # 保留4位小数
        rpkm_list.append({
            "gene_id": gene_id,
            "count": count,
            "length": length,
            "rpkm": round(rpkm, 4)
        })

    if missing_length_genes > 0:
        print(f"[WARNING] {missing_length_genes}个基因无外显子长度数据，RPKM设为0")
    return rpkm_list

def write_rpkm_csv(rpkm_list, output_file="rpkm.csv"):
    """
    写入rpkm.csv文件，列：gene_id,count,length,rpkm
    :param rpkm_list: 包含gene_id/count/length/rpkm的字典列表
    :param output_file: 输出CSV文件名
    """
    with open(output_file, "w", encoding="utf-8", newline="") as f:
        # 写入表头
        f.write("gene_id,count,length,rpkm\n")
        # 写入数据行
        for item in rpkm_list:
            f.write(f"{item['gene_id']},{item['count']},{item['length']},{item['rpkm']}\n")
    print(f"[INFO] RPKM文件已生成：{Path(output_file).absolute()}")

def main():
    # ====================== 请手动修改以下3个路径 ======================
    COUNT_FILE = "Sasanishiki-0h-2_paired.txt"  # 你的单个count文件路径
    ANNOTATION_FILE = "/mnt/e/index/GCF_001433935.1_IRGSP-1.0_genomic.fixed.gtf"  # 你的注释文件路径
    OUTPUT_FILE = "rpkm.csv"  # 输出的rpkm.csv路径
    # ==================================================================

    try:
        # 步骤1：计算基因外显子长度
        gene_lengths = get_gene_exon_length(ANNOTATION_FILE)
        # 步骤2：读取单个count文件
        count_data, total_reads = read_single_count_file(COUNT_FILE)
        # 步骤3：计算RPKM
        rpkm_list = calculate_rpkm_single(count_data, gene_lengths, total_reads)
        # 步骤4：写入CSV文件
        write_rpkm_csv(rpkm_list, OUTPUT_FILE)
        print("\n[SUCCESS] 所有步骤完成！")
    except Exception as e:
        print(f"\n[ERROR] 程序执行失败：{e}")

if __name__ == "__main__":
    main()
