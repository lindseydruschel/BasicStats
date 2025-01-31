# Load required libraries
library(DESeq2)
library(readxl)
library(dplyr)
library(writexl)

# Set file path
file_path <- "C:/Users/druschel/Downloads/FPKM NO D2No5.xlsx"

# Load data from Excel
df <- read_excel(file_path)

# Extract raw count columns
count_columns <- grep("_count$", colnames(df), value = TRUE)  # Identify count columns
count_data <- df %>% select(gene_name, all_of(count_columns)) # Select count data only

# Remove rows with missing gene names
count_data <- count_data %>% filter(!is.na(gene_name))

# Ensure unique gene names
count_data$gene_name <- make.unique(as.character(count_data$gene_name))

# Convert to matrix while preserving gene names
count_matrix <- as.matrix(count_data[,-1])
rownames(count_matrix) <- count_data$gene_name  

## Filter out low-count genes (at least 10 total counts across all samples)
#keep <- rowSums(count_matrix) >= 10
#count_matrix <- count_matrix[keep, ]
# got closer vals without filtering - keeping this out

# Define sample groups
group <- factor(c(rep("D2", sum(grepl("D2", count_columns))), rep("D4", sum(grepl("D4", count_columns)))))

# Create colData (metadata) for DESeq2
col_data <- data.frame(row.names = colnames(count_matrix), group = group)

# Create DESeq2 dataset
dds <- DESeqDataSetFromMatrix(countData = count_matrix, colData = col_data, design = ~group)

# Run DESeq2 normalization and differential expression analysis
dds <- DESeq(dds)

# Extract results with D4 as baseline (D2 vs. D4)
res <- results(dds, contrast = c("group", "D2", "D4"))  # D4 is baseline

# Convert to data frame and add gene names
res_df <- as.data.frame(res)
res_df$gene_name <- rownames(res_df)

# Apply filtering criteria: |log2FC| ≥ 1 & padj ≤ 0.05
res_filtered <- res_df %>% filter(abs(log2FoldChange) >= 1 & padj <= 0.05)

# Merge results with original dataset
df_deseq2 <- df %>% left_join(res_df, by = "gene_name")

# Ensure FDR and log2FC are numeric
df$padj <- as.numeric(df$padj)
df$log2FoldChange <- as.numeric(df$log2FoldChange)
res_df$padj <- as.numeric(res_df$padj)
res_df$log2FoldChange <- as.numeric(res_df$log2FoldChange)

# Define significance thresholds
fdr_threshold <- 0.05
log2fc_threshold <- 1

# Identify significant genes in the original dataset
original_significant_genes <- df %>%
  filter(padj < fdr_threshold & abs(log2FoldChange) >= log2fc_threshold) %>%
  select(gene_name)

# Identify significant genes in the newly calculated DESeq2 dataset
calculated_significant_genes <- res_df %>%
  filter(padj < fdr_threshold & abs(log2FoldChange) >= log2fc_threshold) %>%
  select(gene_name)

# Find overlapping significant genes
shared_genes <- intersect(original_significant_genes$gene_name, calculated_significant_genes$gene_name)

# Identify unique genes in each dataset
unique_to_original <- setdiff(original_significant_genes$gene_name, calculated_significant_genes$gene_name)
unique_to_calculated <- setdiff(calculated_significant_genes$gene_name, original_significant_genes$gene_name)

# Compare original and calculated log2FC & p-values
comparison_df <- df %>%
  select(gene_name, log2FoldChange, padj) %>%
  rename(Original_log2FC = log2FoldChange, Original_padj = padj) %>%
  left_join(res_df %>% select(gene_name, log2FoldChange, padj), by = "gene_name") %>%
  rename(Calculated_log2FC = log2FoldChange, Calculated_padj = padj)

# Save shared, unique genes, and log2FC comparison in an Excel file
write_xlsx(
  list(
    DESeq2_Results = df_deseq2,
    Filtered_DEGs = res_filtered,
    Shared_Genes = data.frame(Shared_Genes = shared_genes),
    Unique_to_Original = data.frame(Unique_to_Original = unique_to_original),
    Unique_to_Calculated = data.frame(Unique_to_Calculated = unique_to_calculated),
    Log2FC_Comparison = comparison_df
  ), 
  "C:/Users/druschel/Downloads/DESeq2_gene_comparison.xlsx"
)

# Print summary statistics
cat("Total Genes Analyzed:", nrow(res_df), "\n")
cat("Significant DE Genes (FDR < 0.05 & |log2FC| ≥ 1):", nrow(res_filtered), "\n")
cat("Shared Significant Genes:", length(shared_genes), "\n")
cat("Unique to Original:", length(unique_to_original), "\n")
cat("Unique to Calculated:", length(unique_to_calculated), "\n")

# Optional: MA Plot
plotMA(res, main = "DESeq2 MA Plot", ylim = c(-5, 5))
