# GeneCountMerger

This is a simple **preprocessing** tool to merge individual **gene count files** (Eg. output count files from htseq)

**NOTE:** first column must contain the genes. If the gene columns do not match in all files, this tool will not work

* * *

##### **Features**

*   Merge individual sample count files. See **Sample Input Files** below for more details
*   Or merge **multiple matrices**
*   **Convert ensembl gene IDs to gene names**
    *   Option to choose from available genome/versions
    *   If genome/version is not available in the options and you have a [.gtf](https://asia.ensembl.org/info/website/upload/gff.html) file for your genome <a href="">follow these instructions.</a>
*   Option to add **pseudocounts (+1)**
*   **Download** merged counts file in .csv format
*   **Transcriptome Analysis (Optional)** after merging your counts:
    *   Use our **Seurat Wizard** to carry out single-cell RNA analysis
    *   Use **DESeq2** or **START** apps to carry out bulk RNA analysis

![alt text](screenshots/mergeScreenshot.png "Input Data")
