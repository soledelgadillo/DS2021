num.genes <- nrow(ma)
genes <- ma$gene

PROBEvec <- ma[[1]]
p.value <- numeric(num.genes)
fold.change <- numeric(num.genes)

for (k in 1:num.genes) {
        NCvec <- unlist(ma[k, c(3:6)])
        WGvec <- unlist(ma[k, c(7:11)])
        
        pval <- t.test(NCvec, WGvec)$p.value
        p.value[k] <- pval
        WG.over.NC.fold.change <- 2^(mean(WGvec)-mean(NCvec))
        fold.change[k] <- WG.over.NC.fold.change
}

analysis.results <- data.frame(PROBEvec, genes, p.value, fold.change)
colnames <- c("Illumina_PROBE_ID", "gene", "two-sided-pvalue", "WG/NC-fold-change")
