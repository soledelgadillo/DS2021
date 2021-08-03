# columns.to.keep <- c(1, 2, 4, 6)
# annotation.df <- annotation.df[, columns.to.keep]

# annot.rows is a vector of row numbers such that for each row r of the analysis.results
# data frame; annot.rows[r] will contain the row of the annotation.df data that 
# has the same Illumina Probe_ID as does row r of analysis.results
n_probes <- nrow(analysis.results)
annot.rows <- numeric(n_probes)
annotation.df_probes <- annotation.df[, 1]

for (r in 1:n_probes) {
        probe_id <- analysis.results[r, 1]
        m <- which(probe_id == annotation.df_probes)
        if (length(m) != 1) stop("did not find unique matching probe id row")
        annot.rows[r] <- m
}
# Then use cbind to bind annotation.df[annot.rows, ] to analysis.results

analysis.results_with_annotation <- cbind(analysis.results, annotation.df[annot.rows, ])