pval_To_3Sig_Digits <- function(pval) {
        if (!is.numeric(pval)) {stop("pval is not numeric")}
        if (pval < 0 || pval > 1) {stop("pval not between 0 and 1")}
        
        if (pval < 0.00001) {
                p.string <- "p < 0.00001"
                } else if (pval < 0.0001) {
                                p.string <- round(pval, digits = 7)
                        } else if (pval < 0.001) {
                                        p.string <- round(pval, digits = 6)
                                } else if (pval < 0.01) {
                                                p.string <- round(pval, digits = 5)
                                        } else if (pval < 0.1) {
                                                p.string <- round(pval, digits = 4)
                                        } else {
                                                p.string <- round(pval, digits = 3)
                                        }
        
        p.string <- as.character(p.string)
        print(p.string)
}