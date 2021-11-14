.ngram <- function(str, n) {
    ret <- list()
    for (i in 1:(length(str) - 1)) {
        ret[[i]] <- c(str[i], str[i + 1])
    }
    ret
}

.template_07 <- function(x, y, z) {
    return(paste(x, "時の", y, "は", z, sep = ""))
}