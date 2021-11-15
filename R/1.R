.ngram <- function(str, n) {
    ret <- list()
    for (i in 1:(length(str) - 1)) {
        ret[[i]] <- c(str[i], str[i + 1])
    }
    ret
}

.template_07 <- function(x, y, z) {
    paste(x, "時の", y, "は", z, sep = "")
}

.conv <- function(x) {
    unlist(
        lapply(x, function(y) {
            strtoi(charToRaw(y), 16L)
        })
    )
}

cipher <- function(str) {
    .cipher <- function(c) {
        if (.conv(c) %in% .conv("a"):.conv("z")) {
            return(rawToChar(as.raw(219 - .conv(c))))
        } else {
            return(c)
        }
    }
    sp <- unlist(strsplit(str, ""))
    ret <- c()
    for (i in seq(1, length(sp))) {
        ret <- c(ret, .cipher(sp[i]))
    }
    paste(ret, collapse = "")
}