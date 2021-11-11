f00 <- function(str) {
    sapply(
        lapply(strsplit(str, NULL), rev),
        paste,
        collapse = ""
    )
}

f01 <- function(str) {
    ret <- ""
    for (i in seq(1, 7, 2)) {
        ret <- paste(ret, substr(str, i, i), sep = "")
    }
    ret
}

f02 <- function(s1, s2) {
    ret <- ""
    for (i in seq(1, nchar(s1))) {
        ret <- paste(
            ret,
            paste(
                substr(s1, i, i), substr(s2, i, i),
                sep = ""
            ),
            sep = ""
        )
    }
    ret
}

f03 <- function(str) {
    paste(
        lapply(
            lapply(
                strsplit(stringr::str_replace(str, "[,.]", ""), " "),
                nchar
            ),
            as.character
        )[[1]],
        sep = "",
        collapse = ""
    )
}

f04 <- function(str) {
    sp <- strsplit(str, " ")[[1]]
    one_pos <- c(1, 5, 6, 7, 8, 9, 15, 16, 19)
    l <- list(seq(1:length(sp)))

    names <- c()
    for (i in seq(1:length(sp))) {
        s <- ifelse(
            i %in% one_pos,
            strsplit(sp[i], "")[[1]][1],
            paste(strsplit(sp[i], "")[[1]][1:2], collapse = "")
        )
        names <- c(names, s)
    }
    names(l[[1]]) <- names
    l[[1]]
}