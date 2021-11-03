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
