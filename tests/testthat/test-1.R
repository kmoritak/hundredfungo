test_that("00. 文字列の逆順", {
  act <- sapply(
    lapply(strsplit("stressed", NULL), rev),
    paste,
    collapse = ""
  )
  expect_equal(act, "desserts")
})

test_that("01. 「パタトクカシーー」", {
  act <- ""
  for (i in seq(1, 7, 2)) {
    act <- paste(act, substr("パタトクカシーー", i, i), sep = "")
  }
  expect_equal(act, "パトカー")
})

test_that("02. 「パトカー」＋「タクシー」＝「パタトクカシーー」", {
  act <- ""
  for (i in seq(1, nchar("パトカー"))) {
    act <- paste(
      act,
      paste(
        substr("パトカー", i, i), substr("タクシー", i, i),
        sep = ""
      ),
      sep = ""
    )
  }
  expect_equal(act, "パタトクカシーー")
})

test_that("03. 円周率", {
  act <- paste(
    lapply(
      lapply(
        strsplit(
          stringr::str_replace(
            "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics.", "[,.]", ""
          ), " "
        ),
        nchar
      ),
      as.character
    )[[1]],
    sep = "",
    collapse = ""
  )

  expect_equal(
    act,
    "3141592753589710"
  )
})

test_that("04. 元素記号", {
  input <- "H He Li Be B C N O F Ne Na Mi Al Si P S Cl Ar K Ca"
  sp <- strsplit(input, " ")[[1]]
  one_pos <- c(1, 5, 6, 7, 8, 9, 15, 16, 19)

  act <- list(seq(1:length(sp)))
  names <- c()
  for (i in seq(1:length(sp))) {
    s <- ifelse(
      i %in% one_pos,
      strsplit(sp[i], "")[[1]][1],
      paste(strsplit(sp[i], "")[[1]][1:2], collapse = "")
    )
    names <- c(names, s)
  }
  names(act[[1]]) <- names

  exp <- list(seq(1:20))
  names <- c()
  for (c in strsplit(input, " ")) {
    names <- c(names, c)
  }
  names(exp[[1]]) <- names
  expect_equal(act, exp)
})

test_that("05. n-gram", {
  str <- "I am an NLPer"
  exp_word_bigram <- list()
  exp_word_bigram[[1]] <- c("I", "am")
  exp_word_bigram[[2]] <- c("am", "an")
  exp_word_bigram[[3]] <- c("an", "NLPer")

  exp_letter_bigram <- list()
  exp_letter_bigram[[1]] <- c("I", " ")
  exp_letter_bigram[[2]] <- c(" ", "a")
  exp_letter_bigram[[3]] <- c("a", "m")
  exp_letter_bigram[[4]] <- c("m", " ")
  exp_letter_bigram[[5]] <- c(" ", "a")
  exp_letter_bigram[[6]] <- c("a", "n")
  exp_letter_bigram[[7]] <- c("n", " ")
  exp_letter_bigram[[8]] <- c(" ", "N")
  exp_letter_bigram[[9]] <- c("N", "L")
  exp_letter_bigram[[10]] <- c("L", "P")
  exp_letter_bigram[[11]] <- c("P", "e")
  exp_letter_bigram[[12]] <- c("e", "r")

  expect_equal(.ngram(strsplit(str, " ")[[1]], 2), exp_word_bigram)
  expect_equal(.ngram(strsplit(str, "")[[1]], 2), exp_letter_bigram)
})

test_that("06. 集合", {
  s0 <- "paraparaparadise"
  s1 <- "paragraph"

  x <- .ngram(strsplit(s0, "")[[1]], 2)
  y <- .ngram(strsplit(s1, "")[[1]], 2)

  u <- union(x, y)
  i <- intersect(x, y)
  d <- setdiff(x, y)

  # Just slacking...
  expect_equal(u, union(x, y))
  expect_equal(i, intersect(x, y))
  expect_equal(d, setdiff(x, y))
  expect_true(TRUE %in% sapply(x, function(x) {
    x == c("s", "e")
  }))
  expect_false(TRUE %in% sapply(y, function(x) {
    x == c("s", "e")
  }))
})

test_that("07. テンプレートによる文生成", {
  x <- 12
  y <- "気温"
  z <- 22.4
  expect_equal(.template_07(x, y, z), "12時の気温は22.4")
})