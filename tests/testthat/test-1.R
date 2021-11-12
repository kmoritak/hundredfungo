test_that("00. 文字列の逆順", {
  expect_equal(f00("stressed"), "desserts")
})

test_that("01. 「パタトクカシーー」", {
  expect_equal(f01("パタトクカシーー"), "パトカー")
})

test_that("02. 「パトカー」＋「タクシー」＝「パタトクカシーー」", {
  expect_equal(f02("パトカー", "タクシー"), "パタトクカシーー")
})

test_that("03. 円周率", {
  expect_equal(
    f03(
      "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
    ),
    "3141592753589710"
  )
})

test_that("04. 元素記号", {
  exp <- list(seq(1:20))
  names <- c()
  for (c in strsplit("H He Li Be B C N O F Ne Na Mi Al Si P S Cl Ar K Ca", " ")) {
    names <- c(names, c)
  }
  names(exp[[1]]) <- names
  expect_equal(
    f04(
      "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
    ),
    exp[[1]]
  )
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

  expect_equal(f05(str, TRUE), exp_word_bigram)
  expect_equal(f05(str, FALSE), exp_letter_bigram)
})