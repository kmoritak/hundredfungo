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

test_that("04. ", {
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