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
