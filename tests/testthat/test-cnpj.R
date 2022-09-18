test_that("cnpj functions", {
  expect_equal(
    add_digits("22344533"),
    "00000022344533"
  )
  expect_equal(
    fix_cnpj("00022344533"),
    "00.000.022/3445-33"
  )
})
