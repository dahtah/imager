context("ExtractCorrectPatch")

## TODO: Rename context
## TODO: Add more tests

test_that("multiplication works", {
  tmp <- extract_patches(boats,c(10,10),c(10,20),rep(5,2),rep(5,2))
  expect_equal(extract_patches_min(boats,c(10,10),c(10,20),rep(5,2),rep(5,2)), vapply(tmp, min, 0))
})
