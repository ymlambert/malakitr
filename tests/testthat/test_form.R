forms <- getForms(token = "42acc1bfe58f9c924bdedc680ab2011622f2bcb5")

test_that("getForms returns a dataframe",{
  expect_s3_class(forms, "data.frame")
})
test_that("Test form's xFormId and idString are found",{
  expect_gt(which(forms$xform_id == 588520),0)
  expect_gt(which(forms$id_string == "test_form"),0)
})

