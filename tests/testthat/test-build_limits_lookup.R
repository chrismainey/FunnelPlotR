test_that("`build limits funcion` works with input and returns expected data.frame", {
  z<-build_limits_lookup(10, 250, 0.65, 1.51, draw_adjusted=TRUE, tau2=0.02,
                         data_type="SR", sr_method="SHMI", multiplier=1, target=1)
 
  
  expect_s3_class(z, "data.frame")
  expect_length(z, 10)
  expect_length(z$number.seq, 1000)
  #expect_equal(round(b$rr,6), c(1.069652, 1.071795, 1.022727))
  
})
