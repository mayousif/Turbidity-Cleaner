## ----eval=FALSE---------------------------------------------------------------
#  install.packages('unitizer')

## ----eval=FALSE---------------------------------------------------------------
#  library(unitizer)
#  demo(unitizer)

## ---- eval=FALSE--------------------------------------------------------------
#  num.var <- 14523.2342520  # assignments are not considered tests
#  test_me(num.var)          # safe

## ---- eval=FALSE--------------------------------------------------------------
#  test_me(14523.2342520)    # could be deparsed differently

## ----eval=FALSE---------------------------------------------------------------
#  chr <- "hello\u044F"
#  fun_to_test(chr)

## ----eval=FALSE---------------------------------------------------------------
#  fun_to_test("hello\u044F") # could be deparsed differently

