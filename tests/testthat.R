library('testthat')
library('Gmisc')

test_check('Gmisc')
## To run the tests manually
if (FALSE){
  my_test_dir <- gsub("Gmisc.*", "Gmisc/tests/testthat", getwd())
  test_dir(my_test_dir)
}