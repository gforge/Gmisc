fn <- system.file("extdata", "Full_test_suite.Rmd", package = "Gmisc")
if (!file.exists(fn))
  stop("Can't find the test file")

if (FALSE) {
  # This isn't allowed in read-only file-systems - see https://github.com/gforge/Gmisc/issues/36
  td <- tempdir()
  rmarkdown::render(fn, output_file = file.path(td, "tmp.html"))
  
  unlink(c(file.path(td, "docx.css"),
           file.path(td, "tmp.html"),
           file.path(td, "tmp_files")), 
         recursive = TRUE)
  
  additional_deletes <- c()
  fn <- system.file("extdata", "Full_test_suite_files", package = "Gmisc")
  if (dir.exists(fn))
    additional_deletes <- c(additional_deletes, fn)
  
  fn <- system.file("extdata", "Full_test_suite.html", package = "Gmisc")
  if (file.exists(fn))
    additional_deletes <- c(additional_deletes, fn)
  
  fn <- system.file("extdata", "docx.css", package = "Gmisc")
  if (file.exists(fn))
    additional_deletes <- c(additional_deletes, fn)
  
  if (length(additional_deletes) > 0)
    unlink(additional_deletes, recursive = TRUE)
}
