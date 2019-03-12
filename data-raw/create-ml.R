# Downloading some common ML datasets
if (!requireNamespace("snedata", quietly = TRUE)) {
  remotes::install_github("jlmelville/snedata")
}

mnist <- snedata::download_mnist()
mnist <- tibble::as_tibble(mnist)
usethis::use_data(mnist, overwrite = TRUE)

fashion <- snedata::download_fashion_mnist()
fashion <- tibble::as_tibble(fashion)
usethis::use_data(fashion, overwrite = TRUE, compress = "bzip2")
