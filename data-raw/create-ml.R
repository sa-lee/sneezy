# TODO: convert to BiocFileCache

# Downloading some common ML datasets
if (!requireNamespace("snedata", quietly = TRUE)) {
  remotes::install_github("jlmelville/snedata")
}

library(sneezy)

mnist <- snedata::download_mnist()
se_mnist <- as_sightse(mnist, 
                    traveller = tourr::grand_tour(),
                    1:784, 
                    view_as = "RleMatrix")
usethis::use_data(se_mnist, overwrite = TRUE, compress = "bzip2")

fashion <- snedata::download_fashion_mnist()
fashion <- tibble::as_tibble(fashion)
usethis::use_data(fashion, overwrite = TRUE, compress = "bzip2")
