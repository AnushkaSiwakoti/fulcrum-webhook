# init.R
#

my_packages = c( 'plumber', 'dplyr', 'jsonlite', 'httr', 'glue', 'rlang', 'data.table', 'rlist', 'stringr')

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible( sapply( my_packages, install_if_missing))