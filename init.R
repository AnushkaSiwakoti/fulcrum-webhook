<<<<<<< HEAD
# init.R

# installs libraries onto the Heroku server

my_packages = c( 'plumber', "tidyverse", "lme4", 'jsonlite', 'httr')

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

=======
# init.R

# installs libraries onto the Heroku server

my_packages = c( 'plumber', "tidyverse", "lme4", 'jsonlite', 'httr')

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

>>>>>>> e2a294c15498949c17eb4f5fc111e4d4887702ee
invisible( sapply( my_packages, install_if_missing))