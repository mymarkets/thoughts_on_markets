library(usethis )
proj_set()
usethis::use_rstudio()
usethis::use_git()
usethis::use_github()

usethis::edit_r_environ()

Sys.getenv("GITHUB_PAT")

devtools::install_github('mymarkets/mymarkets.github.io')

git remote add origin https://github.com/mymarkets/mymarkets.github.io.git
usethis::edit_r_environ()

library(usethis)
use_git_config(user.name = "mymarkets", user.email = "mymarkets@tutanota.com")


git config --global user.name 'mymarkets'
git config --global user.email 'mymarkets@tutanota.com'
git config --global --list
https://github.com/mymarkets/mymarkets.github.io.git