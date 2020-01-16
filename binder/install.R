# Installing older version of statnet.common package (this may take a while)
devtools::install_version("statnet.common", version = "4.1.2", repos = "http://cran.us.r-project.org")

# Installing CRAN packages (this may take a while)
packages <- c("devtools", "tm","quanteda", "tuber","qdapRegex","rlang","purrr","ggplot2", "syuzhet", "lexicon") # create list of required packages
for (package in packages) { # check if those packages are already installed
  if (!require(package, character.only=T)) { # if not, install & load them
    install.packages(package)
    library(package, character.only=T)
  }
  else {library(package, character.only=T)} # if they are already installed, only load them
}
rm(packages, package)

# GitHub packages (this may take a while)
install_github("hadley/emo") # install from GitHub
library(emo) # load the emo package

# Installing the emoGG package from github (not on CRAN yet): Displays emojis in ggplot objects
devtools::install_github("dill/emoGG") # install from GitHub
library(emoGG) # load the emo package