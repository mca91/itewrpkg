# 📦itewrpkg 

[![Build Status](https://travis-ci.org/mca91/itewrpkg.svg?branch=master)](https://travis-ci.org/mca91/itewrpkg)

A metapackage for the bookdown project <i>Introduction to Econometrics with R</i>, an interactive companion to the textbook <i>Introduction to Econometrics</i> by Stock &amp; Watson (2015)

<p align="center"><img align="center" src="https://github.com/mca91/EconometricsWithR/blob/master/docs/images/cover.png" width="25%" height="25%"></p>

#### Installation

It is straightforward to download and install the `itewrpkg` package from GitHub using `install_github()` from the `devtools` package. Run `install.packages("devtools")` if you are not sure whether `devtools` is installed. The following one-liner installs `itewrpkg`:

```
devtools::install_github("mca91/itewrpkg")
```

Running the above command will also install all R packages which are required for reproducing the code examples presented throughout the book. Running `library(itewrpkg)` will load the package and all dependencies which makes it unnecessary to individually attach the packages introduced at the beginning of chapter. This may take a few seconds but may be convenient if you are playing around with code chunks from various chapters 

#### Functions

Upon attaching the package using `library(itewrpkg)` the following functions are available:

- `get_materials_itewr()`
_ `get_repo()`
- `get_repo_filenames()`
- `get_repo_files()`
- `squeeze_rmds`

#### References
Stock, J., & Watson, M. (2015). *Introduction to Econometrics, Third Update, Global Edition*. Pearson Education Limited.
