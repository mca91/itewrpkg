# 📦itewrpkg 

[![Build Status](https://travis-ci.org/mca91/itewrpkg.svg?branch=master)](https://travis-ci.org/mca91/itewrpkg)

A metapackage for the bookdown project <i>Introduction to Econometrics with R</i>, an interactive R companion to the textbook <i>Introduction to Econometrics</i> by Stock &amp; Watson (2015)

<p align="center"><img align="center" src="https://github.com/mca91/EconometricsWithR/blob/master/docs/images/cover.png" width="28%" height="28%"></p>

The book can be found here: [Introduction to Econometrics with R](https://www.econometrics-with-r.org/)

The bookdown project is available in [this](https://github.com/mca91/EconometricsWithR) GitHub repository.

#### Installation

It is straightforward to download and install the `itewrpkg` package from GitHub using `install_github()` from the `devtools` package. Run `install.packages("devtools")` if you are not sure whether `devtools` is installed. The following one-liner installs `itewrpkg`:

```
devtools::install_github("mca91/itewrpkg")
```

Running the above command will also install all R packages which are required for reproducing the code examples presented throughout the book. Running `library(itewrpkg)` will load the package and all dependencies which makes it unnecessary to individually attach the packages introduced at the beginning of chapter. This may take a few seconds but may be convenient if you are playing around with code chunks from various chapters. 

#### Usage

The function `get_materials_itewr()` is intented as a convenience function for students working with the companion. It downloads up-to-date versions of all supplements to the book such as datasets and R codes from the [GitHub repository](https://github.com/mca91/EconometricsWithR) of the book and saves them to the current working directory (or a location of choice provided to the argument `dir`) according to the following structure:

- `<your_working_directory>/ITEWR/Rmds/` (.Rmd files)

- `<your_working_directory>/ITEWR/Data/` (Datasets)

- `<your_working_directory>/ITEWR/Rcodes/` (R Scripts, numbered by chapter)

You may check your working directory using `getwd()`.

#### Functions

Upon attaching the package using `library(itewrpkg)` the following functions are available:

- `get_materials_itewr()` downloads all the materials neccessary to reproduce R examples from the book

- `get_repo()` can be used to download an entire GitHub repository

- `get_repo_filenames()` scrapes file names from a GitHub repository

- `get_repo_files()` get all files from a specific folder in a GitHub repository

- `squeeze_rmds` a simple wrapper for `knitr::purl()` which tangles R code from chapters in a bookdown projekt to `.R` files

#### References
Stock, J., & Watson, M. (2015). *Introduction to Econometrics, Third Update, Global Edition*. Pearson Education Limited.
