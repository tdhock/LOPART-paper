### Write down what package versions work with your R code, and
### attempt to download and load those packages. The first argument is
### the version of R that you used, e.g. "3.0.2" and then the rest of
### the arguments are package versions. For
### CRAN/Bioconductor/R-Forge/etc packages, write
### e.g. RColorBrewer="1.0.5" and if RColorBrewer is not installed
### then we use install.packages to get the most recent version, and
### warn if the installed version is not the indicated version. For
### GitHub packages, write "user/repo@commit"
### e.g. "tdhock/animint@f877163cd181f390de3ef9a38bb8bdd0396d08a4" and
### we use install_github to get it, if necessary.
works_with_R <- function(Rvers,...){
  local.lib <- file.path(getwd(), "library")
  dir.create(local.lib, showWarnings=FALSE, recursive=TRUE)
  .libPaths(local.lib)
  pkg_ok_have <- function(pkg,ok,have){
    stopifnot(is.character(ok))
    if(!as.character(have) %in% ok){
      warning("works with ",pkg," version ",
              paste(ok,collapse=" or "),
              ", have ",have)
    }
  }
  pkg_ok_have("R",Rvers,getRversion())
  pkg.vers <- list(...)
  for(pkg.i in seq_along(pkg.vers)){
    vers <- pkg.vers[[pkg.i]]
    pkg <- if(is.null(names(pkg.vers))){
      ""
    }else{
      names(pkg.vers)[[pkg.i]]
    }
    if(pkg == ""){# Then it is from GitHub.
      ## suppressWarnings is quieter than quiet.
      if(!suppressWarnings(require(requireGitHub))){
        ## If requireGitHub is not available, then install it using
        ## devtools.
        if(!suppressWarnings(require(devtools))){
          install.packages("devtools")
          require(devtools)
        }
        install_github("tdhock/requireGitHub")
        require(requireGitHub)
      }
      requireGitHub(vers)
    }else{# it is from a CRAN-like repos.
      if(!suppressWarnings(require(pkg, character.only=TRUE))){
        install.packages(pkg)
      }
      pkg_ok_have(pkg, vers, packageVersion(pkg))
      library(pkg, character.only=TRUE)
    }
  }
}

if(!requireNamespace("SegAnnot")){
  install.packages("SegAnnot", repo="https://R-Forge.R-project.org")
}

options(repos=c(
  "http://cloud.r-project.org"))
works_with_R(
  "4.3.2",
  neuroblastoma="2023.9.3",
  data.table="1.14.10",
  "tdhock/penaltyLearning@52e62d4e209f90b29066db56b1b03da039f9e92e",
  tikzDevice="0.12.6",
  ggplot2="3.4.4",
  ##ggrepel="0.8.2",
  microbenchmark="1.4.10",
  fpop="2019.8.26",
  bit64="4.0.5",
  R.utils="2.12.3",
  "tdhock/LOPART@0fa34bd9bef61303c997873265337c067b27f2c9",
  ##directlabels="2020.1.31"
  "tdhock/binsegRcpp@ffa8a487a6a543cae8dc4fc6b263d72a1b8ed3d0",
  "tdhock/directlabels@f690edf6db2790960aa00ca388b7e11da74bf783")
options(
  tikzDocumentDeclaration="\\documentclass[12pt]{article}",
  tikzMetricsDictionary="tikzMetricsArxiv")
