
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datamodules

<!-- badges: start -->

<!-- badges: end -->

The goal of datamodules is to facilitate working with a central data
store that is structured so that data-related scripts for downloading,
updating, cleaning, etc. are organized in source-specific subfolders.

This all has to do with my continual pains in working with diverse data
sources. A couple of specific problems that I persistently struggle with
are:

  - For any given data source, there is a mix of tasks that can be
    automated, like data downloading and updating, tasks that likely
    will have to remain interactive and monitored, like data cleaning
    and imputation, and project-specific needs like particular data
    transformations. Where do you put the bits for all of this?
  - I often have multiple copies of essentially the same data in
    different locations, and similarly multiple copies and slight
    variations of similar data cleaning code.
  - It’s always a pain to figure out how to get a particular dataset
    into R for exploration. E.g. sometimes I’d like to look up how many
    ACLED events there have been in Estonia last year without having to
    think about downloading ACLED, where that file is located on my
    computer. etc.

A structure I have evolved towards is to have a central data store in
which raw data are saved, and which contains source-specific cleaning
code, e.g. to get a dataset complian with G\&W country-years. For a
specific project, copy the minimally cleaned data from that store and
further process it as
    needed.

    data/  # the base path for storing artifacts and source-specific cleaning code, notes, etc.
    ├── acled/
    ├── archigos/
    │   ├── input-data/
    │   ├── output-data/
    │   ├── coding-notes.Rmd  # clean, transform
    │   └── README.Rmd        # summary of current data, notes, etc.
    ├── epr
    ...

**datamodules** is meant to fill some gaps in this strategy:

  - `dm_path()` provides the location to the central data store to make
    it easier to read data from it for use in other projects.
  - it includes code to scrape Wikipedia terrorism events, and the idea
    is in the future to add more miscellaneous small code sets like this
    that don’t warrant a fullblown R package themselves
  - it includes some imputation helpers (well one right now)

## Installation

<!--
You can install the released version of datamodules from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("datamodules")
```
-->

    remotes::install_github("andybega/datamodules")

## Example
