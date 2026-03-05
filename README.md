
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AAGI shared code database

## Motivation

This project was motivated by our team’s desire to:

- Reduce duplication in our coding practices
- Offer shared resources to support growing AAGI community, especially
  new comers
- Improve statistical rigour and coding practices
- Create opportunites to connect and collaborate
- Identify areas of greatest code duplication/overlap for creating
  software

## Contents

In this repository, you will find the code that:

- Compiles the metadata of shared [desk-drawer
  code](https://aagi-aus.github.io/AAGIverse-shared-code/database/#what-is-desk-drawer-code)
  into a [searchable
  database](https://aagi-aus.github.io/AAGIverse-shared-code/database/dtable.html)
- Provides
  [guidance](https://aagi-aus.github.io/AAGIverse-shared-code/database/usage.html)
  on how to interact and contribute to the database

## Dependencies

This project is built using Quarto and dependencies are managed using
[`renv`](https://rstudio.github.io/renv/)

To get started:

1.  Clone this repository locally to your computer
2.  Open it in RStudio of your choosen IDE
3.  Install `renv`

``` r
install.packages("renv")
```

4.  Restore the R environment. This will take a while and install all
    the required dependencies needed to render the entire website.

``` r
renv::restore()
```

5.  Click on any .qmd and hit ‘Render’ or use the `quarto render` and
    `quarto preview` command on the command line

## Project Team

This project would not be possible without the support and efforts of
[our
team](https://aagi-aus.github.io/AAGIverse-shared-code/database/#our-team).

## Reach out

We are always open to feedback and welcome new ideas. Please submit a
[GitHub Issue](https://github.com/AAGI-AUS/AAGIverse-shared-code/issues)
or reach out the team on our [Contact Us page]()
