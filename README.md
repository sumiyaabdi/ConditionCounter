# Health Data Science Project

## Introduction

An R package for getting and plotting condition occurrences from a an [OMOP CDM](https://ohdsi.github.io/CommonDataModel/cdm54.html) database.

## To install

Make sure you install the package. This will ensure all the dependencies are present.

``` r
install.packages("remotes")
remotes::install_github("sumiyaabdi/ConditionCounter")
```

Or if installing from within the folder:

```r
devtools::install()
```

## To run

You can source `codeToRun.R`, or run the functions one by one.

## Features

-   Takes a `DatabaseConnector::connection` as input
-   Extracts counts of all conditions by year and month using `extractPatients`
-   Is able to do this for different SQL dialects using `SqlRender`
-   Can plot condition occurrences by year or by month using `plotTrend(data, byMonth = FALSE)`
-   Launches a shiny app to select conditions from dropdown and filter by year range using `launchShinyApp()`
-   Example of how to run in `codeToRun.R`
