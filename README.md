# Health Data Science Project

## Introduction

An R package for getting and plotting condition occurrences from a an [OMOP CDM](https://ohdsi.github.io/CommonDataModel/cdm54.html) database.

## Features

-   Takes a `DatabaseConnector::connection` as input
-   Extracts counts of all conditions by year and month using `extractPatients`
-   Is able to do this for different SQL dialects using `SqlRender`
-   Can plot condition occurrences by year or by month using `plotTrend(data, byMonth = FALSE)`
-   Launches a shiny app to select conditions from dropdown and filter by year range using `launchShinyApp()`
-   Example of how to run in `codeToRun.R`
