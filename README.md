# Japanese Beetle Tracker (R)

An interactive Shiny for R dashboard tracking Japanese Beetle (*Popillia japonica*) sightings across North America, based on GBIF occurrence data.

🔗 [Live App](https://your-posit-cloud-link-here)

## Features

- Filter observations by year range, region, and basis of record
- Interactive leaflet map showing geographic distribution
- Time series, pie chart, bar charts for data exploration

## Installation
```r
install.packages("renv")
renv::restore()
```

## Run Locally
```r
shiny::runApp()
```

## Data

Data sourced from [GBIF](https://www.gbif.org/) — Global Biodiversity Information Facility.