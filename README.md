# NC Triangle Walkability Analysis

This project uses spatial stats to explore walkability and income within the NC Triangle.
Code written in R.

## Description

The NC Triangle region is often credited for its walkability, but what factors influence
that walkability, and is there a pattern to its distribution? This project aims to understand the
relationship between a community’s level of income and their access to walkable areas. The
project asks: “How is the percentage of low-to-moderate income households related to
walkability within the NC Triangle, after adjusting for population density and spatial
autocorrelation?”

For more information on the project, including data, methods, results, and conclusion,
navigate to the final write up, located in the documents folder.

## Installation

This project is self-contained. Following a full download of the folder structure, simply 
run each R file in this order:
1. download_data.R: will pull files from relevant APIs and write them to the Data/Raw folder.
2. wrangle.R: will clean raw files and output them to Data/Cleaned and Data/Outputs
3. analysis.R (interchangeable with 4): will run the analysis from cleaned data.
4. create_map.R (interchangeable with 3): will create three maps and store them under Graphics.

## License

MIT License.
Copyright (c) 2026 Joshua Mark.

## Acknowledgements

Feedback and guidance from my profesor, Dr. Paul Delamater.

Created in R Studio.

Libraries used: tidyverse, magrittr, sf, tigris, tmap, spdep, spatialreg, classInt.
