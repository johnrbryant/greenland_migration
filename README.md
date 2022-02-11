
<!-- README.md is generated from README.Rmd. Please edit that file -->

# greenland\_account

Code to make demographic accounts for Greenland.

The data come from the Stats Bank online database on the Statistics
Greenland website (via the R package **pxweb**).

| Folder           | Description                                                                                                                                                                                                                           |
|:-----------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `rawdata`        | Code to obtain data from the Statistics Greenland website, reformat the data, and create the csv files in `data`. If your main interest is how to construct a demographic account, you can safely ignore this folder.                 |
| `rawdata_region` | As for `rawdata`, but with a region variable.                                                                                                                                                                                         |
| `data`           | CSV files with data on population, births, deaths, immigration, emigration, and internal migration.                                                                                                                                   |
| `acctotal`       | Account with no age-sex detail. Calculated using existing methods (via package **demest**).                                                                                                                                           |
| `acctotal2`      | Account with no age-sex detail. Calculated using experimental methods (via package **pomp**).                                                                                                                                         |
| `accagesex`      | Account with age-sex detail. Calculated using existing methods (via package **demest**).                                                                                                                                              |
| `accagesex2`     | Account with age-sex detail. Calculated using experimental methods (via package **pomp**).                                                                                                                                            |
| `accreg`         | Account with age-sex and region detail. Calculated using existing methods (via package **demest**). *Note: To avoid a problem with exposure for small cells, `accreg` needs to be run with* **demest** *version 0.0.0.5.1 or higher.* |
| `accreg2`        | Account with age-sex and region detail. Calculated using existing methods (via package **pomp**).                                                                                                                                     |
