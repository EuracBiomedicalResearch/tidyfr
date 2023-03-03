# `tidyfr` 0.99

## Changes in 0.99.11

- Use AIDs as `rownames` for the `data.frame` returned by `data` and drop
  the additional column `"aid"`.

## Changes in 0.99.10

- Add Rmd for TDFF usage of the metabolomics_p180 data set.

## Changes in 0.99.9

- Skip (re)formatting of AIDs if AIDs are provided as `character` (assuming that
  they are already in the correct format).

## Changes in 0.99.8

- Add function `remove_participants` function.
- Add several utility functions.

## Changes in 0.99.7

- Data export writes also a *NEWS.md* file.
- Importer for categorical values drops category representing `NA` encoding.

## Changes in 0.99.6

- Add vignette.
- Fix import of data files: support `'` and `"`.
- Fix validator for categorical variables: check only variables declared to be
  categorical.

## Changes in 0.99.5

- Store additional label information in a file labels_additional_information.txt
  (was labels_add.txt).
- .labels imports also labels_additional_information.txt file.

## Changes in 0.99.4

- Add `DataModule` and code to represent CHRIS Data modules.
- Add methods to import data from data modules.

## Changes in 0.99.3

- Add functionality to extract CTFF-compliant data from a
  `SummarizedExperiment`: methods `chris_data`, `chris_labels` and
  `chris_groups`.

## Changes in 0.99.2

- Add documentation and fix small issues.

## Changes in 0.99.1

- Add functionality to export data in the CTF format.
