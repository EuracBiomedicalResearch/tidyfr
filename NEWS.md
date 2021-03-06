# `tidyfr` 0.99

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
