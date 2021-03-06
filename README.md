# The R interface to the Textual Dataset File Format

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check-bioc](https://github.com/EuracBiomedicalResearch/tidyfr/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/EuracBiomedicalResearch/tidyfr/actions?query=workflow%3AR-CMD-check-bioc)
[![codecov.io](http://codecov.io/github/EuracBiomedicalResearch/tidyfr/coverage.svg?branch=main)](http://codecov.io/github/EuracBiomedicalResearch/tidyfr?branch=main)
[![license](https://img.shields.io/badge/license-Artistic--2.0-brightgreen.svg)](https://opensource.org/licenses/Artistic-2.0)

`tidyfr` provides an interface to retrieve data stored in the Textual Dataset
File Format (TDFF) from the Eurac Research Institute for Biomedicine. All data
files from the CHRIS study are stored in this format, thus, `tidyfr` allows to
import CHRIS data. The main properties of this interface/import functionality
are:

- Retrieve data for a particular version of a data module.
- Return the data as `data.frame` with all variables in their correct data type.

# Textual Dataset File Format

A documentation for the new data format can be found
[here](https://wiki.gm.eurac.edu/index.php?title=Textual_Dataset_Format). In
brief, a data set is defined as a directory (or archive) containing 6 text
files:

- info.txt: general data information.
- data.txt: the data.
- labels.txt: column definitions with labels.
- groups.txt: column groups, i.e. columns being related to each other.
- grp_labels.txt: group definition and labels.
- mappings.txt: category definitions and special values.

# Definitions and how data is organized

CHRIS data is structured in the following hierarchical folder structure:

- CHRIS data path: base folder containing one or more of
  - CHRIS modules: a folder bundling all data for a certain module containing
    one or more:
    - Module version: a folder defining the version containing one:
	  - CHRIS data: a folder containing data in the new CHRIS textual dataset
        format.

Data from the CHRIS questionnaire data for the CHRIS baseline release would thus
be available in a folder `/CHRIS/Interview-data/version_1.0/data`.
