# The R interface to CHRIS data

`chrisr` provides an interface to retrieve CHRIS data from its new data storage
format. The main properties of this interface are:
- Retrieve data for a particular data release.
- Extract data for specific data domains.
- Return the data as `data.frame` with all variables in their correct data type.

# CHRIS data format

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
be available in a folder `/CHRIS/Interview-data/version_1.0/Interview-data`.
