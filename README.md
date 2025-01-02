# tibetan-convo-wordorder

This is a repository for my Tibetan word order in conversation project.

## Content

The repository contains the following directories:

-   `data` contains the input data as well as intermediate data representations within the repo, including manual corrections of automatically generated annotations

-   `doc` contains the annotation scheme

-   `output` contains processed data input into the models (in RDS and CSV formats) as well as information from the selected model (including raw information in RDS and CSV format, and visualiations)

-   `renv` contains reproducible environment information for the `renv` package

-   `src` contains all the source code:

    -   The `dev` subfolder contains code for writing code in this repo;

    -   The `utils` subfolder contains code used in all the scripts;

    -   The scripts starting with 01 and 02 are for initial preprocessing (including automatic annotations) and manual correction of automatic annotations;

    -   The scripts starting with 03 include preprocessing code for ROLLing models, as well as code for fitting the models themselves and a Quarto file for visualisation and exploration