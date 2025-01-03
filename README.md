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

## Running

### Interactively

All files can be run interactively. To run the files 01 and 02 interactively, be sure to change the settings using the global variables between the library calls and the first function in each file.

### Command-line

The `src` files from 01 up to 03b can be run on the command line using the Rscript command. (If you do so, make sure that your directory containing Rscript.exe is in PATH.) The 03c file is meant to be run interactively only.

The first two scripts have options, as follows:

```         
Usage: src/01_prep_wo_predictors.R [options]

Options:
    -d CHARACTER, --disco=CHARACTER
            document name (no file extension)

    -p LOGICAL, --debug=LOGICAL
            run in debug mode?

    -h, --help
            Show this help message and exit
```

```         
Usage: src/02_integrate_changes.R [options]


Options:
        -d CHARACTER, --disco=CHARACTER
                document name (no file extension)

        -p LOGICAL, --debug=LOGICAL
                run in debug mode?

        -h, --help
                Show this help message and exit
```

### Full preprocessing pipeline

To update everything that needs to be updated at once up to the final data frame used for analysis (i.e. `output/03a_coded_data/wodata.rds`), run `make`. Note that all the texts that are to be used for final analysis should be listed in `curr_texts.txt` before running `make` or else new files will not be added.

The analysis code (file starting with 03b in `src`) will not be run with `make`; run `make output/best_model.rds` in that case.