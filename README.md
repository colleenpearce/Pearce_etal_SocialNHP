## Changes in Social Environment Impact Primate Gut Microbiota Composition


In order to run the analysis reported please read this document and follow the environment set-up instructions.

**Installation Requirements:**

* R language, minimum version 4.3.2 (via https://cran.r-project.org/)
     * This version of R is necessary for installing all packages, if there are issues with packages, double check that the R version in use is correct.
* The following R packages via `install.packages()`:
    * languageserver, rmarkdown, httpgd
    * Copy the following line(s) to install the listed packages:
         * All at once:
              * `install.packages(c("languageserver", "rmarkdown", "httpgd"))`
         * Individually: 
              * `install.packages("languageserver")`
              * `install.packages("rmarkdown")`
              * `install.packages("httpgd")`
* The Bioconductor package manager `BiocManager` (via https://cran.r-project.org/web/packages/BiocManager/vignettes/BiocManager.html)
    * Follow the instructions from the above link
    * For more information on Bioconducter visit https://bioconductor.org/packages/3.18/bioc/
* The following Bioconductor packages via `BiocManager::install()`:
    * tidyverse, vegan, magrittr, glue, ggpubr, svglite, RColorBrewer, phyloseq, microbiomeMarker, rstatix
    * Copy the following lines to install the listed packages: 
        * `BiocManager::install(tidyverse)`
        * `BiocManager::install(vegan)`
        * `BiocManager::install(magrittr)`
        * `BiocManager::install(glue)`
        * `BiocManager::install(ggpubr)`
        * `BiocManager::install(svglite)`
        * `BiocManager::install(RColorBrewer)`
        * `BiocManager::install(phyloseq)`
        * `BiocManager::install(microbiomeMarker)`
        * `BiocManager::install(rstatix)`
    * Note that some of these installs may take a couple of minutes
* The `pairwiseAdonis` R package (via GitHub at https://github.com/pmartinezarbizu/pairwiseAdonis)
    * At the above link there will be instructions for Linux and Windows users. For Mac users, the instructions for Windows will work to install `pairwiseAdonis`.

Once all packages are successfully installed, the **SocialNHP.Rmd** script can be knit into an html document.

## Data Availability
The data files required to run this analysis are provided in the folder **MetaDataFiles**. However, all sequence data is also deposited in the NCBI repository and is available using the accession number PRJNA1073535 or at the following URL: https://www.ncbi.nlm.nih.gov/bioproject/1073535
