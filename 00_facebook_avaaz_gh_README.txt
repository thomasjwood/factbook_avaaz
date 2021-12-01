ReadMe for replication data for paper, "Political Misinformation and Factual Corrections on the Facebook News Feed: Experimental Evidence", forthcoming the Journal of Politics, by Ethan Porter (George Washington University) and Thomas J Wood (Ohio State University).

These files replicate the figures and table in the order they appeared in the submitted pdf. 

Before you replicate these estimates, please ensure you have a current version of R installed (this replication file was prepared for R 4.1.2 ("Bird Hippie").

Please also ensure you have the following R packages installed:

plyr_1.8.6
showtext_0.9-4
showtextdb_3.0
sysfonts_0.8.5
ggstance_0.3.5
estimatr_0.30.4
broom_0.7.10
emmeans_1.7.1-1
magrittr_2.0.1
forcats_0.5.1
stringr_1.4.0       
dplyr_1.0.7
purrr_0.3.4.9000
readr_2.1.0
tidyr_1.1.4
tibble_3.1.6
ggplot2_3.3.5.9000
tidyverse_1.3.1.9000

These packages entail a few argument conflicts. Please ensure your masking conflicts are resolved as follows (you may confirm the conflict pattern in your instance of R with tidyverse::tidyverse_conflicts()):

x dplyr::arrange()           masks plyr::arrange()
x purrr::compact()           masks plyr::compact()
x dplyr::count()             masks plyr::count()
x magrittr::extract()        masks tidyr::extract()
x dplyr::failwith()          masks plyr::failwith()
x dplyr::filter()            masks stats::filter()
x ggstance::geom_errorbarh() masks ggplot2::geom_errorbarh()
x dplyr::id()                masks plyr::id()
x dplyr::lag()               masks stats::lag()
x dplyr::mutate()            masks plyr::mutate()
x dplyr::rename()            masks plyr::rename()
x magrittr::set_names()      masks purrr::set_names()
x dplyr::summarise()         masks plyr::summarise()
x dplyr::summarize()         masks plyr::summarize()

The microdata necessary to replicate these estimates are hosted as .rds files on a public GitHub repository. Please ensure you have an active internet connection and that GitHub is functional.