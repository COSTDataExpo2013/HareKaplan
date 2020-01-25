HareKaplan
==========

This repository contains supplementary material for the paper "Putting Down Roots: A Graphical Exploration of Community Attachment" by Kaplan and Hare (2019) so that the reader can reproduce the analysis from the paper.

* The **code** folder contains all the `R` scripts used to format the data as well as the analysis in the paper.
    * The **data_munging.R** script takes the Knight Foundation data and creates a clean RData file for use in the paper and the `shiny` application. This script contains all the commands used to clean and save the file in the **data** folder.
    * The **paper.R** script contains all the analysis in the paper. This script depends on data created (and saved) by the **data_munging.R** script that is stored in the **data** folder.
* The **css** folder contains the files for styling the web application.
* The **data** folder contains the Knight Foundation data as well as the cleaned datasets used in the `shiny` application and the paper, **clean_sotc.RData**
* The **paper** folder contains the .Rnw and all supporting files to build the paper.
* The **scripts** folder contains the JavaScript file (**graph.js**) used to create the interactive graphs in the `shiny` application
* The **server.R** and **ui.R** files build the `shiny` application and use the file structure in this repo. To run the application locally, use the `shiny::runApp()` command within this directory.


The full reference is: Kaplan, A.J. & Hare, E.R. (2019) "Putting Down Roots: A Graphical Exploration of Community Attachment", Computational Statistics 34(4):1449–1464.

The final publication is available at the Springer web site via https://doi.org/10.1007/s00180-018-0850-7. © Springer-Verlag GmbH Germany, part of Springer Nature, 2019.
