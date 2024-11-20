
# Schizomida Trait Data Base (STDB)

<!-- badges: start -->
[![](https://img.shields.io/badge/Shinylive-Run%20this%20app%20in%20your%20browser-%233077b4?logo=rstudioide&logoColor=%233077b4&labelColor=%23f7f7f7)](https://williamgearty.com/Schizomida/)
<!-- put a badge for the manuscript eventually -->
<!-- badges: end -->

This repository holds the data and source code for the Schizomida Trait Data Base, which is hosted [here](https://williamgearty.com/Schizomida/). We used the shiny R package (Chang et al., 2023) to build the basic Shiny App framework. This converts normal R code into a server-based webapp. We extended this with many other shiny-adjacent R packages. We used the htmltools (Cheng et al., 2023) and bslib (Sievert et al., 2023) packages to customize the appearance of the user interface (UI). We used the shinyjs package (Attali, 2021) to perform JavaScript functions via the user’s browser. We used the fontawesome package (Iannone, 2023) to add icons to various UI elements. We used the reactable package (Lin, 2023) to render and update data tables in the UI. We used the dplyr (Wickham et al., 2023), openxlsx (Schauberger & Walker, 2023), janitor (Firke, 2023), and Hmisc (Harrell, 2023) packages for data cleaning and preparation. Finally, we used the shinylive package (Schloerke & Chang, 2023) to compile our data and code to the webR language for use as a serverless Github-hosted website (stored within the `docs/` folder). This entire repo can also be downloaded and run as a normal shiny app with the `app.R` file.

## Editing and automation
The repository has automation set up (via GitHub Actions, see `.github/workflows/update_webapp.yaml`) to reduce the amount of changes that are required when wanting to update the webapp (see `convert_data.R` and `update_webapp.R`):

- All **R source code** should be edited in the `app.R` file; any changes will be automatically copied to the shinylive webapp. The `myapp/app.R` file should never be manually edited.
- The raw **database data** should be updated in the `data/STDB_data.xlsx` spreadsheet. The "STDB" sheet contains the data for the database tab, and the "Species description history" sheet contains the data for the description history tab. These two sheets will be converted to csv files, so keep in mind that cell styles will not transfer to the webapp.
- The source for the **"About and Contact"** tab is stored in the `data/STDB_about.htm` file, which was originally converted from the `data/STDB_about.docx` file. Future changes should be made directly in the `data/STDB_about.htm` file.
- The source for the **"References"** tab is stored in the `data/STDB_references.htm` file, which was originally converted from the `data/STDB_references.docx` file. Future changes should be made directly in the data/STDB_references.htm file.
- **Figures** should be updated in the `data/drawings_database/` folder. All figures should be tiff files, which will be converted to png files for the webapp. Figure file names should match headers of the database in the `data/STDB_data.xlsx` file. Any character that is not alphanumeric or an underscore should be replaced with an underscore. See existing file names for examples.
- **Figure captions** should be updated in the "Sheet1" sheet of the `data/figure_captions.xlsx` spreadsheet. As above, this data will be converted to a csv file, so cell styles will not transfer. File names should match exactly to the file names of the figures in the `data/drawings_database/` folder.
- All source and data **conversions** are performed by `convert_data.R`. This file can be modified to tweak this process.
- The **shinylive webapp** is generated by `update_webapp.R`. This file can be modified to update the shinylive assets, favicons, and other webapp details.
- Both of these files are automatically run by the `.github/workflows/update_webapp.yaml` Github Action. This file can be modified to tweak this process.
 
## R packages used:
Attali, D. (2021). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. https://CRAN.R-project.org/package=shinyjs

Chang, W., Cheng, J., Allaire, J. J., Sievert, C., Schloerke, B., Xie, Y., Allen, J., McPherson, J., Dipert, A., & Borges, B. (2023). shiny: Web Application Framework for R. https://CRAN.R-project.org/package=shiny

Cheng, J., Sievert, C., Schloerke, B., Chang, W., Xie, Y., & Allen, J. (2023). htmltools: Tools for HTML. https://CRAN.R-project.org/package=htmltools

Firke, S. (2023). janitor: Simple Tools for Examining and Cleaning Dirty Data. https://CRAN.R-project.org/package=janitor

Harrell, F. E. (2023). Hmisc: Harrell Miscellaneous. https://CRAN.R-project.org/package=Hmisc

Iannone, R. (2023). fontawesome: Easily Work with “Font Awesome” Icons. https://CRAN.R-project.org/package=fontawesome

Lin, G. (2023). reactable: Interactive Data Tables for R. https://CRAN.R-project.org/package=reactable

Schauberger, P. & Walker, A. (2023). openxlsx: Read, Write and Edit xlsx Files. https://CRAN.R-project.org/package=openxlsx

Schloerke, B. & Chang, W. (2023). shinylive: Run Shiny applications in the browser. https://posit-dev.github.io/r-shinylive/

Sievert, C., Cheng, J., & Aden-Buie, G. (2023). bslib: Custom 'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'. https://CRAN.R-project.org/package=bslib

Wickham, H., François, R., Henry, L., Müller, K., & Vaughan, D. (2023). dplyr: A Grammar of Data Manipulation. https://CRAN.R-project.org/package=dplyr

## Contact

### Schizomida Trait Data Base (STDB)
[Sandro Pascal Müller](https://www.researchgate.net/profile/Sandro-Mueller-3)\
[Ilian De Francesco Magnussen](https://www.researchgate.net/profile/Ilian-De-Francesco-Magnussen)

### Shiny App Development
[William Gearty](https://github.com/willgearty)

### Web Image Credit
[melvynyeo (iNaturalist)](https://www.inaturalist.org/people/melvynyeo)

