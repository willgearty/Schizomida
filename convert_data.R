# use this file to convert the excel file to csv files
library(openxlsx)

# load data ----
shiny_schiz_orig <- read.xlsx('data/STDB_data.xlsx',
                              sheet = 'STDB',
                              fillMergedCells = TRUE)

# edit references subheading
shiny_schiz_orig[1, ncol(shiny_schiz_orig)] <- "References"

taxonomy_syn <- read.xlsx('data/STDB_data.xlsx',
                          sheet = 'Species description history')
colnames(taxonomy_syn) <- gsub(".", " ", colnames(taxonomy_syn), fixed = TRUE)

write.csv(shiny_schiz_orig, "myapp/data/STDB.csv", row.names = FALSE)
write.csv(taxonomy_syn, "myapp/data/description_history.csv",
          row.names = FALSE)

# copy about and reference HTML ----
file.copy("data/STDB_about.htm", "myapp/data/STDB_about.htm", overwrite = TRUE)
file.copy("data/STDB_references.htm", "myapp/data/STDB_references.htm", overwrite = TRUE)

# copy figure captions ----
tmp <- read.xlsx("data/drawings_database/figure_captions.xlsx", sheet = "Sheet1")
write.csv(tmp, "myapp/data/figure_captions.csv", row.names = FALSE)

# convert figure tiffs to pngs ----
library(tiff)
library(png)

# remove old files first
file.remove(list.files("docs/drawings_database/", full.names = TRUE))

files <- list.files("data/drawings_database/", "*.tif")
for (file in files) {
  img <- readTIFF(paste0("data/drawings_database/", file), native = FALSE)
  writePNG(img, target = paste0("docs/drawings_database/", gsub(".tif", ".png", file)))
}

