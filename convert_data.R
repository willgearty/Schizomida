# use this file to convert the excel file to csv files
library(openxlsx)

# load data ----
shiny_schiz_orig <- read.xlsx('data/STDB.xlsx',
                              sheet = 'STDB',
                              fillMergedCells = TRUE)

# edit references subheading
shiny_schiz_orig[1, ncol(shiny_schiz_orig)] <- "References"

taxonomy_syn <- read.xlsx('data/STDB.xlsx',
                          sheet = 'Species description history')
colnames(taxonomy_syn) <- gsub(".", " ", colnames(taxonomy_syn), fixed = TRUE)

write.csv(shiny_schiz_orig, "myapp/data/STDB.csv", row.names = FALSE)
write.csv(taxonomy_syn, "myapp/data/description_history.csv",
          row.names = FALSE)