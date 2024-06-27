# use this file to generate the files for the webapp
library(shinylive)
# save the index.html file to a temp file, then restore afterwards
filename <- file.path(tempdir(), "temp_index.html")
file.copy("docs/index.html", filename, overwrite = TRUE)

# delete the "docs/shinylive" folder
# in case we are updating the shinylive assets version
unlink("docs/shinylive", recursive = TRUE)

# update webapp
shinylive::export("myapp", "docs", verbose = FALSE, assets_version = "0.3.0")

# restore index.html
file.copy(filename, "docs/index.html", overwrite = TRUE)

# run this to check that everything looks ok
httpuv::runStaticServer("docs")

# now commit and push!