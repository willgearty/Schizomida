# use this file to generate the files for the webapp
library(shinylive)
# save the index.html file to a temp file, then restore afterwards
filename <- file.path(tempdir(), "temp_index.html")
file.copy("docs/index.html", filename, overwrite = TRUE)

# if updating the shinylive assets version
# delete the "docs/shinylive" folder
# (should find a way to do this automatically in the future)

# update webapp
shinylive::export("myapp", "docs", verbose = FALSE)

# restore index.html
file.copy(filename, "docs/index.html", overwrite = TRUE)

# run this to check that everything looks ok
httpuv::runStaticServer("docs")

# now commit and push!