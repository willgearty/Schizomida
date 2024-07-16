# use this file to generate the files for the webapp
library(shinylive)

# delete shinylive assets file and folder
# in case we are updating the shinylive assets version
unlink("docs/shinylive-sw.js")
unlink("docs/shinylive", recursive = TRUE)

# update webapp
# TODO: use template_params once shinylive assets > 0.4.1
shinylive::export("myapp", "docs", quiet = TRUE, assets_version = "0.4.1",
                  template_dir = "templates/")

# run this to check that everything looks ok
if (Sys.getenv("CI") == "") {
  httpuv::runStaticServer("docs")
}

# now commit and push!