# use this file to generate the files for the webapp
library(shinylive)

# delete shinylive assets file and folder
# in case we are updating the shinylive assets version
unlink("docs/shinylive-sw.js")
unlink("docs/shinylive", recursive = TRUE)

# update webapp
template_params <- list(
  title = "Schizomida Trait Data Base (STDB)",
  include_in_head = paste0('<link rel="icon" type="image/png" href="/Schizomida/favicon-96x96.png" sizes="96x96" />\n',
                           '    <link rel="icon" type="image/svg+xml" href="/Schizomida/favicon.svg" />\n',
                           '    <link rel="shortcut icon" href="/Schizomida/favicon.ico" />\n',
                           '    <link rel="apple-touch-icon" sizes="180x180" href="/Schizomida/apple-touch-icon.png" />\n',
                           '    <link rel="manifest" href="/Schizomida/site.webmanifest" />\n',
                           '    <link rel="shortcut icon" type="image/ico" href="/favicon.ico"/>')
  )
shinylive::export("myapp", "docs", assets_version = "0.5.0",
                  template_params = template_params)

# run this to check that everything looks ok
if (Sys.getenv("CI") == "") {
  httpuv::runStaticServer("docs")
}

# now commit and push!