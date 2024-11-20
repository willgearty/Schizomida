# use this file to generate the files for the webapp
library(shinylive)
library(metathis)
library(dplyr)

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
                           '    <link rel="shortcut icon" type="image/ico" href="/favicon.ico"/>\n',
                           '    ',
                           paste0(meta() %>%
                                    meta_description(
                                      "The Schizomida Trait Data Base hosts taxonomic and morphological data on extant and extinct short-tailed whip-scorpions. The Data Base and website infrastructure are entirely open-source. The Data Base is hosted as a webapp using the shiny R framework and the webR/shinylive serverless infrastructure."
                                    ) %>% 
                                    meta_name("github-repo" = "willgearty/Schizomida") %>% 
                                    meta_viewport() %>% 
                                    meta_social(
                                      title = "Schizomida Trait Data Base (STDB)",
                                      url = "https://williamgearty.com/Schizomida/",
                                      image = "https://williamgearty.com/Schizomida/schizomida.jpg",
                                      image_alt = "A photo of Bucinozomus hortuspalmarum (c) Melvyn Yeo",
                                      og_type = "website",
                                      og_author = c("William Gearty", "Sandro Pascal Müller", "Ilian De Francesco Magnussen"),
                                      twitter_card_type = "summary",
                                      twitter_creator = "@willgearty"
                                    ), collapse = '\n    '))
  )
shinylive::export("myapp", "docs", assets_version = "0.5.0",
                  template_params = template_params)

# run this to check that everything looks ok
if (Sys.getenv("CI") == "") {
  httpuv::runStaticServer("docs")
}

# now commit and push!