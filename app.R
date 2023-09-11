# The Schizomid Trait Database - ShinyApp
# William Gearty and Sandro Pascal Müller
# 2023-09-07

# relevant packages ----
library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(DT)
library(janitor)
library(htmltools)

# load data ----
shiny_schiz <- read.xlsx('data/All schizomid species 13.1.xlsx',
                         sheet = 'All schizomid species new',
                         fillMergedCells = TRUE)
cats <- data.frame(cat = colnames(shiny_schiz), col = as.character(shiny_schiz[1, ])) %>%
  mutate(cat = gsub(".", " ", cat, fixed = TRUE))
shiny_schiz <- shiny_schiz %>%
  row_to_names(1) %>%
  arrange(Family, Subfamily, Genus, Species, Sex) %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  mutate()
male_cols <- cats$col[grepl("(male", cats$cat, fixed = TRUE) |
                        grepl("(male", cats$col, fixed = TRUE)]
female_cols <- cats$col[grepl("(female", cats$cat, fixed = TRUE) |
                          grepl("(female", cats$col, fixed = TRUE)]

# round all numeric columns to 2 decimal places

# generate table layout
double_row <- which(cats$cat == cats$col)
col_width <- table(cats$cat)[unique(cats$cat[-double_row])]
tab_layout <- withTags(table(
  class = 'display',
  thead(
    tr(
      lapply(cats$cat[double_row], th, rowspan = 2),
      lapply(seq_along(col_width), function(i) {
        th(names(col_width)[i], colspan = col_width[i])
      })
    ),
    tr(
      lapply(cats$col[-double_row], th)
    )
  )
))

# for formatting NAs
# based on https://stackoverflow.com/a/58526580/4660582
rowCallback <- c(
  "function(row, data){",
  "  $(row).find('td').each(function (index, td) {",
  "    var $td = $(td);",
  "    if ($td.html() == '') {",
  "      $td.html('NA').css({'color': 'rgb(151,151,151)', 'font-style': 'italic'})",
  "    }",
  "  })",
  "}"
)

# server.R ----
server <- function(input, output, session) {
  observeEvent(input$genus, {
    dat <- shiny_schiz
    if (!is.null(input$genus)) dat <- dat %>% filter(Genus %in% input$genus)
    choices <- sort(unique(as.character(dat$Species)))
    updateSelectInput(inputId = "species", choices = choices)
  }, ignoreNULL = FALSE)
  observeEvent(list(input$genus, input$species), {
    dat <- shiny_schiz
    if (!is.null(input$genus)) dat <- dat %>% filter(Genus %in% input$genus)
    if (!is.null(input$species)) dat <- dat %>% filter(Species %in% input$species)
    choices <- sort(unique(as.character(dat$Sex)))
    updateSelectInput(inputId = "sex", choices = choices)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  observeEvent(input$sex, {
    # show/hide male/female filters and columns
    sapply(male_cols, show)
    sapply(female_cols, show)
    if (!is.null(input$sex)) {
      if (! "male" %in% input$sex) {
        sapply(male_cols, hide)
        hideCols(dataTableProxy('table'),
                 unname(sapply(male_cols,
                               function(x) which(x == colnames(data)) - 1)))
      }
      if (! "female" %in% input$sex) {
        sapply(female_cols, hide)
        hideCols(dataTableProxy('table'),
                 unname(sapply(female_cols,
                               function(x) which(x == colnames(data)) - 1)))
      }
    }
  }, ignoreNULL = FALSE)
  data <- shiny_schiz # set the scope of this variable
  # lots of options available: https://datatables.net/reference/option/
  output$table <- DT::renderDataTable(DT::datatable({
    data <- shiny_schiz
    if(!is.null(input$genus)) {
      data <- data[data$Genus %in% input$genus,]
    }
    if(!is.null(input$species)) {
      data <- data[data$Species %in% input$species,]
    }
    if(!is.null(input$sex)) {
      data <- data[data$Sex %in% input$sex,]
    }
    if(!is.null(input$ant_pro)) {
      data <- data[data$ant_pro %in% input$ant_pro,]
    }
    if(!is.null(input$base_ant_pro)) {
      data <- data[data$base_ant_pro %in% input$base_ant_pro,]
    }
    if(!is.null(input$pro_ds)) {
      data <- data[data$pro_ds %in% input$pro_ds,]
    }
    if(!is.null(input$eyes)) {
      data <- data[data$eyes %in% input$eyes,]
    }
    if(!is.null(input$mpt)) {
      data <- data[data$mpt %in% input$mpt,]
    }
    if(!is.null(input$pp_di)) {
      data <- data[data$pp_di %in% input$pp_di,]
    }
    if(!is.null(input$pp_msl_sp)) {
      data <- data[data$pp_msl_sp %in% input$pp_msl_sp,]
    }
    if(!is.null(input$pp_tr_ap)) {
      data <- data[data$pp_tr_ap %in% input$pp_tr_ap,]
    }
    if(!is.null(input$cc_mf_gt)) {
      data <- data[data$cc_mf_gt %in% input$cc_mf_gt,]
    }
    if(!is.null(input$cc_mf_at)) {
      data <- data[data$cc_mf_at %in% input$cc_mf_at,]
    }
    if(!is.null(input$cc_if_st)) {
      data <- data[data$cc_if_st %in% input$cc_if_st,]
    }
    if(!is.null(input$os_II)) {
      data <- data[data$os_II %in% input$os_II,]
    }
    if(!is.null(input$os_elong)) {
      data <- data[data$os_elong %in% input$os_elong,]
    }
    if(!is.null(input$os_pstds_pro)) {
      data <- data[data$os_pstds_pro %in% input$os_pstds_pro,]
    }
    if(!is.null(input$os_pstds_pro_sh)) {
      data <- data[data$os_pstds_pro_sh %in% input$os_pstds_pro_sh,]
    }
    if(!is.null(input$m_flg_sh)) {
      data <- data[data$m_flg_sh %in% input$m_flg_sh,]
    }
    if(!is.null(input$f_sp_lob)) {
      data <- data[data$f_sp_lob %in% input$f_sp_lob,]
    }
    if(!is.null(input$f_sp_go)) {
      data <- data[data$f_sp_go %in% input$f_sp_go,]
    }
    if(!is.null(input$f_flgmrs)) {
      data <- data[data$f_flgmrs %in% input$f_flgmrs,]
    }
    if(!is.null(input$l_IV_mrg)) {
      data <- data[data$l_IV_mrg %in% input$l_IV_mrg,]
    }
    if(!is.null(input$hab)) {
      data <- data[data$hab %in% input$hab,]
    }
    if(!is.null(input$region)) {
      data <- data[data$region %in% input$region,]
    }
    if(!is.null(input$un_reg)) {
      data <- data[data$un_reg %in% input$un_reg,]
    }
    if(!is.null(input$dis)) {
      data <- data[data$dis %in% input$dis,]
    }
    data
  },
  container = tab_layout,
  rownames = FALSE,
  style = 'bootstrap', class = 'table-bordered',
  extensions = 'Buttons',
  options = list(
    scrollX = TRUE,
    scrollY = "725px",
    scrollCollapse = TRUE,
    paging = FALSE,
    dom = 'Bfrti',
    buttons = list(
      list(extend = "copy", exportOptions = list(columns = ":visible")),
      list(extend = "csv", exportOptions = list(columns = ":visible")),
      list(extend = "excel", exportOptions = list(columns = ":visible"))
    ),
    rowCallback = JS(rowCallback) # formatting NAs
  )) %>%
    formatStyle('Genus', fontStyle = "italic") %>%
    formatStyle('Species', fontStyle = "italic"))
}

# ui.R ----
ui <- {
  fluidPage(
    useShinyjs(),
    tags$head(tags$style("#genus option, #genus + div, #species option, #species + div {
      font-style: italic;
    }")),
    titlePanel('Schizomid Trait Database'),
    sidebarLayout(sidebarPanel(fluidRow(
      tabsetPanel(
        tabPanel(
          "Taxonomy and Sex",
          column(
            4,
            selectInput(
              inputId = 'genus',
              label = 'Genus',
              choices = sort(unique(as.character(shiny_schiz$Genus))),
              multiple = TRUE
            )
          ),
          column(
            4,
            selectInput(
              inputId = 'species',
              label = 'Species',
              choices = sort(unique(as.character(shiny_schiz$Species))),
              multiple = TRUE
            )
          ),
          column(
            4,
            selectInput(
              inputId = 'sex',
              label = 'Sex',
              choices = sort(unique(as.character(shiny_schiz$Sex))),
              multiple = TRUE
            )
          ),
        ),
        tabPanel(
          "Morphology",
          fluidRow(column(
            6,
            selectInput(
              inputId = 'ant_pro',
              label = 'Setation of anterior process',
              choices = sort(unique(as.character(
                shiny_schiz$ant_pro
              ))),
              multiple = TRUE
            )
          ),
          column(
            6,
            selectInput(
              inputId = 'base_ant_pro',
              label = 'Setation of base of anterior process',
              choices = sort(unique(as.character(
                shiny_schiz$base_ant_pro
              ))),
              multiple = TRUE
            )
          )),
          fluidRow(column(
            6,
            selectInput(
              inputId = 'pro_ds',
              label = 'Dorsal setation of propeltidium',
              choices = sort(unique(as.character(shiny_schiz$pro_ds))),
              multiple = TRUE
            )
          ),
          column(
            6,
            selectInput(
              inputId = 'eyes',
              label = 'State of vision',
              choices = sort(unique(as.character(shiny_schiz$eyes))),
              multiple = TRUE
            )
          )),
          fluidRow(column(
            4,
            selectInput(
              inputId = 'mpt',
              label = 'State of metapeltidium',
              choices = sort(unique(as.character(shiny_schiz$mpt))),
              multiple = TRUE
            )
          ),
          column(
            4,
            selectInput(
              inputId = 'pp_di',
              label = 'Dimorphism in pedipalps',
              choices = sort(unique(as.character(shiny_schiz$pp_di))),
              multiple = TRUE
            )
          )),
          fluidRow(column(
            6,
            selectInput(
              inputId = 'pp_msl_sp',
              label = 'Mesal spur on pedipalp trochanter',
              choices = sort(unique(as.character(
                shiny_schiz$pp_msl_sp
              ))),
              multiple = TRUE
            )
          ),
          column(
            6,
            selectInput(
              inputId = 'pp_tr_ap',
              label = 'Apical process of pedipalp trochanter',
              choices = sort(unique(as.character(
                shiny_schiz$pp_tr_ap
              ))),
              multiple = TRUE
            )
          )),
          fluidRow(column(
            6,
            selectInput(
              inputId = 'cc_mf_gt',
              label = 'Guard tooth on movable finger of chelicerae',
              choices = sort(unique(as.character(
                shiny_schiz$cc_mf_gt
              ))),
              multiple = TRUE
            )
          ),
          column(
            6,
            selectInput(
              inputId = 'cc_mf_at',
              label = 'Accessory tooth/teeth on movable finger of chelicerae',
              choices = sort(unique(as.character(
                shiny_schiz$cc_mf_at
              ))),
              multiple = TRUE
            )
          )),
          fluidRow(
            column(
              6,
              selectInput(
                inputId = 'cc_if_st',
                label = 'Small tooth/teeth on fixed finger of chelicerae',
                choices = sort(unique(as.character(
                  shiny_schiz$cc_if_st
                ))),
                multiple = TRUE
              )
            ),
            column(
              3,
              selectInput(
                inputId = 'os_II',
                label = 'Setation of tergite II',
                choices = sort(unique(as.character(shiny_schiz$os_II))),
                multiple = TRUE
              )
            ),
            column(
              3,
              selectInput(
                inputId = 'os_elong',
                label = 'Elongation of opisthosoma',
                choices = sort(unique(as.character(
                  shiny_schiz$os_elong
                ))),
                multiple = TRUE
              )
            )
          ),
          fluidRow(column(
            6,
            selectInput(
              inputId = 'os_pstds_pro',
              label = 'Posterodorsal process opisthosomal segment XII',
              choices = sort(unique(as.character(
                shiny_schiz$os_pstds_pro
              ))),
              multiple = TRUE
            )
          ),
          column(
            6,
            selectInput(
              inputId = 'os_pstds_pro_sh',
              label = 'Shape of posterodorsal process',
              choices = sort(unique(
                as.character(shiny_schiz$os_pstds_pro_sh)
              )),
              multiple = TRUE
            )
          )),
          fluidRow(column(
            6,
            selectInput(
              inputId = 'm_flg_sh',
              label = 'Dorsal shape of male flagellum',
              choices = sort(unique(as.character(
                shiny_schiz$m_flg_sh
              ))),
              multiple = TRUE
            )
          ),
          column(
            6,
            selectInput(
              inputId = 'f_sp_lob',
              label = 'Number of lobes of female spermatheca',
              choices = sort(unique(as.character(
                shiny_schiz$f_sp_lob
              ))),
              multiple = TRUE
            )
          )),
          fluidRow(column(
            6,
            selectInput(
              inputId = 'f_sp_go',
              label = 'Gonopod of female spermatheca',
              choices = sort(unique(as.character(
                shiny_schiz$f_sp_go
              ))),
              multiple = TRUE
            )
          ),
          column(
            6,
            selectInput(
              inputId = 'f_flgmrs',
              label = 'Number of flagellomeres in female flagellum',
              choices = sort(unique(as.character(
                shiny_schiz$f_flgmrs
              ))),
              multiple = TRUE
            )
          )),
          fluidRow(column(
            6,
            selectInput(
              inputId = 'l_IV_mrg',
              label = 'Anterodorsal margin of femur leg IV',
              choices = sort(unique(as.character(
                shiny_schiz$l_IV_mrg
              ))),
              multiple = TRUE
            )
          ))
        ),
        tabPanel(
          "Ecology and Locality",
          fluidRow(column(
            6,
            selectInput(
              inputId = 'hab',
              label = 'Ecology',
              choices = sort(unique(as.character(shiny_schiz$hab))),
              multiple = TRUE
            )
          ),
          column(
            6,
            selectInput(
              inputId = 'region',
              label = 'Type locality (country)',
              choices = sort(unique(as.character(shiny_schiz$region))),
              multiple = TRUE
            )
          )),
          fluidRow(column(
            6,
            selectInput(
              inputId = 'un_reg',
              label = 'Type locality (UN region)',
              choices = sort(unique(as.character(shiny_schiz$un_reg))),
              multiple = TRUE
            )
          ),
          column(
            6,
            selectInput(
              inputId = 'dis',
              label = 'Type locality (distribution/stratigraphy)',
              choices = sort(unique(as.character(shiny_schiz$dis))),
              multiple = TRUE
            )
          ))
        )
      )
    ),),
    mainPanel(fluidRow(DT::dataTableOutput('table')))))
}

# Run shiny ----
shinyApp(ui = ui, server = server)



