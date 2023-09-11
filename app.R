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
  mutate(cat = gsub(".", " ", cat, fixed = TRUE)) %>%
  mutate(tab = case_when(
    cat %in% c("Family", "Subfamily", "Genus", "Species", "Sex") ~ 1, # Taxonomy and Sex
    cat %in% c("Opisthosoma", "Spermathecae (females)", "Flagellum") ~ 3, # Opisthosoma
    cat %in% c("Size", "Legs") ~ 4, # Legs and Size
    cat %in% c("Ecology", "Distribution") ~ 5,
    .default = 2 # Prosoma
  )) %>%
  mutate(col = gsub(":", " -", col))
shiny_schiz <- shiny_schiz %>%
  row_to_names(1) %>%
  arrange(Family, Subfamily, Genus, Species, Sex) %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  mutate()
colnames(shiny_schiz) <- gsub(":", " -", colnames(shiny_schiz))
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
  # update subfamily if family is changed
  observeEvent(list(input$Family), {
    dat <- shiny_schiz
    if (!is.null(input$Family)) dat <- dat %>% filter(Family %in% input$Family)
    choices <- sort(unique(as.character(dat$Subfamily)))
    updateSelectInput(inputId = "Subfamily", choices = choices)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # update genus if family/subfamily is changed
  observeEvent(list(input$Family, input$Subfamily), {
    dat <- shiny_schiz
    if (!is.null(input$Family)) dat <- dat %>% filter(Family %in% input$Family)
    if (!is.null(input$Subfamily)) dat <- dat %>% filter(Subfamily %in% input$Subfamily)
    choices <- sort(unique(as.character(dat$Genus)))
    updateSelectInput(inputId = "Genus", choices = choices)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # update species if family/subfamily/genus is changed
  observeEvent(list(input$Family, input$Subfamily, input$Genus), {
    dat <- shiny_schiz
    if (!is.null(input$Family)) dat <- dat %>% filter(Family %in% input$Family)
    if (!is.null(input$Subfamily)) dat <- dat %>% filter(Subfamily %in% input$Subfamily)
    if (!is.null(input$Genus)) dat <- dat %>% filter(Genus %in% input$Genus)
    choices <- sort(unique(as.character(dat$Species)))
    updateSelectInput(inputId = "Species", choices = choices)
  }, ignoreNULL = FALSE)
  
  # update sex if family/subfamily/genus/species is changed
  observeEvent(list(input$Family, input$Subfamily, input$Genus, input$Species), {
    dat <- shiny_schiz
    if (!is.null(input$Family)) dat <- dat %>% filter(Family %in% input$Family)
    if (!is.null(input$Subfamily)) dat <- dat %>% filter(Subfamily %in% input$Subfamily)
    if (!is.null(input$Genus)) dat <- dat %>% filter(Genus %in% input$Genus)
    if (!is.null(input$Species)) dat <- dat %>% filter(Species %in% input$Species)
    choices <- sort(unique(as.character(dat$Sex)))
    updateSelectInput(inputId = "Sex", choices = choices)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # update male/female filters and columns when sex is changed
  observeEvent(input$sex, {
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
    cols <- colnames(data)
    for (input_name in cols) {
      input_value <- input[[input_name]]
      if (!is.null(input_value)) {
        data <- data %>%
          filter(!!as.symbol(input_name) %in% input_value)
      }
    }
    data
  },
  container = tab_layout,
  rownames = FALSE,
  style = 'bootstrap', class = 'table-bordered',
  extensions = 'Buttons',
  options = list(
    scrollX = TRUE,
    scrollY = "600px",
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
# process inputs
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
          lapply(cats$col[cats$tab == 1], function(x) {
            column(4,
                   selectInput(inputId = x, label = x,
                               choices = sort(unique(as.character(shiny_schiz[[x]]))),
                               multiple = TRUE)
            )
          })
        ),
        tabPanel(
          "Prosoma",
          lapply(unique(cats$cat[cats$tab == 2]), function(cat) {
            list(fluidRow(column(12, h4(cat))),
                 fluidRow(lapply(cats$col[cats$cat == cat], function(col) {
                   column(6,
                          selectInput(inputId = col, label = col,
                                      choices = sort(unique(as.character(shiny_schiz[[col]]))),
                                      multiple = TRUE)
                   )
                 })
                 
                 )
            )
          })
        ),
        tabPanel(
          "Opisthosoma",
          lapply(unique(cats$cat[cats$tab == 3]), function(cat) {
            list(fluidRow(column(12, h4(cat))),
                 fluidRow(lapply(cats$col[cats$cat == cat], function(col) {
                   column(6,
                          selectInput(inputId = col, label = col,
                                      choices = sort(unique(as.character(shiny_schiz[[col]]))),
                                      multiple = TRUE)
                   )
                 })
                 
                 )
            )
          })
        ),
        tabPanel(
          "Legs and Size",
          lapply(unique(cats$cat[cats$tab == 4]), function(cat) {
            list(fluidRow(column(12, h4(cat))),
                 fluidRow(lapply(cats$col[cats$cat == cat], function(col) {
                   column(6,
                          selectInput(inputId = col, label = col,
                                      choices = sort(unique(as.character(shiny_schiz[[col]]))),
                                      multiple = TRUE)
                   )
                 })
                 
                 )
            )
          })
        ),
        tabPanel(
          "Ecology and Locality",
          lapply(unique(cats$cat[cats$tab == 5]), function(cat) {
            list(fluidRow(column(12, h4(cat))),
                 fluidRow(lapply(cats$col[cats$cat == cat], function(col) {
                   column(6,
                          selectInput(inputId = col, label = col,
                                      choices = sort(unique(as.character(shiny_schiz[[col]]))),
                                      multiple = TRUE)
                   )
                 })
                 
                 )
            )
          })
        )
      )
    ),),
    mainPanel(fluidRow(DT::dataTableOutput('table')))))
}

# Run shiny ----
shinyApp(ui = ui, server = server)



