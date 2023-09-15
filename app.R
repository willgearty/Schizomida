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
shiny_schiz_orig <- read.xlsx('data/All schizomid species 13.1.xlsx',
                              sheet = 'All schizomid species new',
                              fillMergedCells = TRUE)

# make a function to clean strings for use as element ids
idEscape <- function(x) {
  gsub("[^a-zA-Z0-9_]", "-", x)
}

cols <- data.frame(cat = colnames(shiny_schiz_orig),
                   col = as.character(shiny_schiz_orig[1, ])) %>%
  mutate(cat = gsub(".", " ", cat, fixed = TRUE)) %>%
  mutate(tab = case_when(
    cat %in% c("Family", "Subfamily", "Genus", "Species", "Sex") ~ 1, # Taxonomy and Sex
    cat %in% c("Opisthosoma", "Spermathecae (females)", "Flagellum") ~ 3, # Opisthosoma
    cat %in% c("Size", "Legs") ~ 4, # Legs and Size
    cat %in% c("Ecology", "Distribution") ~ 5,
    .default = 2 # Prosoma
  )) %>%
  mutate(col_clean = idEscape(col)) # replace special characters with dashes

shiny_schiz <- shiny_schiz_orig %>%
  row_to_names(1) %>%
  arrange(Family, Subfamily, Genus, Species, Sex) %>%
  mutate(across(where(is.character), ~na_if(., "")))

male_names <- Reduce(union,
                    list(
                      grep("(male", cols$col, fixed = TRUE, value = TRUE),
                      cols$col[grepl("(male", cols$cat, fixed = TRUE)],
                      grep("(male", cols$cat, fixed = TRUE, value = TRUE)
                    ))
female_names <- Reduce(union,
                      list(
                        grep("(female", cols$col, fixed = TRUE, value = TRUE),
                        cols$col[grepl("(female", cols$cat, fixed = TRUE)],
                        grep("(female", cols$cat, fixed = TRUE, value = TRUE)
                      ))

# round all numeric columns to 2 decimal places

# generate table layout
double_row <- which(cols$cat == cols$col)
col_width <- table(cols$cat)[unique(cols$cat[-double_row])]
tab_layout <- withTags(table(
  class = 'display',
  thead(
    tr(
      lapply(cols$cat[double_row], th, rowspan = 2),
      lapply(seq_along(col_width), function(i) {
        th(names(col_width)[i], colspan = col_width[i])
      })
    ),
    tr(
      lapply(cols$col[-double_row], th)
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
  data <- shiny_schiz # set the scope of this variable
  
  update_cols <- function(input) {
    show_cols <- c()
    hide_cols <- c()
    for(name in names(input)[grep("-checkbox", names(input), fixed = TRUE)]) {
      # convert to unclean name
      name_unclean <- cols$col[match(gsub("-checkbox", "", name), cols$col_clean)]
      if(input[[name]]) {
        show_cols <- c(show_cols, name_unclean)
      } else {
        hide_cols <- c(hide_cols, name_unclean)
      }
    }
    showCols(dataTableProxy('table'),
             unname(sapply(show_cols,
                           function(x) which(x == colnames(data)) - 1)))
    hideCols(dataTableProxy('table'),
             unname(sapply(hide_cols,
                           function(x) which(x == colnames(data)) - 1)))
  }
  
  # reset all filters if button is pressed
  observeEvent(input$reset_input, {
    reset("side-panel")
    runjs("$('input[type=checkbox]').prop('checked', true).trigger('change')")
  })
  
  # monitor checkboxes and show/hide specified columns
  observeEvent(
    lapply(
      names(input)[grep("checkbox", names(input))],
      function(name) {
        input[[name]]
      }
    ),
    {
      update_cols(input)
    },
    ignoreInit = TRUE
  )
  
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
  observeEvent(input$Sex, {
    sapply(idEscape(male_names), showElement)
    sapply(idEscape(female_names), showElement)
    if (!is.null(input$Sex)) {
      if (! "male" %in% input$Sex) {
        sapply(idEscape(male_names), hideElement)
        hideCols(dataTableProxy('table'),
                 unname(sapply(male_names,
                               function(x) which(x == colnames(data)) - 1)))
      }
      if (! "female" %in% input$Sex) {
        sapply(idEscape(female_names), hideElement)
        hideCols(dataTableProxy('table'),
                 unname(sapply(female_names,
                               function(x) which(x == colnames(data)) - 1)))
      }
    }
  }, ignoreNULL = FALSE)
  
  # lots of options available: https://datatables.net/reference/option/
  output$table <- DT::renderDataTable(DT::datatable({
    data <- shiny_schiz
    for (i in seq_len(nrow(cols))) {
      input_value <- input[[cols$col_clean[i]]]
      if (!is.null(input_value)) {
        data <- data %>%
          filter(!!as.symbol(cols$col[i]) %in% input_value)
      }
    }
    update_cols(input)
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
      list(extend = "copy", text = "Copy Table", exportOptions = list(columns = ":visible")),
      list(extend = "csv", text = HTML("<i class='fas fa-download'></i> CSV"),
           exportOptions = list(columns = ":visible")),
      list(extend = "excel", text = HTML("<i class='fas fa-download'></i> Excel"),
           exportOptions = list(columns = ":visible"))
    ),
    rowCallback = JS(rowCallback) # formatting NAs
  )) %>%
    formatStyle('Genus', fontStyle = "italic") %>%
    formatStyle('Species', fontStyle = "italic"))
  
  # setup blank sheet for download
  output$downloadData <- downloadHandler(
    filename = 'empty-schizomida.xlsx',
    content = function(con) {
      # build workbook, merge cells, then write
      wb <- buildWorkbook(as.data.frame(t(cols[,1:2])), colNames = FALSE)
      for (col in double_row) {
        mergeCells(wb, 1, cols = col, rows = 1:2)
      }
      i <- double_row[length(double_row)] + 1
      for (n in col_width) {
        mergeCells(wb, 1, cols = i:(i + n - 1), rows = 1)
        i <- i + n
      }
      saveWorkbook(wb, con)
    }
  )
}

# ui.R ----
# process inputs
ui <- {
  fluidPage(
    useShinyjs(),
    tags$head(tags$style(
      HTML(
        "#Genus option, #Genus + div, #Species option, #Species + div {
           font-style: italic;
         }
         .dataTables_wrapper {
           padding-right: 10px;
         }
         div.col-sm-6:has(> div.form-group[style*='display: none']) {
           display: none;
         }
         .dataTables_filter {
           float: right;
         }
         h4 {
           font-style: italic;
         }"
      )
    )),
    titlePanel('Schizomid Trait Database'),
    sidebarLayout(sidebarPanel(fluidRow(
      tabsetPanel(
        tabPanel(
          "Taxonomy and Sex",
          fluidRow(column(12, h4(""))), # add a little spacing
          fluidRow(apply(cols %>% filter(tab == 1), 1, function(row) {
            column(4,
                   selectInput(inputId = row["col_clean"],
                               label = HTML(paste0(row["col"],
                                                   " <input type = 'checkbox' checked id = ",
                                                   "'", row["col_clean"], "-checkbox' ",
                                                   "title = 'show/hide this column'>")),
                               choices = sort(unique(as.character(shiny_schiz[[row["col"]]]))),
                               multiple = TRUE))
          }))
        ),
        tabPanel(
          "Prosoma",
          lapply(unique(cols$cat[cols$tab == 2]), function(cat_name) {
            list(fluidRow(column(12, h4(cat_name))),
                 fluidRow(apply(cols %>% filter(cat == cat_name), 1, function(row) {
                   column(6,
                          selectInput(inputId = row["col_clean"],
                                      label = HTML(paste0(row["col"],
                                                          " <input type = 'checkbox' checked id = ",
                                                          "'", row["col_clean"], "-checkbox' ",
                                                          "title = 'show/hide this column'>")),
                                      choices = sort(unique(as.character(shiny_schiz[[row["col"]]]))),
                                      multiple = TRUE))
                 })))
          })
        ),
        tabPanel(
          "Opisthosoma",
          lapply(unique(cols$cat[cols$tab == 3]), function(cat_name) {
            list(fluidRow(column(12, h4(cat_name, id = idEscape(cat_name)))),
                 fluidRow(apply(cols %>% filter(cat == cat_name), 1, function(row) {
                   column(6,
                          selectInput(inputId = row["col_clean"],
                                      label = HTML(paste0(row["col"],
                                                          " <input type = 'checkbox' checked id = ",
                                                          "'", row["col_clean"], "-checkbox' ",
                                                          "title = 'show/hide this column'>")),
                                      choices = sort(unique(as.character(shiny_schiz[[row["col"]]]))),
                                      multiple = TRUE))
                 })))
          })
        ),
        tabPanel(
          "Legs and Size",
          lapply(unique(cols$cat[cols$tab == 4]), function(cat_name) {
            list(fluidRow(column(12, h4(cat_name))),
                 fluidRow(apply(cols %>% filter(cat == cat_name), 1, function(row) {
                   column(6,
                          selectInput(inputId = row["col_clean"],
                                      label = HTML(paste0(row["col"],
                                                          " <input type = 'checkbox' checked id = ",
                                                          "'", row["col_clean"], "-checkbox' ",
                                                          "title = 'show/hide this column'>")),
                                      choices = sort(unique(as.character(shiny_schiz[[row["col"]]]))),
                                      multiple = TRUE))
                 })))
          })
        ),
        tabPanel(
          "Ecology and Locality",
          lapply(unique(cols$cat[cols$tab == 5]), function(cat_name) {
            list(fluidRow(column(12, h4(cat_name))),
                 fluidRow(apply(cols %>% filter(cat == cat_name), 1, function(row) {
                   column(6,
                          selectInput(inputId = row["col_clean"],
                                      label = HTML(paste0(row["col"],
                                                          " <input type = 'checkbox' checked id = ",
                                                          "'", row["col_clean"], "-checkbox' ",
                                                          "title = 'show/hide this column'>")),
                                      choices = sort(unique(as.character(shiny_schiz[[row["col"]]]))),
                                      multiple = TRUE))
                 })))
          })
        )
      )
    ),
    fluidRow(actionButton("reset_input", "Reset all filters"),
             downloadButton("downloadData", "Download blank excel sheet")),
    id = "side-panel",
    ),
    mainPanel(fluidRow(DT::dataTableOutput('table'))))
  )
}

# Run shiny ----
shinyApp(ui = ui, server = server)



