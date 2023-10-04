# The Schizomid Trait Database - ShinyApp
# William Gearty and Sandro Pascal Müller
# 2023-09-07

# relevant packages ----
library(shiny)
library(shinyjs)
library(dplyr)
library(openxlsx)
library(DT)
library(janitor)
library(htmltools)
library(fontawesome)
library(prompter)
library(Hmisc)

# load data ----
shiny_schiz_orig <- read.xlsx('data/STDB_data.xlsx',
                              sheet = 'STDB',
                              fillMergedCells = TRUE)

# edit references subheading
shiny_schiz_orig[1, ncol(shiny_schiz_orig)] <- "References"

taxonomy_syn <- read.xlsx('data/STDB_data.xlsx',
                          sheet = 'Species description history')
colnames(taxonomy_syn) <- gsub(".", " ", colnames(taxonomy_syn), fixed = TRUE)

# convert docx to filtered html, then convert to UTF-8
references_html <- includeHTML("data/STDB_references.htm")

# clean data ----
# make a function to clean strings for use as element ids
idEscape <- function(x) {
  gsub("[^a-zA-Z0-9_]", "-", x)
}

# set up table of headings and subheadings
cols <- data.frame(cat = colnames(shiny_schiz_orig),
                   col = as.character(shiny_schiz_orig[1, ])) %>%
  mutate(cat = gsub(".", " ", cat, fixed = TRUE)) %>%
  mutate(tab = case_when(
    cat %in% c("Family", "Subfamily", "Genus", "Species", "Sex") ~ 1, # Taxonomy and Sex
    cat %in% c("Opisthosoma", "Spermathecae (female)", "Flagellum") ~ 3, # Opisthosoma
    cat %in% c("Size", "Legs") ~ 4, # Legs and Size
    cat %in% c("Ecology", "Distribution") ~ 5,
    grepl("References", cat) ~ 6,
    .default = 2 # Prosoma
  )) %>%
  mutate(col_clean = idEscape(col)) # replace special characters with dashes

shiny_schiz <- shiny_schiz_orig %>%
  row_to_names(1) %>%
  arrange(Family, Subfamily, Genus, Species, Sex) %>%
  mutate(across(where(is.character), ~na_if(., "")))

# Find numeric columns
shiny_schiz_clean <- shiny_schiz
shiny_schiz_clean[shiny_schiz == "No data"] <- ""
num_cols <- apply(shiny_schiz_clean, 2, all.is.numeric)

# Round and save numeric values
shiny_schiz[, num_cols] <- round(apply(shiny_schiz_clean[, num_cols], 2, all.is.numeric, what = "vector"),
                                 digits = 2)

# Get male and female columns
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

# table layout and formatting ----
# generate table layout
ref_col <- grep("References", cols$cat)
double_row <- which(cols$cat == cols$col)
col_width <- table(cols$cat)[unique(cols$cat[-double_row])]
tab_layout <- withTags(table(
  class = 'display',
  thead(
    tr(
      lapply(cols$cat[double_row[double_row != ref_col]], th, rowspan = 2),
      lapply(seq_along(col_width), function(i) {
        th(names(col_width)[i], colspan = col_width[i])
      }),
      th(cols$cat[ref_col], rowspan = 2)
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
  proxy1 <- dataTableProxy('table1')
  proxy2 <- dataTableProxy('table2')
  
  # observers ----
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
    # hide columns in database
    showCols(proxy1,
             unname(sapply(show_cols,
                           function(x) which(x == colnames(data)) - 1)))
    hideCols(proxy1,
             unname(sapply(hide_cols,
                           function(x) which(x == colnames(data)) - 1)))
    # hide columns in second table
    showCols(proxy2,
             unname(sapply(show_cols,
                           function(x) which(x == colnames(data)) - 1)))
    hideCols(proxy2,
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
  
  js_export <- function(format = 'xlsx', empty = FALSE) {
    paste0("
    $('.buttons-collection').prop('disabled', true);
    var $table = $('.dataTables_scrollBody table:visible');
    var nrows = $table.find('tbody tr').length;
    var instance = $table.tableExport({
      formats: ['", format, "'],
      filename: 'schizomida", ifelse(empty, "_empty", ""), "',
      sheetname: 'Sheet1',
      ignoreRows: ", ifelse(empty, "Array(nrows).fill().map((v,i)=>i)", "null"), "
    });
    $('.export-type-", format, "').click();
    $('.tableexport-caption').remove();
    // var exportData0 = instance.getExportData();
    // var exportData = exportData0[Object.keys(exportData0)[0]]['", format, "'];
    // instance.export2file(exportData.data, exportData.mimeType, exportData.filename, 
    //                      exportData.fileExtension, exportData.merges, 
    //                      exportData.RTL, exportData.sheetname);
    $('.buttons-collection').prop('disabled', false);
    ")
  }
  # trigger excel file download
  observeEvent(input$downloadExcel, {
    runjs(js_export())
  })
  
  # trigger csv file download
  observeEvent(input$downloadCSV, {
    runjs(js_export("csv"))
  })
  
  # trigger empty excel file download
  observeEvent(input$downloadEmptyExcel, {
    runjs(js_export(empty = TRUE))
  })
  
  # update subfamily if family is changed
  observeEvent(list(input$Family), {
    dat <- shiny_schiz
    if (!is.null(input$Family)) dat <- dat %>% filter(Family %in% input$Family)
    choices <- sort(unique(as.character(dat$Subfamily)))
    updateSelectizeInput(inputId = "Subfamily", choices = choices, server = TRUE)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # update genus if family/subfamily is changed
  observeEvent(list(input$Family, input$Subfamily), {
    dat <- shiny_schiz
    if (!is.null(input$Family)) dat <- dat %>% filter(Family %in% input$Family)
    if (!is.null(input$Subfamily)) dat <- dat %>% filter(Subfamily %in% input$Subfamily)
    choices <- sort(unique(as.character(dat$Genus)))
    updateSelectizeInput(inputId = "Genus", choices = choices, server = TRUE)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # update species if family/subfamily/genus is changed
  observeEvent(list(input$Family, input$Subfamily, input$Genus), {
    dat <- shiny_schiz
    if (!is.null(input$Family)) dat <- dat %>% filter(Family %in% input$Family)
    if (!is.null(input$Subfamily)) dat <- dat %>% filter(Subfamily %in% input$Subfamily)
    if (!is.null(input$Genus)) dat <- dat %>% filter(Genus %in% input$Genus)
    choices <- sort(unique(as.character(dat$Species)))
    updateSelectizeInput(inputId = "Species", choices = choices, server = TRUE)
  }, ignoreNULL = FALSE)
  
  # update sex if family/subfamily/genus/species is changed
  observeEvent(list(input$Family, input$Subfamily, input$Genus, input$Species), {
    dat <- shiny_schiz
    if (!is.null(input$Family)) dat <- dat %>% filter(Family %in% input$Family)
    if (!is.null(input$Subfamily)) dat <- dat %>% filter(Subfamily %in% input$Subfamily)
    if (!is.null(input$Genus)) dat <- dat %>% filter(Genus %in% input$Genus)
    if (!is.null(input$Species)) dat <- dat %>% filter(Species %in% input$Species)
    choices <- sort(unique(as.character(dat$Sex)))
    updateSelectizeInput(inputId = "Sex", choices = choices, server = TRUE)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # update male/female filters when sex is changed
  observeEvent(input$Sex, {
    sapply(idEscape(male_names), showElement)
    sapply(idEscape(female_names), showElement)
    if (!is.null(input$Sex)) {
      if (! "male" %in% input$Sex) {
        sapply(idEscape(male_names), hideElement)
      }
      if (! "female" %in% input$Sex) {
        sapply(idEscape(female_names), hideElement)
      }
    }
  }, ignoreNULL = FALSE)
  
  # data table ----
  # lots of options available: https://datatables.net/reference/option/
  output$table1 <- DT::renderDataTable(DT::datatable({
    data <- shiny_schiz
    for (i in seq_len(nrow(cols))) {
      input_value <- input[[cols$col_clean[i]]]
      if (!is.null(input_value)) {
        if (is.numeric(shiny_schiz[[cols$col[i]]])) {
          min_val <- floor(min(shiny_schiz[[cols$col[i]]], na.rm = TRUE))
          max_val <- ceiling(max(shiny_schiz[[cols$col[i]]], na.rm = TRUE))
          if (input_value[1] > min_val | input_value[2] < max_val) {
            data <- data %>%
              filter(!!as.symbol(cols$col[i]) >= input_value[1],
                     !!as.symbol(cols$col[i]) <= input_value[2])
          }
        } else {
          data <- data %>%
            filter(!!as.symbol(cols$col[i]) %in% input_value)
        }
      }
    }
    # update columns based on checkboxes
    update_cols(input)
    # update male/female columns based on sex filter
    if (!is.null(input$Sex)) {
      if (! "male" %in% input$Sex) {
        hideCols(proxy1,
                 unname(sapply(male_names,
                               function(x) which(x == colnames(data)) - 1)))
      }
      if (! "female" %in% input$Sex) {
        hideCols(proxy1,
                 unname(sapply(female_names,
                               function(x) which(x == colnames(data)) - 1)))
      }
    }
    data
  },
  container = tab_layout,
  rownames = FALSE,
  style = 'bootstrap', class = 'table-bordered',
  selection = 'none',
  extensions = 'Buttons',
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '200px', targets = "_all")),
    scrollX = TRUE,
    scrollY = TRUE,
    scrollCollapse = TRUE,
    paging = FALSE,
    dom = 'Bfrti',
    buttons = list(
      list(extend = "copy", text = paste(fa("clipboard"),  "Copy"),
           exportOptions = list(columns = ":visible"),
           className = "hint--bottom-right hint--rounded hint--info",
           attr = list("aria-label" = "Copy the below table to the clipboard")),
      list(extend = "collection", text = paste(fa("download", prefer_type = "solid"), "CSV"),
           action = JS("function ( e, dt, node, config ) {
                               Shiny.setInputValue('downloadCSV', true, {priority: 'event'});
                             }"),
           className = "hint--bottom-right hint--rounded hint--info",
           attr = list("aria-label" = "Download a copy of the below table in CSV format")
      ),
      list(extend = "collection", text = paste(fa("download", prefer_type = "solid"), "Excel"),
           action = JS("function ( e, dt, node, config ) {
                               Shiny.setInputValue('downloadExcel', true, {priority: 'event'});
                             }"),
           className = "hint--bottom-right hint--rounded hint--info",
           attr = list("aria-label" = "Download a copy of the below table in Excel format")
      ),
      list(extend = "collection", text = paste(fa("download", prefer_type = "solid"), "Empty Excel"),
           action = JS("function ( e, dt, node, config ) {
                               Shiny.setInputValue('downloadEmptyExcel', true, {priority: 'event'});
                             }"),
           className = "hint--bottom-right hint--rounded hint--info",
           attr = list("aria-label" = "Download an empty copy of the below table in Excel format (only colomn headers)")
      )
    ),
    rowCallback = JS(rowCallback) # formatting NAs
  )) %>%
    formatStyle('Genus', fontStyle = "italic") %>%
    formatStyle('Species', fontStyle = "italic"))
  
  # tax. and syn. table ----
  output$table2 <- DT::renderDataTable(DT::datatable({
    data2 <- taxonomy_syn
    for (col in c("Family", "Subfamily", "Genus", "Species")) {
      input_value <- input[[col]]
      if (!is.null(input_value)) {
        if (col == "Species") {
          input_split <- gsub("†", "", strsplit(input_value, " ")[[1]])
          input_split[1] <- substr(input_split[1], 1, 1)
          input_value <- paste0(input_split[1], ". ", input_split[2])
        }
        input_value <- gsub("†", "", input_value)
        data2 <- data2 %>%
          filter(grepl(input_value, !!as.symbol(col), fixed = TRUE))
      }
    }
    # update columns based on checkboxes
    update_cols(input)
    data2
  },
  rownames = FALSE,
  style = 'bootstrap', class = 'table-bordered',
  selection = 'none',
  extensions = 'Buttons',
  options = list(
    scrollX = TRUE,
    scrollY = TRUE,
    scrollCollapse = TRUE,
    paging = FALSE,
    dom = 'Bfrti',
    buttons = list(
      list(extend = "copy", text = paste(fa("clipboard"),  "Copy"),
           exportOptions = list(columns = ":visible"),
           className = "hint--bottom-right hint--rounded hint--info",
           attr = list("aria-label" = "Copy the below table to the clipboard")),
      list(extend = "collection", text = paste(fa("download", prefer_type = "solid"), "CSV"),
           action = JS("function ( e, dt, node, config ) {
                               Shiny.setInputValue('downloadCSV', true, {priority: 'event'});
                             }"),
           className = "hint--bottom-right hint--rounded hint--info",
           attr = list("aria-label" = "Download a copy of the below table in CSV format")
      ),
      list(extend = "collection", text = paste(fa("download", prefer_type = "solid"), "Excel"),
           action = JS("function ( e, dt, node, config ) {
                               Shiny.setInputValue('downloadExcel', true, {priority: 'event'});
                             }"),
           className = "hint--bottom-right hint--rounded hint--info",
           attr = list("aria-label" = "Download a copy of the below table in Excel format")
      )
    ),
    rowCallback = JS(rowCallback) # formatting NAs
  )))
}

# ui.R ----
# process inputs
ui <- {
  fluidPage(
    useShinyjs(),
    use_prompt(),
    tags$head(
      tags$style(
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
         }
         .dataTables_scrollBody {
           max-height: 62vh;
           max-height: 62dvh;
         }
         .tableexport-caption {
           display: none;
         }
         #side-panel .tab-content {
           height: 70vh;
           height: 70dvh;
           overflow-y: auto;
           overflow-x: hidden;
         }"
        ),
      ),
      tags$script(src = "xlsx.core.min.js"),
      tags$script(src = "FileSaver.min.js"),
      tags$script(src = "tableexport.min.js")
    ),
    titlePanel('Schizomid Trait Database'),
    sidebarLayout(sidebarPanel(fluidRow(
      tabsetPanel(
        tabPanel(
          "Taxonomy and Sex",
          fluidRow(column(12, h4(""))), # add a little spacing
          fluidRow(lapply(seq_len(cols %>% filter(tab == 1) %>% nrow()), function(i) {
            column(4,
                   selectInput(inputId = cols$col_clean[i],
                               label = HTML(paste0(cols$col[i],
                                                   " <input type = 'checkbox' checked id = ",
                                                   "'", cols$col_clean[i], "-checkbox' ",
                                                   "aria-label = 'show/hide this column'",
                                                   "class = '",
                                                   ifelse((i %% 3) == 0, "hint--bottom-left", "hint--bottom-right"),
                                                   " hint--rounded'>")),
                               choices = sort(unique(as.character(shiny_schiz[[cols$col[i]]]))),
                               multiple = TRUE))
          }))
        ),
        tabPanel(
          "Prosoma",
          lapply(unique(cols$cat[cols$tab == 2]), function(cat_name) {
            cols_sub <- cols %>% filter(cat == cat_name)
            list(fluidRow(column(12, h4(cat_name, id = idEscape(cat_name)))),
                 fluidRow(lapply(seq_len(nrow(cols_sub)), function(i) {
                   if (is.numeric(shiny_schiz[[cols_sub$col[i]]])) {
                     min_val <- floor(min(shiny_schiz[[cols_sub$col[i]]], na.rm = TRUE))
                     max_val <- ceiling(max(shiny_schiz[[cols_sub$col[i]]], na.rm = TRUE))
                     column(6, sliderInput(inputId = cols_sub$col_clean[i],
                                           label = HTML(paste0(cols_sub$col[i],
                                                               " <input type = 'checkbox' checked id = ",
                                                               "'", cols_sub$col_clean[i], "-checkbox' ",
                                                               "aria-label = 'show/hide this column'",
                                                               "class = '",
                                                               ifelse((i %% 2) == 1, "hint--bottom-right", "hint--bottom-left"),
                                                               " hint--rounded'>")),
                                           min = min_val,
                                           max = max_val,
                                           value = c(min_val, max_val),
                                           step = .01))
                   } else {
                     column(6, selectInput(inputId = cols_sub$col_clean[i],
                                           label = HTML(paste0(cols_sub$col[i],
                                                               " <input type = 'checkbox' checked id = ",
                                                               "'", cols_sub$col_clean[i], "-checkbox' ",
                                                               "aria-label = 'show/hide this column'",
                                                               "class = '",
                                                               ifelse((i %% 2) == 1, "hint--bottom-right", "hint--bottom-left"),
                                                               " hint--rounded'>")),
                                           choices = sort(unique(as.character(shiny_schiz[[cols_sub$col[i]]]))),
                                           multiple = TRUE))
                   }
                 })))
          })
        ),
        tabPanel(
          "Opisthosoma",
          lapply(unique(cols$cat[cols$tab == 3]), function(cat_name) {
            cols_sub <- cols %>% filter(cat == cat_name)
            list(fluidRow(column(12, h4(cat_name, id = idEscape(cat_name)))),
                 fluidRow(lapply(seq_len(nrow(cols_sub)), function(i) {
                   if (is.numeric(shiny_schiz[[cols_sub$col[i]]])) {
                     min_val <- floor(min(shiny_schiz[[cols_sub$col[i]]], na.rm = TRUE))
                     max_val <- ceiling(max(shiny_schiz[[cols_sub$col[i]]], na.rm = TRUE))
                     column(6, sliderInput(inputId = cols_sub$col_clean[i],
                                           label = HTML(paste0(cols_sub$col[i],
                                                               " <input type = 'checkbox' checked id = ",
                                                               "'", cols_sub$col_clean[i], "-checkbox' ",
                                                               "aria-label = 'show/hide this column'",
                                                               "class = '",
                                                               ifelse((i %% 2) == 1, "hint--bottom-right", "hint--bottom-left"),
                                                               " hint--rounded'>")),
                                           min = min_val,
                                           max = max_val,
                                           value = c(min_val, max_val),
                                           step = .01))
                   } else {
                     column(6, selectInput(inputId = cols_sub$col_clean[i],
                                           label = HTML(paste0(cols_sub$col[i],
                                                               " <input type = 'checkbox' checked id = ",
                                                               "'", cols_sub$col_clean[i], "-checkbox' ",
                                                               "aria-label = 'show/hide this column'",
                                                               "class = '",
                                                               ifelse((i %% 2) == 1, "hint--bottom-right", "hint--bottom-left"),
                                                               " hint--rounded'>")),
                                           choices = sort(unique(as.character(shiny_schiz[[cols_sub$col[i]]]))),
                                           multiple = TRUE))
                   }
                 })))
          })
        ),
        tabPanel(
          "Legs and Size",
          lapply(unique(cols$cat[cols$tab == 4]), function(cat_name) {
            cols_sub <- cols %>% filter(cat == cat_name)
            list(fluidRow(column(12, h4(cat_name, id = idEscape(cat_name)))),
                 fluidRow(lapply(seq_len(nrow(cols_sub)), function(i) {
                   if (is.numeric(shiny_schiz[[cols_sub$col[i]]])) {
                     min_val <- floor(min(shiny_schiz[[cols_sub$col[i]]], na.rm = TRUE))
                     max_val <- ceiling(max(shiny_schiz[[cols_sub$col[i]]], na.rm = TRUE))
                     column(6, sliderInput(inputId = cols_sub$col_clean[i],
                                           label = HTML(paste0(cols_sub$col[i],
                                                               " <input type = 'checkbox' checked id = ",
                                                               "'", cols_sub$col_clean[i], "-checkbox' ",
                                                               "aria-label = 'show/hide this column'",
                                                               "class = '",
                                                               ifelse((i %% 2) == 1, "hint--bottom-right", "hint--bottom-left"),
                                                               " hint--rounded'>")),
                                           min = min_val,
                                           max = max_val,
                                           value = c(min_val, max_val),
                                           step = .01))
                   } else {
                     column(6, selectInput(inputId = cols_sub$col_clean[i],
                                           label = HTML(paste0(cols_sub$col[i],
                                                               " <input type = 'checkbox' checked id = ",
                                                               "'", cols_sub$col_clean[i], "-checkbox' ",
                                                               "aria-label = 'show/hide this column'",
                                                               "class = '",
                                                               ifelse((i %% 2) == 1, "hint--bottom-right", "hint--bottom-left"),
                                                               " hint--rounded'>")),
                                           choices = sort(unique(as.character(shiny_schiz[[cols_sub$col[i]]]))),
                                           multiple = TRUE))
                   }
                 })))
          })
        ),
        tabPanel(
          "Ecology and Locality",
          lapply(unique(cols$cat[cols$tab == 5]), function(cat_name) {
            cols_sub <- cols %>% filter(cat == cat_name)
            list(fluidRow(column(12, h4(cat_name, id = idEscape(cat_name)))),
                 fluidRow(lapply(seq_len(nrow(cols_sub)), function(i) {
                   column(6,
                          selectInput(inputId = cols_sub$col_clean[i],
                                      label = HTML(paste0(cols_sub$col[i],
                                                          " <input type = 'checkbox' checked id = ",
                                                          "'", cols_sub$col_clean[i], "-checkbox' ",
                                                          "aria-label = 'show/hide this column'",
                                                          "class = '",
                                                          ifelse((i %% 2) == 1, "hint--bottom-right", "hint--bottom-left"),
                                                          " hint--rounded'>")),
                                      choices = sort(unique(as.character(shiny_schiz[[cols_sub$col[i]]]))),
                                      multiple = TRUE))
                 })))
          })
        )
      )
    ),
    fluidRow(add_prompt(actionButton("reset_input", HTML(paste(fa("rotate", prefer_type = "solid"), "Reset all filters"))),
                        message = "Reset the table to its original form",
                        position = "right", type = "warning", rounded = TRUE)),
    id = "side-panel"
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Database",
          fluidRow(column(12, h4(""))),
          fluidRow(DT::dataTableOutput('table1'))
        ),
        tabPanel(
          "Species Description History",
          fluidRow(column(12, h4(""))),
          fluidRow(DT::dataTableOutput('table2'))
        ),
        tabPanel(
          "Full References",
          fluidRow(column(12, h4(""))),
          div(references_html, style = "overflow-y: scroll; height: calc(90vh - 120px); height: calc(90dvh - 120px);")
        )
      )
    ))
  )
}

# Run shiny ----
shinyApp(ui = ui, server = server)



