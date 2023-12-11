# The Schizomid Trait Database - ShinyApp
# William Gearty and Sandro Pascal Müller
# 2023-09-07

# relevant packages ----
library(shiny)
library(shinyjs)
library(dplyr)
#webr::install("openxlsx") # excel files don't work
#library(openxlsx)
library(DT)
library(janitor)
library(htmltools)
library(fontawesome)
library(prompter)
library(bslib)

# copied from Hmisc
`%nin%` <- function(x, table) match(x, table, nomatch = 0) == 0

all.is.numeric <- function(x, what = c("test", "vector", "nonnum"), extras = c(".", "NA")) {
  what <- match.arg(what)
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  xs <- x[x %nin% c("", extras)]
  if (!length(xs) || all(is.na(x))) 
    return(switch(what, test = FALSE, vector = x, nonnum = x[0]))
  isnon <- suppressWarnings(!is.na(xs) & is.na(as.numeric(xs)))
  isnum <- !any(isnon)
  switch(what, test = isnum, vector = if (isnum) suppressWarnings(as.numeric(x)) else x, 
         nonnum = xs[isnon])
}

# load data ----
shiny_schiz_orig <- read.csv("data/STDB.csv", check.names = FALSE)

taxonomy_syn <- read.csv("data/description_history.csv", check.names = FALSE)
colnames(taxonomy_syn) <- gsub(".", " ", colnames(taxonomy_syn), fixed = TRUE)

# convert docx to filtered html, then convert to UTF-8
references_html <- includeHTML("data/STDB_references.htm")
about_html <- includeHTML("data/STDB_about.htm")

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
  mutate(col_clean = idEscape(col)) %>% # replace special characters with dashes
  mutate(filt = gsub("(\\s\\(min\\)|\\s\\(max\\))", "", col)) %>%
  mutate(filt_clean = idEscape(filt)) %>%
  mutate(dupe = duplicated(filt))

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
      lapply(cols$cat[double_row[double_row != ref_col]], function(cat) {
        args <- list(cat, rowspan = 2)
        if(cat == "Sex") {
          args$id <- paste0(cat, "-th")
          args$style <- "cursor: pointer;"
        }
        do.call(th, args)
      }),
      lapply(seq_along(col_width), function(i) {
        th(names(col_width)[i], colspan = col_width[i], style = "text-align: center;")
      }),
      th(cols$cat[ref_col], rowspan = 2)
    ),
    tr(
      lapply((1:nrow(cols))[-double_row], function(i) {
        th(cols$col[i], id = paste0(cols$col_clean[i], "-th"), style = "cursor: pointer;")
      })
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
  proxy1 <- dataTableProxy('table1')
  proxy2 <- dataTableProxy('table2')
  
  # observers ----
  ## modal popups ----
  ### row selection ----
  # modal popup when selecting a row
  observeEvent(input$table1_rows_selected, {
    data <- shiny_schiz
    for (i in seq_len(nrow(cols))) {
      input_value <- input[[cols$filt_clean[i]]]
      if (!is.null(input_value)) {
        if (is.numeric(shiny_schiz[[cols$col[i]]])) {
          min_val <- min(shiny_schiz[[cols$col[i]]], na.rm = TRUE)
          max_val <- max(shiny_schiz[[cols$col[i]]], na.rm = TRUE)
          if (input_value[1] > min_val | input_value[2] < max_val) {
            if (input[[paste0(cols$filt_clean[i], "-NAs")]]) {
              data <- data %>%
                filter((!!as.symbol(cols$col[i]) >= input_value[1] & 
                          !!as.symbol(cols$col[i]) <= input_value[2]) |
                         is.na(!!as.symbol(cols$col[i])))
            } else {
              data <- data %>%
                filter(!!as.symbol(cols$col[i]) >= input_value[1],
                       !!as.symbol(cols$col[i]) <= input_value[2])
            }
          }
        } else {
          data <- data %>%
            filter(!!as.symbol(cols$col[i]) %in% input_value)
        }
      }
    }
    modal <- modalDialog(
      tags$table(
        lapply(colnames(data), function(name) {
          tags$tr(tags$th(name), tags$td(data[[name]][input$table1_rows_selected]))
        })
      ),
      title = paste0(data$Species[input$table1_rows_selected], " (",
                     data$Sex[input$table1_rows_selected], ")"),
      size = "l",
      easyClose = TRUE
    )
    # insert dismiss button in the header
    modal$children[[1]]$children[[1]]$children[[1]]$children[[2]] <- modalButton("Dismiss")
    showModal(modal)
  })
  
  ### header selection ----
  # modal popup when clicking on a column header
  lapply(c(5, (1:nrow(cols))[-double_row]), function(i) { # column 5 is "Sex"
    onclick(paste0(cols$col_clean[i], "-th"),
            showModal(modalDialog(title = cols$filt[i],
                                  img(src = paste0("/Schizomida/drawings_database/",
                                                   cols$filt_clean[i], ".png"),
                                      alt = cols$filt[i], width = "100%"),
                                  size = "l", easyClose = TRUE)))
  })
  
  ## show/hide columns ----
  ### table 1 ----
  # update all table1 columns based on checkboxes
  checkbox1_rows <- which(cols$tab %in% 1:5 & !cols$dupe)
  update_cols1 <- function() {
    # hide columns in table1
    show_cols <- c()
    hide_cols <- c()
    for (i in checkbox1_rows) {
      if (input[[paste0(cols$filt_clean[i], "-checkbox1")]]) {
        show_cols <- c(show_cols, cols$filt[i])
      } else {
        hide_cols <- c(hide_cols, cols$filt[i])
      }
    }
    showCols(proxy1,
             unname(sapply(show_cols,
                           function(x) which(x == colnames(shiny_schiz)) - 1)))
    hideCols(proxy1,
             unname(sapply(hide_cols,
                           function(x) which(x == colnames(shiny_schiz)) - 1)))
  }
  
  # monitor checkboxes for table1
  lapply(checkbox1_rows, function(i) {
    observeEvent(
      input[[paste0(cols$filt_clean[i], "-checkbox1")]],
      {
        if (input[[paste0(cols$filt_clean[i], "-checkbox1")]]) {
          showCols(proxy1, grep(cols$filt[i], colnames(shiny_schiz)) - 1)
        } else {
          hideCols(proxy1, grep(cols$filt[i], colnames(shiny_schiz)) - 1)
        }},
      ignoreInit = TRUE
    )
  })
  
  ### table 2 ----
  # update all table2 columns based on checkboxes
  checkbox2_rows <- which(cols$col %in% c("Family", "Subfamily", "Genus", "Species"))
  update_cols2 <- function() {
    # hide columns in table2
    show_cols <- c()
    hide_cols <- c()
    for (i in checkbox2_rows) {
      if (input[[paste0(cols$filt_clean[i], "-checkbox2")]]) {
        show_cols <- c(show_cols, cols$filt[i])
      } else {
        hide_cols <- c(hide_cols, cols$filt[i])
      }
    }
    showCols(proxy2,
             unname(sapply(show_cols,
                           function(x) which(x == colnames(shiny_schiz)) - 1)))
    hideCols(proxy2,
             unname(sapply(hide_cols,
                           function(x) which(x == colnames(shiny_schiz)) - 1)))
  }
  
  # monitor checkboxes for table2
  lapply(checkbox2_rows, function(i) {
    observeEvent(input[[paste0(cols$filt_clean[i], "-checkbox2")]],
                 if (input[[paste0(cols$filt_clean[i], "-checkbox2")]]) {
                   showCols(proxy2, which(cols$filt[i] == colnames(shiny_schiz)) - 1)
                 } else {
                   hideCols(proxy2, which(cols$filt[i] == colnames(shiny_schiz)) - 1)
                 },
                 ignoreInit = TRUE
    )
  })
  
  ## numeric filters ----
  # monitor numeric filters and show/hide NA checkboxes when not default
  observeEvent(
    lapply(
      cols$filt_clean[num_cols & !cols$dupe],
      function(name) {
        input[[name]]
      }
    ),
    for(i in which(num_cols & !cols$dupe)) {
      input_value <- input[[cols$filt_clean[i]]]
      ids <- paste0(cols$filt_clean[i], c("-NAs", "-NAs-label"))
      min_val <- min(shiny_schiz[[cols$col[i]]], na.rm = TRUE)
      max_val <- max(shiny_schiz[[cols$col[i]]], na.rm = TRUE)
      if (input_value[1] > min_val | input_value[2] < max_val) {
        sapply(ids, function(id) runjs(paste0("$('#", id, "').show()")))
      } else {
        sapply(ids, function(id) runjs(paste0("$('#", id, "').hide().prop('checked', false).trigger('change')")))
      }
    },
    ignoreInit = TRUE
  )
  
  ## reset buttons ----
  # reset all filters if button is pressed
  observeEvent(input$reset_input1, {
    reset("side-panel1")
    runjs("$('input[id$=checkbox1]').prop('checked', true).trigger('change')")
  })
  
  observeEvent(input$reset_input2, {
    reset("side-panel2")
    runjs("$('input[id$=checkbox2]').prop('checked', true).trigger('change')")
  })
  
  ## export buttons ----
  ### excel ----
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
  
  ### csv ----
  # trigger csv file download
  observeEvent(input$downloadCSV, {
    runjs(js_export("csv"))
  })
  
  ### empty excel ----
  # trigger empty excel file download
  observeEvent(input$downloadEmptyExcel, {
    runjs(js_export(empty = TRUE))
  })
  
  ## taxonomy filters ----
  ### table 1 ----
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
  
  # update sex and character filters if family/subfamily/genus/species is changed
  observeEvent(list(input$Family, input$Subfamily, input$Genus, input$Species), {
    dat <- shiny_schiz
    if (!is.null(input$Family)) dat <- dat %>% filter(Family %in% input$Family)
    if (!is.null(input$Subfamily)) dat <- dat %>% filter(Subfamily %in% input$Subfamily)
    if (!is.null(input$Genus)) dat <- dat %>% filter(Genus %in% input$Genus)
    if (!is.null(input$Species)) dat <- dat %>% filter(Species %in% input$Species)
    choices <- sort(unique(as.character(dat$Sex)))
    updateSelectizeInput(inputId = "Sex", choices = choices, server = TRUE)
    
    # update other character filters
    for (i in which(cols$tab %in% 2:6 & !cols$dupe & sapply(shiny_schiz, Negate(is.numeric)))) {
      if (!is.numeric(dat[[cols$col[i]]])) {
        choices <- sort(unique(as.character(dat[[cols$filt[i]]])))
        updateSelectizeInput(inputId = cols$filt_clean[i], choices = choices,
                             selected = input[[cols$filt_clean[i]]], server = TRUE)
      }
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  ### table 2 ----
  observeEvent(list(input$Family2), {
    dat <- taxonomy_syn
    if (!is.null(input$Family2)) dat <- dat %>% filter(Family %in% input$Family2)
    choices <- sort(unique(as.character(taxonomy_syn$Subfamily)))
    updateSelectizeInput(inputId = "Subfamily2", choices = choices, server = TRUE)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # update genus if family/subfamily is changed
  observeEvent(list(input$Family2, input$Subfamily2), {
    dat <- taxonomy_syn
    if (!is.null(input$Family2)) dat <- dat %>% filter(Family %in% input$Family2)
    if (!is.null(input$Subfamily2)) dat <- dat %>% filter(Subfamily %in% input$Subfamily2)
    choices <- sort(unique(as.character(dat$Genus)))
    updateSelectizeInput(inputId = "Genus2", choices = choices, server = TRUE)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # update species if family/subfamily/genus is changed
  observeEvent(list(input$Family2, input$Subfamily2, input$Genus2), {
    dat <- taxonomy_syn
    if (!is.null(input$Family2)) dat <- dat %>% filter(Family %in% input$Family2)
    if (!is.null(input$Subfamily2)) dat <- dat %>% filter(Subfamily %in% input$Subfamily2)
    if (!is.null(input$Genus2)) dat <- dat %>% filter(Genus %in% input$Genus2)
    choices <- sort(unique(as.character(dat$Species)))
    updateSelectizeInput(inputId = "Species2", choices = choices, server = TRUE)
  }, ignoreNULL = FALSE)
  
  ## show/hide filters ----
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
  
  # datatables ----
  ## data table ----
  ### setup reactive data ----
  df <- eventReactive(
    do.call(c, lapply(
      cols$filt_clean,
      function(name) {
        list(input[[name]], input[[paste0(name, "-NAs")]])
      }
    )),
    {
      data <- shiny_schiz
      for (i in seq_len(nrow(cols))) {
        input_value <- input[[cols$filt_clean[i]]]
        if (!is.null(input_value)) {
          if (is.numeric(shiny_schiz[[cols$col[i]]])) {
            min_val <- min(shiny_schiz[[cols$col[i]]], na.rm = TRUE)
            max_val <- max(shiny_schiz[[cols$col[i]]], na.rm = TRUE)
            if (input_value[1] > min_val | input_value[2] < max_val) {
              if (input[[paste0(cols$filt_clean[i], "-NAs")]]) {
                data <- data %>%
                  filter((!!as.symbol(cols$col[i]) >= input_value[1] & 
                            !!as.symbol(cols$col[i]) <= input_value[2]) |
                           is.na(!!as.symbol(cols$col[i])))
              } else {
                data <- data %>%
                  filter(!!as.symbol(cols$col[i]) >= input_value[1],
                         !!as.symbol(cols$col[i]) <= input_value[2])
              }
            }
          } else {
            data <- data %>%
              filter(!!as.symbol(cols$col[i]) %in% input_value)
          }
        }
      }
      data
    }
  )
  
  ### render table ----
  # lots of options available: https://datatables.net/reference/option/
  output$table1 <- DT::renderDataTable(DT::datatable(
    {
      data <- df()
      # update unique species count
      runjs(paste0("$('#species_count').html('(", length(unique(data$Species)), " unique species)')"))
      data
    },
    container = tab_layout,
    rownames = FALSE,
    elementId = 'table1',
    style = 'bootstrap',
    class = 'table-bordered stripe',
    selection = list(mode = "single", selected = NULL, target = "row", selectable = TRUE),
    extensions = c("Buttons", "FixedColumns"),
    options = list(
      autoWidth = TRUE,
      ordering = TRUE,
      columnDefs = list(list(width = '200px', targets = "_all"),
                        list(orderable = FALSE, targets = 4:(ncol(shiny_schiz) - 1))),
      scrollX = TRUE,
      scrollY = TRUE,
      scrollCollapse = TRUE,
      paging = FALSE,
      dom = 'Bfrti',
      buttons = list(
        list(extend = "copy", text = paste(fa("clipboard"),  "Copy"),
             exportOptions = list(columns = ":visible"),
             className = "hint--bottom-right hint--rounded hint--info",
             attr = list("aria-label" = "Copy the below table to the clipboard",
                         "style" = "z-index: 10;")),
        list(extend = "collection", text = paste(fa("download", prefer_type = "solid"), "CSV"),
             action = JS("function ( e, dt, node, config ) {
                                 Shiny.setInputValue('downloadCSV', true, {priority: 'event'});
                               }"),
             className = "hint--bottom-right hint--rounded hint--info",
             attr = list("aria-label" = "Download a copy of the below table in CSV format",
                         "style" = "z-index: 10;")
        ),
        list(extend = "collection", text = paste(fa("download", prefer_type = "solid"), "Excel"),
             action = JS("function ( e, dt, node, config ) {
                                 Shiny.setInputValue('downloadExcel', true, {priority: 'event'});
                               }"),
             className = "hint--bottom-right hint--rounded hint--info",
             attr = list("aria-label" = "Download a copy of the below table in Excel format",
                         "style" = "z-index: 10;")
        ),
        list(extend = "collection", text = paste(fa("download", prefer_type = "solid"), "Empty Excel"),
             action = JS("function ( e, dt, node, config ) {
                                 Shiny.setInputValue('downloadEmptyExcel', true, {priority: 'event'});
                               }"),
             className = "hint--bottom-right hint--rounded hint--info",
             attr = list("aria-label" = "Download an empty copy of the below table in Excel format (only colomn headers)",
                         "style" = "z-index: 10;")
        ),
        list(extend = "fixedColumns", text = "Freeze First Column",
             className = "hint--bottom-right hint--rounded hint--info",
             attr = list("aria-label" = "Freeze the first currently visible column",
                         "style" = "z-index: 10;", "id" = "fixedButton")
        )
      ),
      rowCallback = JS(rowCallback), # formatting NAs
      infoCallback = JS(c(
        "function( settings, start, end, max, total, pre ) {",
        "  return 'Showing ' + total + ' entries';",
        "}"
      )), # for formatting the info below the table
      drawCallback = JS(c("if($('#fixedButton').hasClass('active')) {",
                          "  document.body.classList.add('fixedActive');",
                          "} else {",
                          "  document.body.classList.remove('fixedActive');",
                          "}")),
      initComplete = JS("function(settings, json) {
                           if(!$('body').hasClass('fixedActive')) $('#fixedButton').click();
                         }")
    )) %>%
      formatStyle('Genus', fontStyle = "italic") %>%
      formatStyle('Species', fontStyle = "italic"))
  
  ### update columns ----
  # update columns based on checkboxes and filters
  observeEvent(
    df(),
    {
      update_cols1()
      # update male/female columns based on sex filter
      if (!is.null(input$Sex)) {
        if (! "male" %in% input$Sex) {
          hideCols(proxy1,
                   unlist(unname(sapply(male_names,
                                        function(x) which(x == colnames(shiny_schiz)) - 1))))
        }
        if (! "female" %in% input$Sex) {
          hideCols(proxy1,
                   unlist(unname(sapply(female_names,
                                        function(x) which(x == colnames(shiny_schiz)) - 1))))
        }
      }
    })
  
  ## tax. and syn. table ----
  ### setup reactive data ----
  df2 <- eventReactive(
    lapply(
      c("Family", "Subfamily", "Genus", "Species"),
      function(name) {
        input[[paste0(name, "2")]]
      }
    ),
    {
      data2 <- taxonomy_syn
      for (col in c("Family", "Subfamily", "Genus", "Species")) {
        input_value <- input[[paste0(col, "2")]]
        if (!is.null(input_value)) {
          data2 <- data2 %>%
            filter(grepl(input_value, !!as.symbol(col), fixed = TRUE))
        }
      }
      data2
    })
  
  ### render table ----
  # lots of options available: https://datatables.net/reference/option/
  output$table2 <- DT::renderDataTable(DT::datatable(
    df2(),
    elementId = 'table2',
    rownames = FALSE,
    style = 'bootstrap',
    class = 'table-bordered stripe',
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
      rowCallback = JS(rowCallback), # formatting NAs
      infoCallback = JS(c(
        "function( settings, start, end, max, total, pre ) {",
        "  return 'Showing ' + total + ' species';",
        "}"
      )) # for formatting the info below the table
    )))
  ### update columns ----
  # update columns based on checkboxes
  observeEvent(df2(), update_cols2())
}

# ui.R ----
ui <- {
  fluidPage(
    theme = bs_theme(version = 5, preset="bootstrap"),
    useShinyjs(),
    use_prompt(),
    ## head ----
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
         #table1, #table2, .dataTables_wrapper, .main {
           padding-right: 0px !important;
         }
         h5 {
           font-style: italic;
         }
         .dataTables_scrollBody {
           max-height: 60vh;
           max-height: 60dvh;
         }
         .tableexport-caption {
           display: none;
         }
         .sidebar-content .card-body {
           height: 80vh !important;
           height: 80dvh !important;
           overflow-y: auto !important;
         }
         #side-panel {
           overflow-x: hidden !important;
         }
         .darkmode--activated .table-striped tr.odd>* {
           background-color: #e0e0e0 !important;
         }
         .darkmode-layer, .darkmode-toggle {
           z-index: 500;
         }
         .darkmode--activated .modal-content {
           background-color: black !important;
           color: white !important;
           border: solid white !important;
         }
         .sidebar-content {
           padding: 0 !important;
         }
         .accordion-item:nth-of-type(1) .accordion-header,
         .accordion-item:nth-of-type(1) .accordion-button {
           background-color: #aaffb1;
         }
         .accordion-item:nth-of-type(2) .accordion-header,
         .accordion-item:nth-of-type(2) .accordion-button {
           background-color: #ffd0d0;
         }
         .accordion-item:nth-of-type(3) .accordion-header,
         .accordion-item:nth-of-type(3) .accordion-button {
           background-color: #ffdfa5;
         }
         .accordion-item:nth-of-type(4) .accordion-header,
         .accordion-item:nth-of-type(4) .accordion-button {
           background-color: #ffc0ff;
         }
         .accordion-item:nth-of-type(5) .accordion-header,
         .accordion-item:nth-of-type(5) .accordion-button {
           background-color: #ccccff;
         }
         .accordion-title {
           font-size: large !important;
         }
         .nav li a {
           margin-right: 10px !important;
           font-size: large !important;
         }
         "
        ),
      ),
      tags$script(src = "xlsx.core.min.js"),
      tags$script(src = "FileSaver.min.js"),
      tags$script(src = "tableexport.min.js"),
      tags$script(src="https://cdn.jsdelivr.net/npm/darkmode-js@1.5.7/lib/darkmode-js.min.js")
    ),
    tags$script(HTML("function addDarkmodeWidget() {
        new Darkmode({label: '🌓', left: '32px', right: 'unset'}).showWidget();
      }
      window.addEventListener('load', addDarkmodeWidget);
      $(function() {
        $('.navbar-collapse').append($(\"<a href='https://github.com/willgearty/Schizomida/issues' target='_blank' class='btn btn-primary'>Contribute</a>\"));
      });"
    )),
    ## title ----
    titlePanel('Schizomida Trait Data Base (STDB)'),
    ## panels ----
    page_navbar(bg = "#f7f6f4", gap = "15px",
                ### database ----
                nav_panel("Database", layout_sidebar(
                  fluidRow(DT::dataTableOutput('table1'),
                           div(
                             div(paste0("(", length(unique(shiny_schiz$Species)), " unique species)"),
                                 id = "species_count", style = "float: left;"),
                             div(HTML("Click a character column header for a figure of the character<br />Click a row for a species summary"),
                                 style = "float: right; margin-top: -24px; text-align: right;")
                           ), style = "width: 100%;"),
                  #### filters ----
                  sidebar = sidebar(width = "35%",
                                    card(fluidRow(h4("Filters"),
                                                  accordion(multiple = FALSE,
                                                            accordion_panel(
                                                              "Taxonomy and Sex",
                                                              fluidRow(lapply(seq_len(cols %>% filter(tab == 1) %>% nrow()), function(i) {
                                                                column(4,
                                                                       selectInput(inputId = cols$col_clean[i],
                                                                                   label = HTML(paste0(cols$col[i],
                                                                                                       " <input type = 'checkbox' checked id = ",
                                                                                                       "'", cols$col_clean[i], "-checkbox1' ",
                                                                                                       "aria-label = 'show/hide this column'",
                                                                                                       "class = '",
                                                                                                       ifelse((i %% 3) == 0, "hint--bottom-left", "hint--bottom-right"),
                                                                                                       " hint--rounded'>")),
                                                                                   choices = sort(unique(as.character(shiny_schiz[[cols$col[i]]]))),
                                                                                   multiple = TRUE))
                                                              }))
                                                            ),
                                                            accordion_panel(
                                                              "Prosoma",
                                                              lapply(unique(cols$cat[cols$tab == 2]), function(cat_name) {
                                                                cols_sub <- cols %>% filter(cat == cat_name, !dupe)
                                                                list(fluidRow(column(12, h5(cat_name, id = idEscape(cat_name)))),
                                                                     fluidRow(lapply(seq_len(nrow(cols_sub)), function(i) {
                                                                       if (is.numeric(shiny_schiz[[cols_sub$col[i]]])) {
                                                                         min_val <- min(shiny_schiz[[cols_sub$col[i]]], na.rm = TRUE)
                                                                         max_val <- max(shiny_schiz[[cols_sub$col[i]]], na.rm = TRUE)
                                                                         column(6, sliderInput(inputId = cols_sub$filt_clean[i],
                                                                                               label = HTML(paste0(cols_sub$filt[i],
                                                                                                                   " <input type = 'checkbox' checked id = ",
                                                                                                                   "'", cols_sub$filt_clean[i], "-checkbox1' ",
                                                                                                                   "aria-label = 'show/hide this column'",
                                                                                                                   "class = '",
                                                                                                                   ifelse((i %% 2) == 1, "hint--bottom-right", "hint--bottom-left"),
                                                                                                                   " hint--rounded'>",
                                                                                                                   "<br><label id='", cols_sub$filt_clean[i], "-NAs-label' ",
                                                                                                                   "for='", cols_sub$filt_clean[i], "-NAs' ",
                                                                                                                   "style='font-size: small; display: none;'>",
                                                                                                                   "Include NAs</label> ",
                                                                                                                   "<input style='display: none;' type = 'checkbox' id = '",
                                                                                                                   cols_sub$filt_clean[i], "-NAs' ",
                                                                                                                   "aria-label = 'include NA values in this column'",
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
                                                                                                                   "'", cols_sub$col_clean[i], "-checkbox1' ",
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
                                                            accordion_panel(
                                                              "Opisthosoma",
                                                              lapply(unique(cols$cat[cols$tab == 3]), function(cat_name) {
                                                                cols_sub <- cols %>% filter(cat == cat_name, !dupe)
                                                                list(fluidRow(column(12, h5(cat_name, id = idEscape(cat_name)))),
                                                                     fluidRow(lapply(seq_len(nrow(cols_sub)), function(i) {
                                                                       if (is.numeric(shiny_schiz[[cols_sub$col[i]]])) {
                                                                         min_val <- min(shiny_schiz[[cols_sub$col[i]]], na.rm = TRUE)
                                                                         max_val <- max(shiny_schiz[[cols_sub$col[i]]], na.rm = TRUE)
                                                                         column(6, sliderInput(inputId = cols_sub$filt_clean[i],
                                                                                               label = HTML(paste0(cols_sub$filt[i],
                                                                                                                   " <input type = 'checkbox' checked id = ",
                                                                                                                   "'", cols_sub$filt_clean[i], "-checkbox1' ",
                                                                                                                   "aria-label = 'show/hide this column'",
                                                                                                                   "class = '",
                                                                                                                   ifelse((i %% 2) == 1, "hint--bottom-right", "hint--bottom-left"),
                                                                                                                   " hint--rounded'>",
                                                                                                                   "<br><label id='", cols_sub$filt_clean[i], "-NAs-label' ",
                                                                                                                   "for='", cols_sub$filt_clean[i], "-NAs' ",
                                                                                                                   "style='font-size: small; display: none;'>",
                                                                                                                   "Include NAs</label> ",
                                                                                                                   "<input style='display: none;' type = 'checkbox' id = '",
                                                                                                                   cols_sub$filt_clean[i], "-NAs' ",
                                                                                                                   "aria-label = 'include NA values in this column'",
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
                                                                                                                   "'", cols_sub$col_clean[i], "-checkbox1' ",
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
                                                            accordion_panel(
                                                              "Legs and Size",
                                                              lapply(unique(cols$cat[cols$tab == 4]), function(cat_name) {
                                                                cols_sub <- cols %>% filter(cat == cat_name, !dupe)
                                                                list(fluidRow(column(12, h5(cat_name, id = idEscape(cat_name)))),
                                                                     fluidRow(lapply(seq_len(nrow(cols_sub)), function(i) {
                                                                       if (is.numeric(shiny_schiz[[cols_sub$col[i]]])) {
                                                                         min_val <- min(shiny_schiz[[cols_sub$col[i]]], na.rm = TRUE)
                                                                         max_val <- max(shiny_schiz[[cols_sub$col[i]]], na.rm = TRUE)
                                                                         column(6, sliderInput(inputId = cols_sub$filt_clean[i],
                                                                                               label = HTML(paste0(cols_sub$filt[i],
                                                                                                                   " <input type = 'checkbox' checked id = ",
                                                                                                                   "'", cols_sub$filt_clean[i], "-checkbox1' ",
                                                                                                                   "aria-label = 'show/hide this column'",
                                                                                                                   "class = '",
                                                                                                                   ifelse((i %% 2) == 1, "hint--bottom-right", "hint--bottom-left"),
                                                                                                                   " hint--rounded'>",
                                                                                                                   "<br><label id='", cols_sub$filt_clean[i], "-NAs-label' ",
                                                                                                                   "for='", cols_sub$filt_clean[i], "-NAs' ",
                                                                                                                   "style='font-size: small; display: none;'>",
                                                                                                                   "Include NAs</label> ",
                                                                                                                   "<input style='display: none;' type = 'checkbox' id = '",
                                                                                                                   cols_sub$filt_clean[i], "-NAs' ",
                                                                                                                   "aria-label = 'include NA values in this column'",
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
                                                                                                                   "'", cols_sub$col_clean[i], "-checkbox1' ",
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
                                                            accordion_panel(
                                                              "Ecology and Locality",
                                                              lapply(unique(cols$cat[cols$tab == 5]), function(cat_name) {
                                                                cols_sub <- cols %>% filter(cat == cat_name)
                                                                list(fluidRow(column(12, h5(cat_name, id = idEscape(cat_name)))),
                                                                     fluidRow(lapply(seq_len(nrow(cols_sub)), function(i) {
                                                                       column(6,
                                                                              selectInput(inputId = cols_sub$col_clean[i],
                                                                                          label = HTML(paste0(cols_sub$col[i],
                                                                                                              " <input type = 'checkbox' checked id = ",
                                                                                                              "'", cols_sub$col_clean[i], "-checkbox1' ",
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
                                    )),
                                    #### reset button ----
                                    div(style="text-align: center; padding-bottom: var(--bslib-sidebar-padding);",
                                        add_prompt(actionButton("reset_input1", HTML(paste(fa("rotate", prefer_type = "solid"), "Reset all filters")), width = "50%"),
                                                   message = "Reset the table to its original form",
                                                   position = "top", rounded = TRUE)),
                                    id = "side-panel1"
                  )
                )),
                ### species hist. ----
                nav_panel("Species Description History", layout_sidebar(
                  fluidRow(DT::dataTableOutput('table2'), style = "width: 100%;"),
                  #### filters ----
                  sidebar = sidebar(width = "35%",
                                    card(fluidRow(h2("Filters"),
                                                  accordion(multiple = FALSE,
                                                            accordion_panel(
                                                              "Taxonomy",
                                                              fluidRow(lapply(seq_len(cols %>% filter(tab == 1, cat != "Sex") %>% nrow()), function(i) {
                                                                column(4,
                                                                       selectInput(inputId = paste0(cols$col_clean[i], "2"),
                                                                                   label = HTML(paste0(cols$col[i],
                                                                                                       " <input type = 'checkbox' checked id = ",
                                                                                                       "'", cols$col_clean[i], "-checkbox2' ",
                                                                                                       "aria-label = 'show/hide this column'",
                                                                                                       "class = '",
                                                                                                       ifelse((i %% 3) == 0, "hint--bottom-left", "hint--bottom-right"),
                                                                                                       " hint--rounded'>")),
                                                                                   choices = sort(unique(as.character(taxonomy_syn[[cols$col[i]]]))),
                                                                                   multiple = TRUE))
                                                              }))
                                                            )
                                                  )
                                    )),
                                    #### reset button ----
                                    div(style="text-align: center; padding-bottom: var(--bslib-sidebar-padding);",
                                        add_prompt(actionButton("reset_input2", HTML(paste(fa("rotate", prefer_type = "solid"), "Reset all filters")), width = "50%"),
                                                   message = "Reset the table to its original form",
                                                   position = "top", rounded = TRUE)),
                                    id = "side-panel2"
                  )
                )),
                ### general anatomy ----
                nav_panel(
                  "Schizomid General Anatomy",
                  div(style = "overflow-y: scroll; height: calc(90vh - 120px); height: calc(90dvh - 120px);")
                ),
                ### references ----
                nav_panel(
                  "Full References",
                  div(references_html, style = "overflow-y: scroll; height: calc(90vh - 120px); height: calc(90dvh - 120px);")
                ),
                ### about ----
                nav_panel(
                  "About and Contact",
                  div(about_html, style = "overflow-y: scroll; height: calc(90vh - 120px); height: calc(90dvh - 120px);")
                )
    )
  )
}

# Run shiny ----
shinyApp(ui = ui, server = server)



