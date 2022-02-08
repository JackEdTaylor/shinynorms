library(shiny)
library(shinyjs)
library(shinycssloaders)

library(ordinal)

library(readr)
library(dplyr)
library(openxlsx)

library(ggplot2)
theme_set(theme_bw(base_size = 20))

# Fitting models can sometimes make use of multiple cores by default because of the BLAS routines the method calls. Depending on setup, this could lead to the app demanding high CPU usage for each running session. Uncomment the code below to limit the number of cores available to BLAS.

# library(RhpcBLASctl)
# blas_set_num_threads(1)

ui <- navbarPage(
  
  "CLMM Norms",
  id = "main_navbar",
  
  tabPanel(
    "Introduction",
    useShinyjs(),
    withMathJax(),
    fluidRow(column(
      10, offset=1,
      includeHTML("introduction.html")
    )),
    br(), br(), br(),
    column(12, actionButton("begin", "Get Started", style = "font-size:135%", icon=icon("arrow-circle-right")), align="center"),
    br(), br(), br(), br(),
    column(12, downloadButton("download_eg_norms", "Download Example Dataset", style = "font-size:135%"), align="center"),
    icon = icon("info")
  ),
  
  tabPanel(
    "1) Import Trials",
    fluidRow(
      column(
        3,
        h3("Import Trials"),
        HTML("<h4>The first step is to import the trial-level data. This should be in <a href=\"https://en.wikipedia.org/wiki/Wide_and_narrow_data\">\"long\" or \"narrow\" format</a>, with one trial per row. If you presented multiple items to multiple participants, there should be at least three columns: <i>participant ID</i>, <i>item ID</i>, and <i>rating</i>. Your data can be in any of the following formats: <i>.csv</i>, <i>.tsv</i>, <i>.xlsx</i>, <i>.xls</i>. If an Excel document, the trials should be in the first sheet of the workbook.</h4>"),
        br(),
        br()
      ),
      column(
        8, offset=1,
        br(),
        br(),
        fileInput("trials_file", "Import Trial-Level Data", accept = c(".csv", ".tsv", ".xlsx", ".xls"))
      )
    ),
    br(),
    hr(),
    br(),
    fluidRow(
      column(
        3,
        h3("Check Trials"),
        HTML("<h4>This table shows the imported trials. Check they have been imported correctly.</h4>"),
        br(),
        br()
      ),
      column(
        8, offset=1,
        dataTableOutput("raw_trials_table")
      )
    )
  ),
  
  tabPanel(
    "2) Identify Columns",
    fluidRow(
      div(
        id = "previous_section_warning_2",
        column(12, h4("Warning: check you have completed the previous sections!", style="color:red;")),
        br()
      ),
      column(
        3,
        h3("Identify Columns"),
        HTML("<h4>The second step is to identify which columns refer to participants, items, and ratings. If you only have one participant, or only one item, you can tick the checkboxes to reflect this.</h4>"),
        br(),
        br()
      ),
      column(
        8, offset=1,
        div(
          br(),
          br(),
          uiOutput("column_id_ui"),
          fluidRow(
            column(4, checkboxInput("one_participant", "My data has just one participant", width="80%")),
            column(4, checkboxInput("one_item", "My data has just one item", width="80%"))
          ),
          style = "align:center;"
        )
      )
    ),
    br(),
    hr(),
    br(),
    br(),
    fluidRow(
      column(
        3,
        h4("Check Summaries"),
        HTML("Check these summary statistics match what you'd expect."),
        br(),
        br()
      ),
      column(
        8, offset=1,
        uiOutput("setup_summ_stats")
      )
    )
  ),
  
  tabPanel(
    "3) Fit Model",
    fluidRow(
      div(
        id = "previous_section_warning_3",
        column(12, h4("Warning: check you have completed the previous sections!", style="color:red;")),
        br()
      ),
      column(
        3,
        h3("Fit Model"),
        HTML("<h4>Now we can fit the CLMM to get the norms via <i>ordinal::clmm()</i>. Select the desired link function and click <i>Fit Model</i>. The density plot shows the latent distribution associated with the selected link function.</h4>"),
        br(),
        br()
      ),
      column(
        8, offset=1,
        br(),
        br(),
        fluidRow(column(6, selectInput("link", "Link Function (Latent Distribution)", c("probit (Normal)"="probit", "logit (Logistic)"="logit", "cloglog (Complementary Log-Log)"="cloglog", "loglog (Log-Log)"="loglog", "cauchit (Cauchy)"="cauchit"), width="90%"), align = "center")),
        fluidRow(column(6, plotOutput("link_distribution_plot", height = "300px"), style = "align:center;")),
        br(),
        fluidRow(column(6, br(), actionButton("fit_model", "Fit Model", icon=icon("calculator"), style="font-size:135%"), align = "center")),
        br(),
        hidden(div(id = "loading_message", HTML("<h4>Fitting model... (this may take a while if you have a large dataset)</h4>"))),
        hidden(div(id = "done_message", HTML("<h4>Done fitting model!</h4>")))
      )
    ),
    br(),
    hr(),
    br(),
    br(),
    fluidRow(
      column(
        3,
        h3("Model Summary"),
        HTML("<h4>This will display the summary of the fitted model. The top section contains model convergence statistics, and the bottom section presents the model summary. The model summary presents the model formula, fit statistics including AIC, variance of random effects, and the estimated threshold locations.</h4>"),
        br(),
        br()
      ),
      column(
        8, offset=1,
        hidden(div(id = "convergence_warning", HTML("<h4, style=\"color:red;\">Warning: model may not have converged!</h4>"))),
        withSpinner(verbatimTextOutput("model_summ"))
      )
    )
  ),
  
  
  tabPanel(
    "4) Results",
    fluidRow(
      div(
        id = "previous_section_warning_4",
        column(12, h4("Warning: check you have completed the previous sections!", style="color:red;")),
        br()
      ),
      column(11, offset=1, h3("Download Results")),
      br(),
      br(),
      br(),
      br(),
      column(12, fluidRow(
        column(
          3, offset=1,
          h4(
            "Click these buttons to download the norms for items and/or participants. This will contain the random effect estimates from the CLMM for each item and participant, but also the raw means, SDs, and N observations."
          )
        ),
        column(
          7,
          column(6, downloadButton("download_item_norms", "Download Item Norms", style="font-size:135%"), align = "center"),
          column(6, downloadButton("download_participant_norms", "Download Participant Norms", style="font-size:135%"), align = "center")
        )
      ))
    ),
    hr(),
    br(),
    br(),
    fluidRow(
      column(11, offset=1, h3("CLMM Threshold Locations")),
      br(),
      br(),
      br(),
      column(12, fluidRow(
        column(
          3, offset=1,
          br(), br(),
          h4(
            "This plot shows the estimated locations of the thresholds in the latent distribution. The thresholds \\(\\lambda_1\\), \\(\\lambda_2\\), ...\\(\\lambda_i\\) demarcate the boundaries between regions of the latent distribution which are associated with each possible rating. The latent distribution shown here is centred on zero. The participant and item random effects describe constant shifts in the location of this distribution associated with each individual participant and item."
          )
        ),
        column(7, plotOutput("m_thresh_locs_plot", height="300px"), align = "center")
      ))
    ),
    hr(),
    br(),
    br(),
    fluidRow(
      column(11, offset=1, h3("CLMM Item Norms")),
      br(),
      br(),
      br(),
      br(),
      column(
        3, offset=1,
        br(), br(),
        HTML(
          "<h4>These plots show <i>(1)</i> the distribution of item random effects in the sample, and <i>(2)</i> the difference between the item random effects and raw means of ratings.</h4>"
        )
      ),
      column(7,
             column(6, plotOutput("m_re_i_dist_plot", height="300px"), align = "center"),
             column(6, plotOutput("m_re_i_distort_plot", height="300px"), align = "center")
      )
    ),
    hr(),
    br(),
    br(),
    column(12, fluidRow(
      column(11, offset=1, h3("CLMM Participant Norms")),
      br(),
      br(),
      br(),
      br(),
      column(
        3, offset=1,
        br(), br(),
        HTML(
          "<h4>These plots show <i>(1)</i> the distribution of participant random effects in the sample, and <i>(2)</i> the difference between the participant random effects and raw means of ratings.</h4>"
        )
      ),
      column(7,
             column(6, plotOutput("m_re_s_dist_plot", height="300px"), align = "center"),
             column(6, plotOutput("m_re_s_distort_plot", height="300px"), align = "center")
      )
    ))
  ),
  
  tags$style(
    type = 'text/css',
    '.navbar-nav li a { font-size: 18px; }',
    ".navbar-brand { font-size: 22px; }"
  )
  
)



server <- function(input, output, session) {
  
  output$download_eg_norms <- downloadHandler(
    filename = "example_norms_data.csv",
    content = function(file) {
      file.copy("example_norms_data.csv", file)
    }
  )
  
  observeEvent(input$begin, {
    updateTabsetPanel(session, "main_navbar", selected = "1) Import Trials")
  })
  
  raw_trials <- reactive({
    file <- input$trials_file
    req(file)
    ext <- tools::file_ext(file$datapath)
    hide("previous_section_warning_2")
    
    if (ext == "csv") {
      readr::read_csv(file$datapath, col_types = cols())
    } else if (ext == "tsv") {
      readr::read_tsv(file$datapath, col_types = cols())
    } else if (ext %in% c("xls", "xlsx")) {
      tibble(openxlsx::read.xlsx(file$datapath))
    } else {
      NA
    }
  })
  
  output$raw_trials_table <- renderDataTable(raw_trials(), options = list(scrollX = TRUE))
  
  output$column_id_ui <- renderUI({
    req(raw_trials())
    trial_cols <- colnames(raw_trials())
    opts <- c("", trial_cols)
    names(opts) <- c("(Select Column)", trial_cols)
    
    fluidRow(
      column(4, selectInput("participant_col", "Participants Column", choices = opts, width="80%")),
      column(4, selectInput("item_col", "Items Column", choices = opts, width="80%")),
      column(4, selectInput("rating_col", "Ratings Column", choices = opts, width="80%"))
    )
  })
  
  observeEvent(input$one_participant, {
    if (input$one_participant) {
      disable("participant_col")
    } else {
      enable("participant_col")
    }
  })
  
  observeEvent(input$one_item, {
    if (input$one_item) {
      disable("item_col")
    } else {
      enable("item_col")
    }
  })
  
  output$setup_summ_stats <- renderUI({
    
    n_trials <- nrow(raw_trials())
    
    n_participants <- if (input$one_participant) "1" else
      if (is.na(input$participant_col) | input$participant_col=="") "?" else
        length(unique(raw_trials()[[input$participant_col]]))
    
    n_items <- if (input$one_item) "1" else
      if (is.na(input$item_col) | input$item_col=="") "?" else
        length(unique(raw_trials()[[input$item_col]]))
    
    mean_trials_per_subj <- if (input$one_participant) n_trials else
      if (is.na(input$participant_col) | input$participant_col=="") "?" else raw_trials() %>%
      group_by(!!dplyr::sym(input$participant_col)) %>%
      count() %>%
      pull(n) %>%
      mean() %>%
      round(2)
    
    mean_ratings_per_item <- if (input$one_item) n_trials else
      if (is.na(input$item_col) | input$item_col=="") "?" else raw_trials() %>%
      group_by(!!dplyr::sym(input$item_col)) %>%
      count() %>%
      pull(n) %>%
      mean() %>%
      round(2)
    
    resp_order <- if(is.na(input$rating_col) | input$rating_col=="") "?" else raw_trials() %>%
      pull(!!dplyr::sym(input$rating_col)) %>%
      unique() %>%
      sort() %>%
      paste(collapse = " < ")
    
    if (all(c(n_trials, n_participants, n_items, mean_trials_per_subj, mean_ratings_per_item) != "?") & (!is.na(input$rating_col) & input$rating_col!="")) {
      hide("previous_section_warning_3")
    } else {
      show("previous_section_warning_3")
    }
    
    div(fluidRow(
      column(12, br(), br()),
      column(12, HTML(sprintf("<h3>N Trials = %s</h3>", n_trials))),
      column(12, br(), br()),
      column(12, HTML(sprintf("<h3>Ratings: %s</h3>", resp_order))),
      column(12, br(), br()),
      column(6, HTML(sprintf("<h3>N Participants = %s</h3>", n_participants))),
      column(6, HTML(sprintf("<h3>N Items = %s</h3>", n_items))),
      column(6, HTML(sprintf("<h3>Average Trials per Participant = %s</h3>", mean_trials_per_subj))),
      column(6, HTML(sprintf("<h3>Average Trials per Item = %s</h3>", mean_ratings_per_item)))
    ), style = "align:center; text-align: center;")
  })
  
  formatted_trials <- reactive({
    d <- raw_trials()
    
    d[[input$rating_col]] <- factor(
      d[[input$rating_col]],
      levels = unique(sort(d[[input$rating_col]])),
      ordered = TRUE
    )
    
    if (!input$one_participant & input$participant_col!="") d[[input$participant_col]] <- as.character(d[[input$participant_col]])
    
    if (!input$one_item & input$item_col!="") d[[input$item_col]] <- as.character(d[[input$item_col]])
    
    d
  })
  
  output$link_distribution_plot <- renderPlot({
    plot_dist(input$link)
  })
  
  # record whether the model has been fit yet, initialised as FALSE
  mod_ever_fit <- reactiveVal(FALSE)
  
  m <- eventReactive(input$fit_model, {
    mod_ever_fit(TRUE)
    hide("previous_section_warning_4")
    
    disable("fit_model")
    show("loading_message")
    hide("done_message")
    hide("convergence_warning")
    
    f <- if (input$one_participant & input$one_item) {
      sprintf("%s ~ 1", input$rating_col)
    } else if (input$one_participant) {
      sprintf("%s ~ 1 + (1|%s)", input$rating_col, input$item_col)
    } else if (input$one_item) {
      sprintf("%s ~ 1 + (1|%s)", input$rating_col, input$participant_col)
    } else {
      sprintf("%s ~ 1 + (1|%s) + (1|%s)", input$rating_col, input$participant_col, input$item_col)
    }
    
    mod_fun <- if (input$one_participant & input$one_item) clm else clmm
    
    m_i <- tryCatch({
      mod_fun(
        formula = f,
        data = formatted_trials(),
        link = input$link,
        control = list(
          innerCtrl="warnOnly",
          checkRanef="warn"
        ),
        convergence = "warn"
      )
    }, error = function(e) {
      show("previous_section_warning_4")
      paste(as.character(e), "If the model did not converge, one solution may be to try a different link function. Alternatively, you may want to try a different model setup or change the control parameters manually (outside of this app).", sep="\n")
    })
    
    hide("loading_message")
    show("done_message")
    enable("fit_model")
    
    m_i
    
  })
  
  output$model_summ <- renderText({
    
    if (!is.character(m())) {
      
      convergence_dat <- if (any(class(m())=="clm")) m()$convergence else m()$optRes
      
      conv_code <- if (any(class(m())=="clm")) convergence_dat$code else convergence_dat$convergence
      
      if (conv_code != 0) show("convergence_warning")
      
      convergence_dat_names <- names(convergence_dat)
      convergence_dat_labelled <- sapply(1:length(convergence_dat), function(i) paste(list(convergence_dat_names[[i]], convergence_dat[[i]]), collapse=": "))
      
      paste(
        "CONVERGENCE:",
        "",
        paste(convergence_dat_labelled, collapse="\n"),
        "",
        "",
        "MODEL SUMMARY:",
        "",
        paste(capture.output(print(m())), collapse="\n"),
        sep="\n"
      )
      
    } else {
      m()
    }
    
  })
  
  item_norms <- reactive({
    res <- formatted_trials() %>%
      group_by(!!dplyr::sym(input$item_col)) %>%
      summarise(
        N = n(),
        M = mean(as.numeric(!!dplyr::sym(input$rating_col)), na.rm=TRUE),
        SD = mean(as.numeric(!!dplyr::sym(input$rating_col)), na.rm=TRUE)
      )
    
    if (mod_ever_fit()) {
      if (any(class(m())=="clmm")) {
        if (input$item_col %in% names(ranef(m()))) {
          item_re <- ranef(m())[[input$item_col]] %>%
            as_tibble(rownames = input$item_col) %>%
            rename(latent_M = `(Intercept)`)
          
          res <- left_join(res, item_re, by=input$item_col)
        }
      }
    }
    
    res
  })
  
  subj_norms <- reactive({
    res <- formatted_trials() %>%
      group_by(!!dplyr::sym(input$participant_col)) %>%
      summarise(
        N = n(),
        M = mean(as.numeric(!!dplyr::sym(input$rating_col)), na.rm=TRUE),
        SD = mean(as.numeric(!!dplyr::sym(input$rating_col)), na.rm=TRUE)
      )
    
    if (mod_ever_fit()) {
      if (any(class(m())=="clmm")) {
        if (input$participant_col %in% names(ranef(m()))) {
          participant_re <- ranef(m())[[input$participant_col]] %>%
            as_tibble(rownames = input$participant_col) %>%
            rename(latent_M = `(Intercept)`)
          
          res <- left_join(res, participant_re, by=input$participant_col)
        }
      }
    }
    
    res
  })
  
  output$download_item_norms <- downloadHandler(
    filename = function() {paste("item_norms", paste(input$rating_col, ".csv", sep=""), sep="_")},
    content = function(con) {write_csv(item_norms(), file=con)}
  )
  
  output$download_participant_norms <- downloadHandler(
    filename = function() {paste("participant_norms", paste(input$rating_col, ".csv", sep=""), sep="_")},
    content = function(con) {write_csv(subj_norms(), file=con)}
  )
  
  output$m_thresh_locs_plot <- renderPlot({
    plot_thresh(m())
  })
  
  output$m_re_i_dist_plot <- renderPlot({
    if ("latent_M" %in% colnames(item_norms())) {
      item_norms() %>%
        ggplot(aes(latent_M)) +
        geom_density(size = 1.5) +
        labs(x = "Item Random Effect", y = "Density")
    } else {
      NULL
    }
  })
  
  output$m_re_i_distort_plot <- renderPlot({
    if ("latent_M" %in% colnames(item_norms())) {
      item_norms() %>%
        ggplot(aes(latent_M, M)) +
        geom_point(size=1.5) +
        geom_smooth(method = "loess", se=FALSE) +
        labs(x = "Item Random Effect", y = "Raw Mean")
    } else {
      NULL
    }
  })
  
  output$m_re_s_dist_plot <- renderPlot({
    if ("latent_M" %in% colnames(subj_norms())) {
      subj_norms() %>%
        ggplot(aes(latent_M)) +
        geom_density(size = 1.5) +
        labs(x = "Participant Random Effect", y = "Density")
    } else {
      NULL
    }
  })
  
  output$m_re_s_distort_plot <- renderPlot({
    if ("latent_M" %in% colnames(subj_norms())) {
      subj_norms() %>%
        ggplot(aes(latent_M, M)) +
        geom_point(size=1.5) +
        geom_smooth(method = "loess", se=FALSE) +
        labs(x = "Participant Random Effect", y = "Raw Mean")
    } else {
      NULL
    }
  })
  
}

shinyApp(ui = ui, server = server)
