install.packages("shinylive")
install.packages("httpuv")
library(shinylive)
library(httpuv)
library(shiny)
library(readxl) 
library(tools)  
library(DT)
library(ggplot2) 

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .nav-tabs > li > a {
        color: #FF69B4 !important; 
        font-weight: bold;
        border: none !important; 
      }
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:focus, 
      .nav-tabs > li.active > a:hover {
        color: #C71585 !important; 
        font-weight: bold;
        border: none !important; 
        border-top: 4px solid #C71585 !important;    /* Top pink line */
        border-bottom: 4px solid #C71585 !important; /* Bottom pink line */
        background-color: transparent !important;
      }
    "))
  ),
  titlePanel("Statistical Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Data Controls"),
      checkboxInput("ignore_na", "Ignore Missing Values (NA)", value = TRUE),
      hr(),
      
      # Module 1: Data Input
      conditionalPanel(
        condition = "input.main_tabs == 'Data Input'",
        h4("Data Upload Settings"),
        fileInput("file_upload", "Upload Dataset (.csv or .xlsx):", accept = c(".csv", ".xlsx", ".xls")),
        checkboxInput("has_header", "Does your dataset include column headers?", value = TRUE)
      ),
      
      # Modules 2 & 3: Summary & Plots
      conditionalPanel(
        condition = "input.main_tabs == 'Summary Statistics' || input.main_tabs == 'Plots'",
        h4("Variable Selection"),
        radioButtons("analysis_type", "Active Variable Type to Analyze:", choices = c("Quantitative", "Qualitative")),
        
        selectInput("quant_var", "Quantitative Variable (Numeric):", choices = NULL),
        selectInput("qual_var", "Qualitative Variable (Categorical):", choices = NULL),
        
        conditionalPanel(
          condition = "input.main_tabs == 'Plots'",
          h5("Plot Configurations"),
          conditionalPanel(
            condition = "input.analysis_type == 'Quantitative'",
            radioButtons("quant_plot_type", "Quantitative Plot Type:", choices = c("Histogram", "Density Curve")),
            sliderInput("bin_width", "Histogram Bin Width:", min = 1, max = 50, value = 15, step = 1)
          ),
          conditionalPanel(
            condition = "input.analysis_type == 'Qualitative'",
            radioButtons("qual_plot_type", "Qualitative Plot Type:", choices = c("Bar Chart", "Pie Chart"))
          )
        )
      ),
      
      # Module 4: Hypothesis Testing 
      conditionalPanel(
        condition = "input.main_tabs == 'Hypothesis Testing'",
        h4("Testing Diagnostic Workflow"),
        selectInput("ht_quant_var", "1. Quantitative Variable (Test Subject):", choices = NULL),
        radioButtons("test_structure", "2. Sample Structure:", choices = c("One-Sample", "Two-Sample")),
        radioButtons("test_logic", "3. Population Variance Known?:", choices = c("No (T-Test)", "Yes (Z-Test)")),
        numericInput("null_mu", "4. Null Mean (\u03bc\u2080):", value = 0),
        
        conditionalPanel(
          condition = "input.test_structure == 'Two-Sample'",
          selectInput("ht_qual_var", "5. Qualitative Variable (Grouping):", choices = NULL)
        )
      ),
      
      # Module 5: Regression 
      conditionalPanel(
        condition = "input.main_tabs == 'Regression'",
        h4("Regression Settings"),
        selectInput("reg_x", "Independent Variable (X, Quantitative):", choices = NULL),
        selectInput("reg_y", "Dependent Variable (Y, Quantitative):", choices = NULL),
        radioButtons("cor_method", "Correlation Type:", choices = c("pearson", "spearman"))
      ),
      
      # "ENTER" Panel
      conditionalPanel(
        condition = "input.main_tabs != 'Home Page' && input.main_tabs != 'User Guidelines' && input.main_tabs != 'Data Input'",
        hr(),
        wellPanel(
          style = "background-color: #F2D8E4; border: 2px solid #47132B;",
          h4("What To Do", style = "text-align: center; color: #47132B;"),
          helpText("Please select your chosen parameters above, then click ENTER to compute.", style = "text-align: center;"),
          actionButton("run_analysis", "ENTER", style = "width: 100%; font-weight: bold; font-size: 16px; background-color: #FF69B4; color: white; border: 2px solid #FF1493;")
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "main_tabs",
                  
                  tabPanel("Home Page", 
                           h3("Welcome to the Statistical Analysis Dashboard"),
                           p("This interactive application allows you to perform exploratory data analysis, visualization, and advanced inferential modeling."),
                           p("Navigate through the tabs above to begin your analysis workflow."),
                           p("Side Note: You are strongly encouraged to read and understand each instruction before executing any tasks to ensure a smooth experience while using the application.")),
                  
                  tabPanel("User Guidelines", 
                           h3("How to Use This Dashboard"),
                           h4("Step 1: Setting Up System Requirements & R Packages"),
                           tags$ul(
                             tags$li("You need a computer with ", tags$b("R"), " and ", tags$b("RStudio"), " installed."),
                             tags$li("Install these helper tools by typing this code in RStudio: ", tags$code('install.packages(c("shiny", "readxl", "tools", "DT", "ggplot2"))'), ".")
                           ),
                           h4("Step 2: Running the Application"),
                           tags$ul(
                             tags$li("Click the 'Run App' button in RStudio to start.")
                           ),
                           h4("Step 3: Exploring the Tabs"),
                           tags$ul(
                             tags$li(tags$b("Data Input: "), "Upload a .csv or .xlsx file using the sidebar file input. View dataset dimensions and an interactive preview table."),
                             tags$li(tags$b("Summary Statistics: "), "Select any variable to view quantitative summary metrics (mean, standard deviation) or categorical frequency tables."),
                             tags$li(tags$b("Plots: "), "Generate interactive histograms, density curves, or bar charts. Use the slider to modify histogram bin widths dynamically."),
                             tags$li(tags$b("Hypothesis Testing: "), "Toggle between one-sample and two-sample testing frameworks to compute test statistics and p-values."),
                             tags$li(tags$b("Regression: "), "Compute correlation coefficients, analyze linear regression models, view scatterplots with fitted trendlines, and check diagnostic residual plots.")
                           )
                  ),
                  
                  tabPanel("Data Input", 
                           h4("Module 1: Data Input & Management"), 
                           p("Please upload your dataset here to initialize the dashboard. This module allows you to preview your raw data in an organised form for you to verify its structure before running any mathematical operations."),
                           hr(),
                           h5("Interactive Data Preview:"), DTOutput("data_preview"),
                           hr(),
                           h5("Dataset Dimensions & Missing Values:"), tableOutput("data_metadata_table")),
                  
                  tabPanel("Summary Statistics", 
                           h4("Module 2: Descriptive Statistics Engine"), 
                           p("Here, you are able to explore the foundational metrics of your selected variables. For quantitative data, this module computes central tendency and dispersion instead of frequency counts for qualitative categories."),
                           p(":)"),
                           p("Please note that in the Data Controls settings, the 'Active Variable Type to Analyze' you chose will determine your output for this module."),
                           p("For example, choosing the 'Quantitative' option will result in output of only the 'Quantitative/Numeric Variable' even if both options were picked and vice versa."),
                           p("p/s: This module does not correlate the chosen 'Quantitative' option with nulled 'Qualitative' option and so forth."),
                           hr(),
                           tableOutput("var_summary_table")),
                  
                  tabPanel("Plots", 
                           h4("Module 3: Dynamic Data Visualization"), 
                           p("Now, you are able to visualize the distribution of your data in your preferred method.This module smoothly generates charts and graphs to help you spot patterns, trends, and outliers."),
                           p(":)"),
                           p("Please take note that the 'Data Controls' of this module works like the previous module did."),
                           p("After setting your parameters, the 'Active Variable Type to Analyze' you chose will determine your output for this module."),
                           p("Choosing the 'Quantitative' option will result in output of only the 'Quantitative/Numeric Variable' even if all options were picked and vice versa. Additionally, the 'Histogram Bin Width' option only applies to when you choose the 'histogram' option during plot configurations of Quantitative Variables."),
                           p("p/s: This module does not correlate the chosen 'Quantitative' option with nulled 'Qualitative' option and so forth."),
                           hr(),
                           plotOutput("dynamic_plot")),
                  
                  tabPanel("Hypothesis Testing", 
                           h4("Module 4: Automated Hypothesis Testing"), 
                           p("Are your values statistically significant? This module helps you to evaluate claims about your sample data using inferential logic."),
                           p("Please note that only Quantitative data values will be taken into account during this module."),
                           
                           hr(),
                           tableOutput("ht_summary_table"),
                           
                           div(
                             hr(),
                             h3("Dashboard Functional Guide"),
                             
                             h4("1. Module Controls"),
                             tags$ul(
                               tags$li(strong("Quantitative Variable:"), " Select the specific numerical data column you want to analyze."),
                               tags$li(strong("Sample Structure (One-Sample):"), " Select this to compare a single dataset's average against a fixed target number."),
                               tags$li(strong("Sample Structure (Two-Sample):"), " Select this to compare the averages of two separate groups against each other."),
                               tags$li(strong("Variance Known (No / T-Test):"), " Select this as your default choice. Use this whenever you are analyzing a sample and do not know the metrics for the entire population."),
                               tags$li(strong("Variance Known (Yes / Z-Test):"), " Select this only if you have the exact, historical variance data for the complete population."),
                               tags$li(strong("Null Mean:"), " Enter the baseline number you are testing against. If you are doing a standard Two-Sample test to see if groups differ, leave this at 0.")
                             ),
                             
                             h4("2. Formal Mathematical Logic"),
                             tags$ul(
                               tags$li(strong("One-Sample Null Hypothesis:"), " H0: \u03bc = \u03bc\u2080"),
                               tags$li(strong("Two-Sample Null Hypothesis:"), " H0: \u03bc\u2081 - \u03bc\u2082 = 0"),
                               tags$li(strong("T-Test Statistic:"), " T = (x\u0304 - \u03bc\u2080) / (s / \u221An)"),
                               tags$li(strong("Z-Test Statistic:"), " Z = (x\u0304 - \u03bc\u2080) / (\u03c3 / \u221An)")
                             ),
                             
                             br(),
                             h3("How to Read the Results (P-Value Guide)"),
                             
                             h4("1. The Quick Decision Rule"),
                             tags$ul(
                               tags$li(strong("p < 0.05 (Significant):"), " Your data proves a real difference or effect. The baseline claim is false."),
                               tags$li(strong("p \u2265 0.05 (Not Significant):"), " Your data does not show a meaningful difference. Keep the baseline claim.")
                             ),
                             
                             h4("2. Formal Statistical Interpretation"),
                             tags$ul(
                               tags$li(strong("Significance Level:"), " The standard error threshold is set at 0.05."),
                               tags$li(strong("Rejecting the Null (H0):"), " If p < 0.05, we reject H0 in favor of the alternative hypothesis (H1)."),
                               tags$li(strong("Failing to Reject:"), " If p \u2265 0.05, we fail to reject H0 due to insufficient evidence.")
                             ),
                             
                             h4("3. Manual Verification"),
                             p("If you ever need to manually verify the dashboard's calculated p-values or find the exact critical regions, you can input the raw test statistic (T or Z) directly into the Distribution mode of your Casio ClassWiz fx-570EX.")
                           )
                  ),
                  
                  tabPanel("Regression", 
                           h4("Module 5: Relationship Modeling & Regression"), 
                           p("Finally, analyze how two variables influence each other. The correlation and linear trendline generated here will help you map and predict mathematical relationships of your chosen variables."),
                           p("Please note that only Quantitative data values will be taken into account during this module."),
                           hr(),
                           h5("Model Parameters & Equation:"), tableOutput("reg_summary_table"),
                           verbatimTextOutput("reg_equation"),
                           h5("Relationship Scatterplot:"), plotOutput("scatter_plot"),
                           h5("Diagnostic Residual Plot:"), plotOutput("residual_plot"),
                           
                           # --- NEW REGRESSION GUIDE INSERTED HERE ---
                           div(
                             hr(),
                             h3("Dashboard Functional Guide"),
                             
                             h4("1. Correlation Type Selection"),
                             tags$ul(
                               tags$li(strong("Pearson (r):"), " Select this default option when you expect a standard, straight-line (linear) relationship between two continuous variables without extreme outliers."),
                               tags$li(strong("Spearman (rho):"), " Select this when your data consists of rankings, has extreme outliers, or follows a trend that consistently moves in one direction (monotonic) rather than a perfectly straight line.")
                             ),
                             
                             h4("2. How to Read the Results"),
                             tags$ul(
                               tags$li(strong("Correlation values near +1 or -1:"), " Indicate a very strong relationship between the variables."),
                               tags$li(strong("Correlation values near 0:"), " Indicate little to no mathematical relationship."),
                               tags$li(strong("R-Squared (R\u00b2):"), " Represents the percentage of variance in your Dependent Variable (Y) that is predictably explained by your Independent Variable (X).")
                             )
                           )
                           # ------------------------------------------
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Data Uploading
  uploaded_data <- reactive({
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    header_logic <- input$has_header
    
    df <- switch(ext,
                 csv = read.csv(input$file_upload$datapath, header = header_logic, na.strings = c("", "NA")),
                 xlsx = readxl::read_excel(input$file_upload$datapath, col_names = header_logic),
                 xls = readxl::read_excel(input$file_upload$datapath, col_names = header_logic),
                 validate("Invalid file format."))
    
    df <- as.data.frame(df)
    if(!header_logic) colnames(df) <- LETTERS[1:ncol(df)]
    
    df <- type.convert(df, as.is = TRUE)
    return(df)
  })
  
  observe({
    req(uploaded_data())
    df <- uploaded_data()
    
    quant_cols <- names(df)[sapply(df, is.numeric)]
    qual_cols <- names(df)[!names(df) %in% quant_cols]
    
    updateSelectInput(session, "quant_var", choices = quant_cols)
    updateSelectInput(session, "qual_var", choices = qual_cols)
    updateSelectInput(session, "ht_quant_var", choices = quant_cols)
    updateSelectInput(session, "ht_qual_var", choices = qual_cols)
    updateSelectInput(session, "reg_x", choices = quant_cols)
    updateSelectInput(session, "reg_y", choices = quant_cols)
  })
  
  # Module 1: Data Input
  output$data_preview <- renderDT({ uploaded_data() }, options = list(pageLength = 5, scrollX = TRUE))
  
  output$data_metadata_table <- renderTable({
    df <- uploaded_data()
    missing_counts <- colSums(is.na(df))
    data.frame(Attribute = names(missing_counts), Missing_Values_Count = missing_counts)
  }, striped = TRUE, hover = TRUE)
  
  # Module 2: Summary Statistics
  output$var_summary_table <- renderTable({
    input$run_analysis
    isolate({
      req(uploaded_data())
      df <- uploaded_data()
      
      if(input$analysis_type == "Quantitative") {
        req(input$quant_var)
        x <- df[[input$quant_var]]
        if(input$ignore_na) x <- na.omit(x)
        
        data.frame(
          Metric = c("Variable Name", "Data Type", "Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum", "Standard Deviation (\u03c3)"),
          Value = as.character(c(input$quant_var, "Quantitative", round(min(x), 4), round(quantile(x, 0.25), 4), round(median(x), 4), round(mean(x), 4), round(quantile(x, 0.75), 4), round(max(x), 4), round(sd(x), 4)))
        )
      } else {
        req(input$qual_var)
        x <- df[[input$qual_var]]
        if(input$ignore_na) x <- na.omit(x)
        
        freq_table <- as.data.frame(table(x))
        colnames(freq_table) <- c(paste("Category:", input$qual_var), "Frequency")
        freq_table
      }
    })
  }, striped = TRUE, hover = TRUE)
  
  # Module 3: Plots
  output$dynamic_plot <- renderPlot({
    input$run_analysis
    isolate({
      req(uploaded_data())
      df <- uploaded_data()
      
      if(input$analysis_type == "Quantitative") {
        req(input$quant_var, input$quant_plot_type)
        x <- df[[input$quant_var]]
        if(input$ignore_na) x <- na.omit(x)
        df_plot <- data.frame(val = x)
        
        p <- ggplot(df_plot, aes(x = val)) + theme_minimal() +
          labs(title = paste("Distribution of", input$quant_var), x = input$quant_var, y = "Density / Frequency")
        
        if(input$quant_plot_type == "Histogram") {
          p + geom_histogram(bins = input$bin_width, fill = "pink", color = "black")
        } else {
          p + geom_density(fill = "pink", alpha = 0.5, color = "black")
        }
      } else {
        req(input$qual_var, input$qual_plot_type)
        x <- df[[input$qual_var]]
        if(input$ignore_na) x <- na.omit(x)
        df_plot <- data.frame(val = x)
        
        freq_df <- as.data.frame(table(val = df_plot$val))
        p <- ggplot(freq_df, aes(x = if(input$qual_plot_type == "Pie Chart") "" else val, y = Freq, fill = val)) +
          theme_minimal() +
          labs(title = paste("Proportions of", input$qual_var), fill = input$qual_var)
        
        if(input$qual_plot_type == "Bar Chart") {
          p + geom_col(color = "black") + labs(x = input$qual_var, y = "Count") + theme(legend.position = "none")
        } else {
          p + geom_col(width = 1, color = "white") + coord_polar("y", start = 0) + theme_void()
        }
      }
    })
  })
  
  # Module 4: Hypothesis Testing
  output$ht_summary_table <- renderTable({
    input$run_analysis
    isolate({
      req(input$ht_quant_var, uploaded_data())
      df <- uploaded_data()
      x <- df[[input$ht_quant_var]]
      if(input$ignore_na) x <- na.omit(x)
      
      if(input$test_structure == "One-Sample") {
        
        if(input$test_logic == "Yes (Z-Test)") {
          z_stat <- (mean(x) - input$null_mu) / (sd(x) / sqrt(length(x)))
          p_val <- 2 * pnorm(-abs(z_stat))
          
          data.frame(
            Metric = c("Test Structure", "Methodology", "Test Statistic (Z)", "Null Mean (\u03bc\u2080)", "p-value"),
            Value = c("One-Sample", "Z-Test (Population Variance Known)", round(z_stat, 4), input$null_mu, format.pval(p_val))
          )
        } else {
          res <- t.test(x, mu = input$null_mu)
          
          data.frame(
            Metric = c("Test Structure", "Methodology", "Test Statistic (T)", "Null Mean (\u03bc\u2080)", "p-value"),
            Value = c("One-Sample", "T-Test (Population Variance Unknown)", round(res$statistic, 4), input$null_mu, format.pval(res$p.value))
          )
        }
        
      } else {
        req(input$ht_qual_var)
        g <- df[[input$ht_qual_var]]
        if(input$ignore_na) {
          valid_idx <- complete.cases(x, g)
          x <- x[valid_idx]; g <- g[valid_idx]
        }
        
        if(length(unique(g)) != 2) {
          return(data.frame(Error = "Grouping variable MUST have exactly 2 categories for a Two-Sample T-Test."))
        }
        
        res <- t.test(x ~ g)
        data.frame(
          Metric = c("Test Structure", "Methodology", "Test Statistic (T)", "Degrees of Freedom", "p-value"),
          Value = c("Two-Sample", "Independent T-Test", round(res$statistic, 4), round(res$parameter, 4), format.pval(res$p.value))
        )
      }
    })
  }, striped = TRUE, hover = TRUE)
  
  # Module 5: Regression
  output$reg_summary_table <- renderTable({
    input$run_analysis
    isolate({
      req(input$reg_x, input$reg_y, uploaded_data())
      df <- uploaded_data()
      x <- df[[input$reg_x]]
      y <- df[[input$reg_y]]
      
      if(input$ignore_na) {
        valid <- complete.cases(x, y)
        x <- x[valid]; y <- y[valid]
      }
      
      cor_val <- cor(x, y, method = input$cor_method)
      model <- lm(y ~ x)
      
      data.frame(
        Statistic = c(paste(toupper(tools::toTitleCase(input$cor_method)), "Correlation (r)"), "Intercept (\u03b2\u2080)", "Slope (\u03b2\u2081)", "R-Squared (R\u00b2)"),
        Value = c(round(cor_val, 4), round(coef(model)[1], 4), round(coef(model)[2], 4), round(summary(model)$r.squared, 4))
      )
    })
  }, striped = TRUE)
  
  output$reg_equation <- renderPrint({
    input$run_analysis
    isolate({
      req(input$reg_x, input$reg_y, uploaded_data())
      df <- uploaded_data()
      x <- df[[input$reg_x]]; y <- df[[input$reg_y]]
      if(input$ignore_na) { valid <- complete.cases(x, y); x <- x[valid]; y <- y[valid] }
      
      model <- lm(y ~ x)
      cat("Explicit Equation: Y =", round(coef(model)[1], 4), "+ (", round(coef(model)[2], 4), ") * X\n")
    })
  })
  
  output$scatter_plot <- renderPlot({
    input$run_analysis
    isolate({
      req(input$reg_x, input$reg_y, uploaded_data())
      df <- uploaded_data()
      if(input$ignore_na) df <- na.omit(df[, c(input$reg_x, input$reg_y)])
      
      ggplot(df, aes(x = .data[[input$reg_x]], y = .data[[input$reg_y]])) +
        geom_point(color = "darkblue", alpha = 0.6) +
        geom_smooth(method = "lm", color = "red", se = FALSE) +
        labs(title = "Linear Regression Scatterplot", x = input$reg_x, y = input$reg_y) + theme_minimal()
    })
  })
  
  output$residual_plot <- renderPlot({
    input$run_analysis
    isolate({
      req(input$reg_x, input$reg_y, uploaded_data())
      df <- na.omit(uploaded_data()[, c(input$reg_x, input$reg_y)])
      
      model <- lm(df[[input$reg_y]] ~ df[[input$reg_x]])
      res_df <- data.frame(Fitted = fitted(model), Residuals = residuals(model))
      
      ggplot(res_df, aes(x = Fitted, y = Residuals)) +
        geom_point(color = "darkgreen", alpha = 0.6) +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        labs(title = "Diagnostic Residual Plot", x = "Fitted Values (\u0176)", y = "Residuals (Error)") +
        theme_minimal()
    })
  })
}

shinyApp(ui = ui, server = server)