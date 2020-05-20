library(shiny)

### load in packages
library("dplyr")
library("titanic")
library("ggplot2")
library("ggforce")

# ### set wd (remove later)
# setwd("C:/Users/Lena/Documents/Datasketch/Sankey plots in R")
# 
#
# ### specify plot settings
# stratum_font_colour = "white"
# stratum_colour = "black"
# flow_colours <- c("orange", "blue")
# labels <- c("change", "these", "labels")

### load data
input_data <- read.csv("./data/example_data.csv")

# ### for local testing
# df <- input_data
# col_vars <- c("Class", "Sex", "Age")
# fill_var <- "Survived"
# fill_var <- ""


###================================================================================
### functions
###================================================================================

prepare_data <- function(df, col_vars, fill_var = ""){
  
  if(fill_var == ""){
    fill_var <- NULL
    }
  
  if(is.null(fill_var)){
    groupby <- col_vars
  } else if(fill_var %in% col_vars){
    groupby <- col_vars
  } else {
    groupby <- c(col_vars, fill_var)
  }
  
  dat_prelim <- df %>% 
    group_by_at(groupby) %>% 
    summarise(Freq = n()) %>%
    ungroup() %>% 
    mutate(id = row_number())
  
  if(is.null(fill_var)){
  dat_plot <- dat_prelim %>% 
    tidyr::gather(key = "x",
                  value = "stratum",
                  factor_key = TRUE,
                  col_vars)
  } else {
    dat_plot <- dat_prelim %>% 
      tidyr::gather(key = "x",
                    value = "stratum",
                    factor_key = TRUE,
                    col_vars) %>%
      left_join(dat_prelim %>%
                  mutate_(fill = fill_var) %>%
                  select(id, fill),
                by = "id")
  }
  return(dat_plot)
}

create_sankey_plot <- function(df, fill_var = "", palette = NULL, labels = NULL, stratum_colour = "black"){
  
  if(fill_var == ""){
    fill_var <- NULL
  }
  
  
  if(stratum_colour == "black"){
    stratum_font_colour = "white"
  } else {
    stratum_font_colour = "black"
    
  }
  
  stratum_line_colour = "black"
  
  stratum_width <- 0.4
  stratum_angle <- 0
  legend_position <- "right"
  alpha <- 0.7
  
  gg <- ggplot(df, aes(x = x, id = id, split = stratum, value = Freq)) +
        geom_parallel_sets(alpha = alpha, axis.width = stratum_width) +
        geom_parallel_sets_axes(axis.width = stratum_width, 
                                fill = stratum_colour,
                                colour = stratum_line_colour) +
        geom_parallel_sets_labels(colour = stratum_font_colour, angle = stratum_angle) +
        theme_minimal() +
        theme(
          legend.position = legend_position,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.title.x  = element_blank()
          )
  
  if(!is.null(fill_var)){
    gg <-   gg <- ggplot(df, aes(x = x, id = id, split = stratum, value = Freq)) +
      geom_parallel_sets(aes(fill = fill), alpha = alpha, axis.width = stratum_width) +
      geom_parallel_sets_axes(axis.width = stratum_width, 
                              fill = stratum_colour,
                              colour = stratum_line_colour) +
      geom_parallel_sets_labels(colour = stratum_font_colour, angle = stratum_angle) +
      scale_fill_discrete(name = fill_var) +
      theme_minimal() +
      theme(
        legend.position = legend_position,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.title.x  = element_blank()
      )
  }
  
  if(!is.null(palette)){
    gg <- gg +
      scale_fill_manual(name = fill_var, values = palette) +
      scale_color_manual(values = palette)
  }
  
  if(!is.null(labels)){
    gg <- gg +
      scale_x_discrete(labels = labels)
  }
  
  return(gg)
}


###================================================================================
### app
###================================================================================

# Define UI for data download app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("App for Sankey plot"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose columns ----
      selectizeInput("chooseColumns", 
                     "Choose columns for axes:",
                     selected = c("Class", "Sex", "Age"),
                     choices = names(input_data), 
                     multiple = TRUE
                     ),
      selectizeInput(
        "fillval", 
        "Choose column for fill:",
        choices = names(input_data),
        options = list(
          placeholder = 'Optionally select a fill variable',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      radioButtons("stratumColour", 
                   "Choose colour of stratum",
                   choices = list("Black" = "black", "White" = "white"), 
                   selected = "black"
                   ),
      
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # tableOutput("table"),
      plotOutput("sankeyChart")
      
    )
    
  )
)

# Define server logic to display and download selected file ----
server <- function(input, output) {
  
  plot_data <- reactive({
    prepare_data(df = input_data, 
                 col_vars = input$chooseColumns, 
                 fill_var = input$fillval)
    })
  
  plot <- reactive({
    create_sankey_plot(df = plot_data(), 
                       stratum_colour = input$stratumColour,
                       fill_var = input$fillval)
    })

  output$sankeyChart <- renderPlot({
    plot()
  })
  
  output$table <- renderTable({
    head(plot_data() %>% arrange(id), n = 10)
  })
  
  # Downloadable png of selected plot type ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("sankey.png")
    },
    content = function(file) {
      plot()
      ggsave(file, device = "png")
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)