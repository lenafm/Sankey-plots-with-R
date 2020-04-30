library(shiny)

### load in packages
library("dplyr")
library("ggplot2")
library("ggforce")
library("ggalluvial")
library("ggparallel")
library("googleVis")

###=================================================================================
### define factor levels to specify order of strata
fct_levels <- c("Labour", "Conservative", "Liberal Democrat", "UKIP", 
                "Brexit Party", "Green Party", "SNP", "Plaid Cymru")

###=================================================================================
### define plot colours
colours <- c("#DC241f", "#0087DC", "#FAA61A", "#70147A", 
             "#12B6CF", "#528D6B", "#FDF38E", "#008142")
names(colours) <- fct_levels

###=================================================================================
### format data
dat_17_19 <- read.csv("sankey_data.csv")

dat_17_19_ggalluvial <- dat_17_19 %>% 
  mutate_at(vars(vote17, vote19),
            funs(factor(., levels = fct_levels)))

dat_17_19_ggforce <- dat_17_19  %>%
  gather_set_data(1:2) %>%
  arrange(x,vote19,desc(vote17)) %>% 
  mutate_at(vars(vote17, vote19, y),
            funs(factor(., levels = fct_levels)))

###=================================================================================
### define plot settings
stratum_width <- 0.3
stratum_font_colour <- "black"
stratum_fill_colour <- "white"
stratum_line_colour <- "black"
stratum_angle <- 0
legend_position <- ""
alpha <- 0.7


# Define UI for data download app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Choose package for Sankey plot"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Choose dataset ----
      selectInput("package", "Choose a package:",
                  choices = c("ggalluvial", "ggforce"),
                  selected = "ggalluvial"),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput("sankeyChart")
      
    )
    
  )
)

# Define server logic to display and download selected file ----
server <- function(input, output) {
  
  plot_data <- reactive({
    
    if(input$package == "ggalluvial"){
      df <- dat_17_19_ggalluvial
    } else if(input$package == "ggforce") {
      df <- dat_17_19_ggforce
    }
    df
    
    })
  
  plot <- reactive({
    if(input$package == "ggalluvial"){
      gg <- ggplot(dat_17_19_ggalluvial,
                   aes(y = freq, axis1 = vote17, axis2 = vote19)) +
        geom_alluvium(aes(fill = vote17, color = vote17),
                      width = stratum_width, 
                      alpha = alpha, 
                      knot.pos = 0.4) +
        geom_stratum(width = stratum_width,
                     fill = stratum_fill_colour, 
                     colour = stratum_line_colour) +
        ggfittext::geom_fit_text(infer.label = TRUE, 
                                 stat = "stratum", 
                                 width = stratum_width,
                                 colour = stratum_font_colour,
                                 fontface = "bold",
                                 min.size = 3) +
        scale_fill_manual(values = colours) +
        scale_color_manual(values = colours) +
        scale_x_continuous(breaks = 1:2, labels = c("GE vote 2017", "GE vote 2019")) +
        theme_minimal() +
        theme(
          legend.position = legend_position,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.title  = element_blank()
        )
    } else if(input$package == "ggforce") {
      gg <- ggplot(dat_17_19_ggforce, aes(x = x, id = id, split = y, value = freq)) +
        geom_parallel_sets(aes(fill = vote17), 
                           alpha = alpha, 
                           axis.width = stratum_width,
                           n=1000) +
        geom_parallel_sets_axes(axis.width = stratum_width, 
                                fill = stratum_fill_colour,
                                color = stratum_line_colour, 
                                size = 0.3) +
        geom_parallel_sets_labels(colour = stratum_font_colour, 
                                  size = 3, 
                                  angle = stratum_angle, 
                                  fontface="bold") +
        scale_fill_manual(values  = colours) +
        scale_color_manual(values = colours) +
        scale_x_discrete(labels = c("GE vote 2017", "GE vote 2019")) +
        theme_minimal() +
        theme(
          legend.position = legend_position,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.title  = element_blank()
        )
    }
    gg
  })
  
   
  # Table of selected sankey plot
  output$sankeyChart <- renderPlot({
    plot()
  })
  
  # Downloadable png of selected plot type ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$package, ".png")
    },
    content = function(file) {
      plot()
      ggsave(file, device = "png")
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)