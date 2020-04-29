library(shiny)

### load in packages
library("dplyr")
library("ggplot2")
library("ggforce")
library("ggalluvial")
library("ggparallel")
library("googleVis")

### define plot colours
colours = c("#DC241f", "#0087DC", "#FAA61A", "#70147A", "#12B6CF", 
            "#528D6B", "#FDF38E", "#008142")
names(colours) = c("Labour", "Conservative", "Liberal Democrat", "UKIP", "Brexit Party", 
                   "Green Party", "SNP", "Plaid Cymru")
alpha <- 0.7 # transparency value

# define factor levels
fct_levels <- c("Labour", "Conservative", "Liberal Democrat", "UKIP", "Brexit Party", "Green Party", "SNP", "Plaid Cymru")
fct_levels_17 <- paste0(fct_levels, " 17")
fct_levels_19 <- paste0(fct_levels, " 19")

### create data for sankey plots
dat_17_19 = read.csv("sankey_data.csv")

dat_17_19_ggalluvial = dat_17_19 %>% 
  mutate_at(vars(vote17, vote19),
            funs(factor(., levels = fct_levels)))

dat_17_19_ggforce <- dat_17_19  %>%
  gather_set_data(1:2) %>%
  arrange(x,vote19,desc(vote17)) %>% 
  mutate_at(vars(vote17, vote19),
            funs(factor(., levels = fct_levels)))

dat_17_19_googleVis = dat_17_19 %>% 
  mutate_at(vars(vote17, vote19),
            funs(factor(., levels = fct_levels))) %>%
  mutate(vote17 = paste0(vote17, " 17"), 
         vote19 = paste0(vote19, " 19")) %>% 
  arrange(factor(vote17, levels = fct_levels_17), 
          factor(vote19, levels = fct_levels_19)) %>% 
  as.data.frame()


# Define UI for data download app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Downloading Data"),
  
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
      gg <- ggplot(plot_data(),
                   aes(y = freq, axis1 = vote17, axis2 = vote19)) +
        geom_alluvium(aes(fill = vote17, color = vote17),
                      width = 1/12, alpha = alpha, knot.pos = 0.4) +
        geom_stratum(width = 1/6) +
        ggfittext::geom_fit_text(infer.label = TRUE, stat = "stratum", width = 1/6, min.size = 3) +
        scale_fill_manual(values  = colours) +
        scale_color_manual(values = colours) +
        scale_x_continuous(breaks = 1:2, labels = c("GE vote 2017", "GE vote 2019")) +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 14, face = "bold")
        )
    } else if(input$package == "ggforce") {
      gg <- ggplot(plot_data(), aes(x = x, id = id, split = y, value = freq)) +
        geom_parallel_sets(aes(fill = vote19), alpha = alpha, axis.width = 0.2,
                           n=100, strength = 0.5) +
        geom_parallel_sets_axes(axis.width = 0.25, fill = "gray95",
                                color = "gray80", size = 0.15) +
        geom_parallel_sets_labels(colour = 'gray35', size = 4.5, angle = 0, fontface="bold") +
        scale_fill_manual(values  = colours) +
        scale_color_manual(values = colours) +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 20, face = "bold"),
          axis.title.x  = element_blank()
        )
    }
    gg
  })
  
   
  # Table of selected sankey plot
  output$sankeyChart <- renderPlot({
    plot()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(plot_data(), file, row.names = FALSE)
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)