library(shiny)
library(rsconnect)
library(dplyr)
library(corrplot)
library(igraph)
library(readr)
library(networkD3)
library(shinyWidgets)

# file <- file.choose()
data_file <- "https://raw.githubusercontent.com/macmacmacmac/cs534_dash_shiny/main/10_Orgo1_user_pages_and_outcomes_categorized_no_NApages.csv"
data <- read.csv(data_file)

titles_file <- "https://raw.githubusercontent.com/macmacmacmac/cs534_dash_shiny/main/pagetitles.txt"
titles <- read_file(titles_file)

titles <- gsub("[^A-Za-z0-9\\,\r\n[[:space:]]","",titles)
titles_array <- unlist(strsplit(titles, "[\r\n]"))

retrow <- function(line){
  pageNum <- substr(line, 1, 5)
  theRest <- substr(line, 7, nchar(line))
  theRest <- gsub("\\s+", " ", theRest)
  theRest <- gsub("[[:space:]]", "\n", theRest)
  
  
  newLine <- c(pageNum, theRest)
  
  return(newLine)
}

titles_df <- lapply(titles_array,retrow)
titles_df <- do.call(rbind.data.frame, titles_df)
colnames(titles_df) <- c("pageNum", "pageTitle")

data_colnames <- colnames(data)

data_pagenames <- data_colnames[3:145]

getPageNums <- function(s){
  return(substr(s, 6,10))
}

data_pagenames <- lapply(data_pagenames, getPageNums)

get_page_title <- function(p){
  rows <- titles_df[titles_df$pageNum == p,2]
  row <- rows[1]
  return(row)
}

data_pagenames <- lapply(data_pagenames, get_page_title)

data_new_colnames <- data_colnames
data_new_colnames[3:145] <- unlist(data_pagenames)

colnames(data) <- data_new_colnames

corr_mat <- cor(data[,3:145])
round(corr_mat, 2)

buildCol <- function(namecol){
  tstatistic_vector <- unlist(lapply(colnames(data)[3:145], function(col1){
    t_test <- cor.test(data[[col1]], data[[namecol]])$p.value
    return(t_test)
  }))
  return(tstatistic_vector)
}

t_mat <- lapply(colnames(data)[3:145], buildCol)
t_mat <- do.call(rbind.data.frame, t_mat)

questionCols <- colnames(data)[3:145]
colnames(t_mat) <- questionCols
rownames(t_mat) <- questionCols

#-------------------------------------------------------------------------------
#
#
# UI CODE
#
#
#-------------------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel(
    h1("Pearson Correlations---Filtered by significant values after t test")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
      
      h4("Adjust significance level:"),
      sliderInput(inputId = "ALPHA_VALUE",
                  label = "Alpha Value (in e-5)",
                  min = 1.0,
                  max = 10.0,
                  value = 2.5,
                  step = 0.5),
      switchInput(inputId = "toggleLabel",
                  label = "Toggle label",
                  value = TRUE),
      
      plotOutput(outputId = "heatmapPlot",
                 height = 600)
    ),
    
    mainPanel(
      # "This is the main",
      h5("Smaller nodes indicate topics that come later, 
         width indicates strength, 
         color indicates direction of correlation"),
      # actionButton(inputId ="tkbutton", 
      #              label ="interact"),
      plotOutput(outputId = "networkPlot"),
    ),
    
    fluid = TRUE
  )
)
#-------------------------------------------------------------------------------
#
#
# SERVER CODE
#
#
#-------------------------------------------------------------------------------
server <- function(input, output) {
  # output$dimension_display <- renderText({
  #   paste(input$dimension[1], input$dimension[2], input$dimension[2]/input$dimension[1])
  # })
  
  heatmapPlot.width = reactive(input$dimension[1]*0.3)
  heatmapPlot.height = reactive(input$dimension[1]*0.3)                  
  output$heatmapPlot <- renderPlot(
    {
      alpha <- input$ALPHA_VALUE * 1e-5
      updater <- paste(input$dimension)
      
      filtered_t_mat <- t_mat >= alpha
      
      relevant_vertices <- as.data.frame(!filtered_t_mat)
      relevant_vertices[col(relevant_vertices) == row(relevant_vertices)] <- 0
      relevant_vertices_trim <- relevant_vertices %>% select(where(function(x) sum(x) > 0))
      relevant_vertices_trim <- colnames(relevant_vertices_trim)
      
      relevant_data <- data[, colnames(data) %in% relevant_vertices_trim]
      colnames(relevant_data) <- gsub("\n"," ",colnames(relevant_data))
      relevant_cor_mat <- cor(relevant_data)
      round(relevant_cor_mat, 2)
      
      tl.cex = max(-(input$ALPHA_VALUE - 2.5)*0.1 + 0.8, 0.01)
      tl.pos = ifelse(input$toggleLabel, 'td','n')
      # tl.pos = ifelse(input$ALPHA_VALUE>3,'n','td')
      
      corrplot(relevant_cor_mat,
               type = "upper", 
               order = "hclust", 
               tl.col = "black", 
               tl.cex = tl.cex,
               tl.pos=tl.pos, 
               tl.srt = 90)},
    width = reactive("auto"),
    height = reactive("auto")
  )
  
  networkPlot.width = reactive(input$dimension[1]*0.5)
  networkPlot.height = reactive(input$dimension[1]*0.6)
  output$networkPlot <- renderPlot({
    alpha <- input$ALPHA_VALUE * 1e-5
    
    filtered_t_mat <- t_mat >= alpha
    
    copy_corr_mat <- corr_mat
    copy_corr_mat[filtered_t_mat] <- 0
    copy_corr_mat[copy_corr_mat == 1] <- 0
    
    network <- graph_from_adjacency_matrix(copy_corr_mat, weighted=T, mode="undirected", diag=F)
    network <- delete.vertices(simplify(network), degree(network)==0)
    
    edge_list <- as.data.frame(as_edgelist(network))
    edge_list$correlation <- unlist(lapply(rownames(edge_list), function(x){
      (corr_mat[edge_list[x,1], edge_list[x,2]])
    }))
    edge_list<-data.frame(From=edge_list[,1], 
                          To=edge_list[,2], 
                          correlation=edge_list$correlation)
    new_network<-graph_from_data_frame(edge_list, directed=FALSE)
    
    vertex_indices <- apply(as.data.frame(V(new_network)$name), 1, function(x){
      which(questionCols == x)
    })
    maxmin <- function(x){
      return((max(x) - x)/(max(x)-min(x)))
    }
    grey_scale <- 5*maxmin(vertex_indices) + 7.5
    grey_label <- (0.4*maxmin(vertex_indices)) + 0.8
    
    label_sheet <- as.matrix(setNames(grey_label, V(new_network)$name))
    size_sheet <- as.matrix(setNames(grey_scale, V(new_network)$name))
    
    coords <- layout_with_fr(new_network)
    plot(new_network,
         layout = coords,
         vertex.frame.width = 0,
         vertex.size = size_sheet,
         vertex.color = rgb(0.8, 0.8, 0.8),
         vertex.label.cex = label_sheet,
         vertex.label.color = rgb(0, 0, 0),
         edge.width=5*E(new_network)$correlation,
         edge.color=ifelse(E(new_network)$correlation>0, 'blue','red'))
    
    #not ready
    # observeEvent(input$tkbutton, {
    #   tkplot(new_network,
    #          canvas.width = 900,
    #          canvas.height = 900,
    #          layout = coords,
    #          vertex.frame.width = 0,
    #          vertex.size = 15,
    #          vertex.label.cex = 0.8,
    #          vertex.color = rgb(0.8, 0.8, 0.8),
    #          vertex.label.color = rgb(0, 0, 0),
    #          edge.width=5*E(new_network)$correlation,
    #          edge.color=ifelse(E(new_network)$correlation>0, 'blue','red'))
    # })
    
    # tkplot(new_network,
    #        canvas.width = 900,
    #        canvas.height = 900,
    #        vertex.color = "gray",
    #        vertex.label.cex = 0.8,
    #        edge.width=2*E(new_network)$correlation+1,
    #        edge.color=ifelse(E(new_network)$correlation>0, 'blue','red')
    #       )
    
    
  }, 
  # width = 900,
  # height = 900
  width = networkPlot.width,
  height = networkPlot.height
  )
  
}

shinyApp(ui = ui, server = server)