library(shiny)
library(ggplot2)
library(plotly)
library(shinyBS)
library(shinydashboard)
library(bupaR)
library(RColorBrewer)
library(processanimateR)
library(shiny)
library(dplyr)
library(DiagrammeR)


source(paste(getwd(), "helper.r", sep = "/"))


df <- readRDS("/Users/vibha_verma/Documents/MCKPI_17Feb/DATA/veeva_vault/veeva_vault_eventlog.rds")
df <- head(df,1000)

UI <- fluidPage(
    tags$head(tags$style(HTML('

                        .modal-lg {
                        width: auto;
                        }
                        #plot {
                        height: 750px !important;
                        }
                       '))),
    selectInput("color_scale", label = h5("Select color scale:"),
                choices = c(rownames(brewer.pal.info)),
                selected = "PuBu"),
    selectInput("color_edges", label = h5("Select Edge Color:"),
                choices = colours(),
                selected = "dodgerblue4"),
    numericInput("node_fontsize", label = h5("Select Node font size"), value = 40),
    actionButton("expand","Expand graph"),
    processanimaterOutput("graph"),
    bsModal("modalExample",
            "Your plot",
            "expand", # <----set the observer to the right button
            size = "large",
            processanimaterOutput("plot"))
)

Server <- function(input, output) {
    
    output$plot <- renderPlot({
        hist(500)
    })
    
    freq <-reactive({
        frequency(color_scale = input$color_scale,color_edges =input$color_edges)
    })
    
    output$graph <- renderProcessanimater(expr = {
        graph <- custom_process_map(df, render = F,type=freq())
        
        node_attr <- graph$nodes_df
        node_attr$fontsize <- input$node_fontsize
        graph$nodes_df <- node_attr
        
        model <-
            DiagrammeR::add_global_graph_attrs(graph,
                                               attr = "rankdir",
                                               value = NULL,
                                               attr_type = "graph")
        animate_process(
            df,
            model,
            mode = "absolute",
            legend = "color",
            mapping = token_aes(
                color = token_scale(
                    "handling",
                    scale = "ordinal",
                    range = RColorBrewer::brewer.pal(5, "YlOrBr")
                )
            ),
            duration = 60
        )
    })
    
    output$plot <- renderProcessanimater(expr = {
        graph <- custom_process_map(df, render = F,type=freq())
        
        node_attr <- graph$nodes_df
        node_attr$fontsize <- input$node_fontsize
        graph$nodes_df <- node_attr
        
        model <-
            DiagrammeR::add_global_graph_attrs(graph,
                                               attr = "rankdir",
                                               value = NULL,
                                               attr_type = "graph")
        animate_process(
            df,
            model,
            mode = "absolute",
            legend = "color",
            mapping = token_aes(
                color = token_scale(
                    "handling",
                    scale = "ordinal",
                    range = RColorBrewer::brewer.pal(5, "YlOrBr")
                )
            ),
            duration = 60
        )
    })
    # })
}


shinyApp(ui = UI, server = Server)
