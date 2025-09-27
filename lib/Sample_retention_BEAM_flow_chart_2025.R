library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggalluvial)
library(shinyWidgets)

# Load and process data
#print(getwd()) # problem is that shiny is setting wd where it starts (in lib)

wd<-getwd()

if (grepl("/lib",wd)) setwd("..")

data_short <- fread("data/obs_short_data_vis.csv")


data_short[, (c("monitoring_suitable", "taxon_bycatch_monitor_ok", "base_model_heterogeneity")) := lapply(.SD, as.factor), .SDcols = c(c("monitoring_suitable", "taxon_bycatch_monitor_ok", "base_model_heterogeneity"))]

data_short[, bycatch_reported := as.factor((ifelse(n_ind > 0 | unused_n_ind_monitoring > 0 | unused_n_ind_sampling > 0, "yes", "no")))]
# 
# Now I need to put monitoring as unsuitable for lines where n_ind = 0 but unused_n_ind_monitoring > 0 
data_short[, monitoring_suitable := as.character(monitoring_suitable)]
data_short[n_ind == 0 & unused_n_ind_monitoring > 0, monitoring_suitable := "no"]
# #Same for the sampling protocol
data_short[, taxon_bycatch_monitor_ok := as.character(taxon_bycatch_monitor_ok)]
data_short[n_ind == 0 & unused_n_ind_sampling > 0, taxon_bycatch_monitor_ok :=  "no"]


#And put the BPUE_available to 0

data_short$message <- factor(data_short$message)

levels(data_short$message) <- c("","Fishing data available \nfor all levels of r.e.","Levels from \u2265 1 \nr.e. not available","r.e. on monitoring \nor sampling protocol")
#And put the BPUE_available to 0

data_short[, bpue_available := (ifelse(n_ind == 0, 0, bpue_available))]

data_short[, bycatch_reported := as.factor(as.character(bycatch_reported))]



# Assuming `data_short` is already created


ui <- fluidPage(
  titlePanel("Sample retention through the BEAM framework"),
  
  # Horizontal layout for filters
  fluidRow(
    column(3,
           pickerInput(
             inputId = "ecoregion",
             label = "Ecoregion:",
             choices = NULL,
             multiple = TRUE,
             options = list(`actions-box` = TRUE, `live-search` = TRUE),
             selected = NULL
           )
    ),
    column(3,
           pickerInput(
             inputId = "metierl4",
             label = "Metier:",
             choices = NULL,
             multiple = TRUE,
             options = list(`actions-box` = TRUE, `live-search` = TRUE),
             selected = NULL
           )
    ),
    column(3,
           pickerInput(
             inputId = "taxon",
             label = "Taxon:",
             choices = NULL,
             multiple = TRUE,
             options = list(`actions-box` = TRUE, `live-search` = TRUE),
             selected = NULL
           )
    ),
    column(2,
           pickerInput(
             inputId = "bycatch_reported",
             label = "Bycatch Reported:",
             choices = c("yes", "no"),
             multiple = TRUE,
             options = list(`actions-box` = TRUE),
             selected = c("yes", "no")
           )
    )
    
  ),
  
  
  fluidRow(
    column(12, br())  # Adds vertical space
  ),
  
  
  # Plot below filters
  fluidRow(
    column(12,
           plotOutput("filteredPlot1", height = "700px")
    )
  ), fluidRow(
    column(12,
           plotOutput("filteredPlot2", height = "700px")
    )
  ),
  
  fluidRow(
    column(12, br())  # Adds vertical space
  ),
  
  fluidRow(
    column(12,
           textOutput("text")
    )
  )
  
  
)


server <- function(input, output, session) {
  # Get unique values
  ecoregions <- unique(data_short$ecoregion)
  metiers <- unique(data_short$metierl4)
  taxa <- unique(data_short$taxon)
  
  # Observe select all toggles
  
  observe({
    updatePickerInput(session, "ecoregion", choices = unique(data_short$ecoregion), selected = unique(data_short$ecoregion))
    updatePickerInput(session, "metierl4", choices = unique(data_short$metierl4), selected = unique(data_short$metierl4))
    updatePickerInput(session, "taxon", choices = unique(data_short$taxon), selected = unique(data_short$taxon))
    updatePickerInput(session, "bycatch_reported", choices = unique(data_short$bycatch_reported), selected = unique(data_short$bycatch_reported))
    
  })
  
  
  flow_data1 <- reactive({
    
    filtered1 <- data_short %>%
      filter(
        ecoregion %in% input$ecoregion,
        metierl4 %in% input$metierl4,
        taxon %in% input$taxon,
        bycatch_reported %in% input$bycatch_reported
      ) %>%
      mutate(
        check1 = ifelse(bycatch_reported == "yes", "Bycatch occurred", "No bycatch reported"),
        check2 = ifelse(monitoring_suitable == "yes", "Suitable", "Unsuitable"),
        check3 = ifelse(taxon_bycatch_monitor_ok == "yes", "Taxa monitored", "Taxa not monitored"),
        bpue_available = factor(ifelse(bpue_available == 1, "Yes", "No"),
                                levels = c("No", "Yes"))
      ) %>%
      group_by(check1, check2, check3, bpue_available) %>%
      summarise(Freq = n(), .groups = "drop") %>%
      mutate(
        Percentage = Freq / sum(Freq) * 100,
        sqrtFreq = sqrt(Freq)
      )
    
    return(filtered1)
  })
  
  flow_data2 <- reactive({
    
    filtered2 <- data_short[bpue_available == "1",] %>%
      filter(
        ecoregion %in% input$ecoregion,
        metierl4 %in% input$metierl4,
        taxon %in% input$taxon,
        bycatch_reported %in% input$bycatch_reported
      ) %>%
      mutate(
        bpue_available = "Yes",
        check4 = message,
        bpue_usable = ifelse(bpue_usable == "yes", "Yes", "No"),
        total_bycatch = ifelse(bycatch_estimated == "yes", "Total bycatch estimate available", "No estimate of total bycatch")
      ) %>%
      group_by(bpue_available,check4, bpue_usable, total_bycatch) %>%
      summarise(Freq = n(), .groups = "drop") %>%
      mutate(
        Percentage = Freq / sum(Freq) * 100,
        sqrtFreq = sqrt(Freq)
      )
    
    return(filtered2)
  })
  
  output$filteredPlot1 <- renderPlot({
    df <- flow_data1()
    
    if (nrow(df) == 0) return(NULL)
    
    # Now plot
    ggplot(
      df,
      aes(
        axis1 = check1,
        axis2 = check2,
        axis3 = check3,
        axis4 = bpue_available,
        y = sqrtFreq
      )
    ) +
      scale_x_discrete(
        limits = c(
          "Bycatch", 
          "Monitoring protocol",
          "Sampling protocol",
          "BPUE Available"
        )
      ) +
      geom_alluvium(aes(fill = bpue_available),
                    width = 1/30, col = "white") +
      geom_stratum(width = 1/15,
                   color = "black",
                   fill = "white") +
      geom_text(
        stat = "stratum",
        aes(label = after_stat(paste0(stratum))),# "\n", round(prop*100, 1), "%"))),
        angle = 90, hjust = 0.5, size = 3.5
      ) +
      theme_minimal() +
      scale_fill_manual(
        values = c("No" = "grey70", "Yes" = "#80b918"),
        drop = FALSE) +
      theme(
        legend.position = "top",
        axis.title.y = element_text(size = 10, margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 9), title = element_text(size=16)
      ) +
      labs(
        title = "BEAM Filtering and Quality Control Pipeline Conditioning the Availability of BPUE Estimates and Suitability for Total Bycatch Estimation",
        y = "Number of species x metier L4 x ecoregion combination (Square-root transformed for visualisation purposes)",
        fill = "BPUE available"
      )
  })
  output$filteredPlot2 <- renderPlot({
    df <- flow_data2()
    
    if (nrow(df) == 0) return(NULL)
    
    # Now plot
    ggplot(
      df,
      aes(
        axis1 = bpue_available,
        axis2 = check4,
        axis3 = bpue_usable,
        axis4 = total_bycatch,
        y = sqrtFreq
      )
    ) +
      scale_x_discrete(
        limits = c("BPUE Available", "Data availability for model fitting", "BPUE usable", "Total bycatch estimated"
        )
      ) +  geom_alluvium(aes(fill = total_bycatch),
                         width = 1/30, col="white") +
      geom_stratum(width = 1 /12 ,
                   color = "black",
                   fill = "white") +
      geom_text(stat = "stratum", aes(label = after_stat(paste0(stratum))), #"\n", round(prop*100, 1), "%"))),
                angle = 90, hjust = 0.5, size = 3) +
      theme_minimal() +
      scale_fill_manual(values = c("grey70","#3a86ff"), labels = c("No", "Yes")) +
      theme(legend.position = "top", axis.title.y = element_text(size=10, margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)), axis.text.y = element_text(size=7), axis.text.x = element_text(size=9)) +
      labs(title = "", y = "Number of species x metier L4 x ecoregion combinations for which a BPUE is available (Square-root transformed)", fill = "Total bycatch estimate available")
  })
  output$text <- renderText(
    "\n \n Note: The y-axis represent the total pool of unique species x ecoregion x metierl4 samples available based on the selected parameters. For each combination, the 'best case scenario' is considered. For instance, if a species had some bycatch reported both under suitable and unsuitable monitoring and/or sampling protocol, it will only show up as suitable, as it lead to a BPUE estimate. Only species for which NO bycatch was reported under suitable conditions but some was reported under unsuitable monitoring or sampling protocol are included as such. The no bycatch section contains combinations for which no event was reported despite suitable monitoring and sampling, and others for which no bycatch was reported but the best available data was also unsuitable based on the BEAM quality assessment framework")
  
  
}

shinyApp(ui = ui, server = server)
