library(tidyverse)
library(shiny)
library(DT)
library(here)


### Load the data here

# Load training data, add filenames as column for selection later
uncoded_files <- list.files(here("data", "Uncoded"))


uncoded <- list()
for(a in 1:length(uncoded_files)){
  uncoded[[a]] <- read_csv(here("data", "Uncoded", uncoded_files[a]))
}
names(uncoded) <- uncoded_files

# generate vector of unique serial numbers for household rosters
#all_serials <- unique(combine(unlist(map(uncoded, `[[`, "serial1")), unlist(map(uncoded, `[[`, "serial2"))))
#  combine() %>% 
#  unique()


# Load Philly data for household rosters
# Two filters: needed columns, and match the serial numbers in the training data
#bind_rows(select(read.csv(here("data", "Phl10.csv"), stringsAsFactors = F), 
#                                namefrst, namelast, age, sex, relate, bpl, year, serial, pernum),
#                         select(read.csv(here("data", "Phl20.csv"), stringsAsFactors = F), 
#                                namefrst, namelast, age, sex, relate, bpl, year, serial, pernum),
#                         select(read.csv(here("data", "Phl30.csv"), stringsAsFactors = F), 
#                                namefrst, namelast, age, sex, relate, bpl, year, serial, pernum),
#                         select(read.csv(here("data", "Phl40.csv"), stringsAsFactors = F), 
#                                namefrst, namelast, age, sex, relate, bpl, year, serial, pernum)) %>% 
#  filter(serial %in% all_serials) %>% 
#  write_csv(here("data", "for_rosters.csv"))
for_rosters <- read_csv(here("data", "for_rosters.csv"))


### Define UI for application that plots features of movies 
ui <- fluidPage(
  
  title = "Linking Census Records",
  
  # Set the layout here, and everntually add input and output features 
  fluidRow(
    
    # Coumn with all section controls (input)
    column(3, 
           h4("Control Panel"),
           selectInput(inputId = "file",
                       label = "Select Training File",
                       choices = uncoded_files,
                       selected = uncoded_files[1]),
           br(),
           numericInput(inputId = "focal",
                        label = "Select Focal Record",
                        value = 1,
                        min = 1,
                        max = 100),
           br(),
           actionButton(inputId = "match",
                        label = "Identify Match"),
           verbatimTextOutput('x')
           ),
    
    # Editable table to compare and code matches (output)
    column(9,
           h4("Focal Record"),
           DTOutput(outputId = "focal_record"),
           h4("Potential Matches"),
           DTOutput(outputId = "block")
           )
    
  # End of first row
  ), 
  
  # Start second row of household rosters
  fluidRow(
    
    # focal roster
    column(6,
           h4("Focal Household Roster"),
           DTOutput(outputId = "focal_roster")),
    
    # comparison roster
    column(6,
           h4("Comparison Household Roster"),
           DTOutput(outputId = "compare_roster"))
  
  # End of second row
  )

# end of UI
)

# Define server function required to generate outputs
server <- function(input, output) {
  
  # get training file
  file <- reactive({
    req(input$file)
    uncoded[[input$file]]
  })

  # set focal record and block of potential matches
  focal_record <- reactive({
    req(input$focal)
    filter(file(), focal_app_id == input$focal) %>% 
      select(namefrst2, mi2, namelast2, age2, marst2, bpl2, serial2) %>% 
      unique()
  })
    
  block <- reactive({
    req(input$focal)
    filter(file(), focal_app_id == input$focal) %>% 
      select(namefrst1, mi1, namelast1, jw_frst, jw_last, age1, marst1, bpl1, serial1, compare_app_id)
  })
  
  # grab focal household id
  hh_focal <- reactive({
    focal_record()[["serial2"]]
  })
  
  # grab comparison household id
  hh_compare <- reactive({
    req(input$block_rows_selected)
    slice(block(), input$block_rows_selected)[["serial1"]]
  })
  
  # get household rosters
  focal_roster <- reactive({
    filter(for_rosters, serial == hh_focal()) %>% 
      select(namefrst, namelast, age, sex, relate, bpl)
  })
  
  # set comparison record's roster
  compare_roster <- reactive({
    filter(for_rosters, serial == hh_compare()) %>% 
      select(namefrst, namelast, age, sex, relate, bpl)
  })
  
  # OUTPUT focal record
  output$focal_record <- renderDT({
      datatable(data = select(focal_record(), 1:5), options = list(pageLength = 1), selection = 'none')
  })
    
  
  # OUTPUT block of potential matches
  output$block <- renderDT({
      datatable(data = select(block(), 1:8), options = list(pageLength = 5), selection = 'single')
  })
    
  
  # OUTPUT focal hh roster
  output$focal_roster <- renderDT({
      datatable(data = focal_roster(), options = list(pageLength = 5), selection = 'none')
  })
  
  
  # OUTPUT comparison roster
  output$compare_roster <- renderDT({
      datatable(compare_roster(), options = list(pageLength = 5), selection = 'none')
  })

  # print the selected indices
  output$x = renderPrint({
    s <- input$block_rows_selected
    if (is.null(s)) {
      cat('Selected Match: None')
    } else {
      cat('Selected Match: Row ', s)
    }
  })
  
  # identify match in file() expression based on selected row in potential matches
  observeEvent(input$match, {
    req(input$file)
    req(input$focal)
    
    s <- input$block_rows_selected
    
    if(file.exists(here("data", "Coded", input$file))){
      dont_add <- read_csv(here("data", "Coded", input$file)) %>% 
        .[["focal_app_id"]] %>% 
        unique()
      if(input$focal %in% dont_add == F){
        if(is.null(s)){
          file() %>% 
            filter(focal_app_id == input$focal) %>% 
            write_csv(here("data", "Coded", input$file),
                      append = T)
        } else {
          file() %>% 
            filter(focal_app_id == input$focal) %>%
            mutate(match = if_else(compare_app_id == s, 1, 0)) %>% 
            write_csv(here("data", "Coded", input$file),
                      append = T)
        }
      }
    } else {
      if(is.null(s)){
        file() %>% 
          filter(focal_app_id == input$focal) %>% 
          write_csv(here("data", "Coded", input$file),
                    append = F)
      } else {
        file() %>% 
          filter(focal_app_id == input$focal) %>%
          mutate(match = if_else(compare_app_id == s, 1, 0)) %>% 
          write_csv(here("data", "Coded", input$file),
                    append = F)
      }
    }
  })
# End of server function 
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
