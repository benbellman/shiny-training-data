library(shiny)
library(DT)
library(here)
library(tidyverse)

### Load the data here

# Load training data, add filenames as column for selection later
uncoded_files <- list.files(here("data", "Uncoded"))

uncoded <- list()
for(a in 1:length(uncoded_files)){
  uncoded[[a]] <- read_csv(here("data", "Uncoded", uncoded_files[a]))
}
names(uncoded) <- uncoded_files

# generate vector of unique serial numbers for household rosters
all_serials <- unique(combine(unlist(map(uncoded, `[[`, "serial1")), unlist(map(uncoded, `[[`, "serial2"))))
  combine() %>% 
  unique()


# Load Philly data for household rosters
# Two filters: needed columns, and match the serial numbers in the training data
for_rosters <- bind_rows(select(read.csv(here("data", "Phl10.csv"), stringsAsFactors = F), 
                                namefrst, namelast, age, sex, relate, bpl, year, serial),
                         select(read.csv(here("data", "Phl20.csv"), stringsAsFactors = F), 
                                namefrst, namelast, age, sex, relate, bpl, year, serial),
                         select(read.csv(here("data", "Phl30.csv"), stringsAsFactors = F), 
                                namefrst, namelast, age, sex, relate, bpl, year, serial),
                         select(read.csv(here("data", "Phl40.csv"), stringsAsFactors = F), 
                                namefrst, namelast, age, sex, relate, bpl, year, serial)) %>% 
  filter(serial %in% all_serials)


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
           numericInput(inputId = "compare",
                        label = "Select Comparison Record",
                        value = 1,
                        min = 1,
                        max = 99),
           br(),
           actionButton(inputId = "save",
                        label = "Save File")
           # End of Control Panel
           ),
    
    # Editable table to compare and code matches (output)
    column(9,
           DTOutput(outputId = "focal_record"),
           br(),
           DTOutput(outputId = "block")
           )
    
  # End of first row
  ), 
  
  # Start second row of household rosters
  fluidRow(
    
    # focal roster
    column(6,
           h4("Focal Roster"),
           DTOutput(outputId = "focal_roster")),
    
    # comparison roster
    column(6,
           h4("Comparison Roster"),
           DTOutput(outputId = "compare_roster"))
  
  # End of second row
  )

# end of UI
)

# Define server function required to generate outputs
server <- function(input, output) {
  
  # get training file
  observeEvent(input$file, {
    file <- uncoded[[input$file]]
  })
  
  # limit to focal record
  output$focal_record <- renderDT({
    filter(file, focal_app_id == input$focal) %>% 
      select(namefrst2, mi2, namelast2, age2, marst2, bpl2) %>% 
      unique() %>% 
      datatable(options = list(pageLength = 1))
  }, server = F, editable = F)
    
  
  # define block
  output$block <- renderDT({
    filter(file, focal_app_id == input$focal) %>% 
      select(match, namefrst1, mi1, namelast1, jw_frst, jw_last, age1, marst1, bpl1) %>% 
      datatable(options = list(pageLength = 5))
  }, server = F, editable = T)
    
  
  # define focal hh roster
  output$focal_roster <- renderDT({
    filter(for_rosters, serial == output$focal_record[1, "serial2"]) %>% 
      select(namefrst, namelast, age, sex, relate, bpl) %>% 
      datatable(options = list(pageLength = 5))
  }, server = F, editable = F)
  
  
  # define comparison roster
  output$compare_roster <- renderDT({
    filter(for_rosters, serial == filter(output$block, 
                                         compare_app_id == input$compare)[["serial2"]]) %>% 
      select(namefrst, namelast, age, sex, relate, bpl) %>% 
      datatable(options = list(pageLength = 5))
  }, server = F, editable = F)

  
  # update training data when a match is coded with app
  observeEvent(input$block_cell_edit, {
    info <- input$block_cell_edit
    
    # change the match column in the training file
    file[file$focal_app_id == input$focal &
           file$compare_app_id == as.numeric(info$row), "match"] <- info$value
  })
  
  # save training file when "Save" button is clicked
  observeEvent(input$save, {
    write_csv(file, here("data", "Uncoded", input$file))
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
