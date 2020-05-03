ui = bootstrapPage(
    # TODO: test the Google Analytics
    tags$head(
      includeHTML("Google Analytics/gtag.html"),
      tags$head(tags$script(src = "message-handler.js")),
      tags$head(tags$script(src = "busy.js"))
    ),
    
    # TODO: click the name should be back to the index.html
    navbarPage(title = "COVID-19 Tracker", 
               id = "nav",
               theme = shinytheme("cerulean"), # https://bootswatch.com/, theme layout of navigation bar 
               collapsible = TRUE, # collapse the navigation elements into a menu when the width of the browser is less than 940 pixels
               
               tabPanel(title = "COVID-19 Mapper", icon = icon("map"),
                        div(class = "busy", width="100%", height="100%", img(id = "loading_gif", src = "loading.gif", width="3%", height="3%")),
                        div(class = "outer",
                            tags$head(includeCSS("styles.css")),
                            leafletOutput(outputId = "mymap", width="100%", height="100%"),
                            absolutePanel(id = "controls", fixed = TRUE, draggable = TRUE,
                                          top = 70, left = 50, width = 280, height = "auto",
                                          
                                          h3(textOutput("reactive_case_count"), align = "right"),
                                          h4(textOutput("reactive_death_count"), align = "right"),
                                          h4(textOutput("reactive_recovered_count"), align = "right"), # green
                                          h4(textOutput("reactive_active_count"), align = "right"), #red
                                          h6(textOutput("clean_date_reactive"), align = "right"),
                                          tags$i(h6("Updated once daily. For more regular updates, refer to: ", tags$a(href = "https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard."))),
                                          plotlyOutput("new_cases_curve", height ="130px", width = "100%"),
                                          plotlyOutput("cumulative_case_cure", height = "130px", width = "100%"),
                                          plotOutput("new_case_recovery_death_cure", height = "130px", width = "100%"), #plotOutput
                                          sliderInput("plot_date",
                                                      label = h5("Select Date"),
                                                      min = cv_min_date,
                                                      max = cv_max_date,
                                                      value = cv_max_date,
                                                      timeFormat = "%d %b",
                                                      animate = animationOptions(interval = 1000, loop = FALSE, playButton = icon('play', lib = 'glyphicon'))
                                          )
                            ),
                            #absolutePanel(id = "logo", bottom = 20, left = 60, width = 80, fixed = TRUE, draggable = FALSE, height = "auto", tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png', height='40', width='80'))),
                            absolutePanel(id = "logo", bottom = 20, left = 20, width = 80, fixed = TRUE, draggable = FALSE, height = "auto", actionButton(inputId = "envelope", label = "", icon = icon("envelope"), style='padding:5px', onclick = "var a = $('a[data-value=\"Your Comment\"]'); a.click()")
                            )
                        )
               ),
               tabPanel(title = "Your Comment", icon = icon("envelope"),
                  textInput(inputId = "customer_email", label = a(style = "color:black", "Email", a(style = "color:red", "*")), placeholder = "Your Email Address"),
                  textAreaInput(inputId = "customer_comment", label = a(style = "color:black", "Comment", a(style = "color:red", "*")), placeholder = "Please share your valuable comment. Thanks.", width = "400px", height = "300px", rows = 30),
                  actionButton("submit", "Submit")
               ),

               tabPanel(title = "Region plots", 
                        helpText("Coming soon......")
                        # sidebarLayout(
                        #     
                        #     sidebarPanel(
                        #         
                        #         pickerInput(inputId = "level_select",
                        #                     label = "Level",
                        #                     choices = c("Global", "Continent", "Country"),
                        #                     selected = c("Country"),
                        #                     multiple = FALSE),
                        #         
                        #         pickerInput(inputId = "region_select",
                        #                     label = "Country/Region",
                        #                     choices = c("TEMP-VALUE"),
                        #                     options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                        #                     selected = c("TEMP-VALUE"),
                        #                     multiple = TRUE),
                        #         
                        #         pickerInput(inputId = "outcome_slect",
                        #                     label = "Outcome",
                        #                     choices = c("Cases", "Deaths"),
                        #                     selected = c("Cases"),
                        #                     multiple = FALSE),
                        #         
                        #         "Plots from the selected dropdown list value. Countries with at least 100 confirmed cases are included."
                        #     ),
                        #     
                        #     mainPanel(
                        #         tabsetPanel(
                        #             tabPanel("New"),
                        #             tabPanel("Cumulative"),
                        #             tabPanel("Cumulative (log10)"))
                        #     )
                        # )
               ),
               
               tabPanel(title = "SARS mapper", helpText("Coming soon......")),
               tabPanel(title = "Outbreak comparisons", helpText("Coming soon......")
                        # sidebarLayout(
                        #     
                        #     sidebarPanel(
                        #         radioButtons(inputId = "comparison_metric",
                        #                      label = h3("Select comparison"),
                        #                      choices = c("Cases" = "cases", "Deaths" = "deaths", "Countries/regions affected" = "countries", "Case fatality rate" = "cfr")
                        #         ),
                        #         textOutput(outputId = "epi_notes_1"),
                        #         textOutput(outputId = "epi_notes_2"),
                        #         textOutput(outputId = "epi_notes_3")
                        #     ),
                        #     
                        #     mainPanel(
                        #         #plotlyOutput("comparison_plot"), width = 6)
                        #     )
                        # )
                ),
               
               tabPanel(title = "Data", 
                        numericInput(inputId = "maxrows", label = h3("Show latest day rows"), value = 20, min = 10, max = 60),
                        #  ------------------------------------------------------------------
                        # |  Output function   | render function |          CREATES          |
                        #  ------------------------------------------------------------------
                        # |htmlOutput/uiOutput |     renderUI    | a Shiny tag object or HTML|
                        #  ------------------------------------------------------------------
                        # |   imageOutput      |   renderImage   |          images           |
                        #  ------------------------------------------------------------------
                        # |   plotOutput       |    renderPlot   |          plots            |
                        #  ------------------------------------------------------------------
                        # |   tableOutput      |    renderTable  |     data frame, matrixs   |
                        #  ------------------------------------------------------------------
                        # |   textOutput       |   renderText    |      character strings    |
                        #  ------------------------------------------------------------------
                        # | verbatimTextOutput |   renderPrint   |     any printed output    |
                        #  ------------------------------------------------------------------
                        tableOutput(outputId = "rawtable"),
                        downloadButton(outputId = "downloadCsv", label = h5("Download latest day")),
                        tags$br(),tags$br(),
                        "Adapted from timeline data published by ", 
                        tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering.")
               )
    )
)



