# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ##############################################################
    #        COVID-19 mapper (right summary panel)               #
    ##############################################################
    # Reactive shown COVID-19 situation
    reactive_db = reactive({
        cv_cases %>% filter(Date == input$plot_date)
    })
    
    output$reactive_case_count = renderText({
        print(str_c('Plot Date: ', input$plot_date, ' Total Cases: ', sum(reactive_db()$Accumulate_Case)), logger = 'company')
        paste0(prettyNum(sum(reactive_db()$Accumulate_Case), big.mark=","), " Total Cases")
    })
    
    output$reactive_death_count = renderText({
        print(str_c('Plot Date: ', input$plot_date, ' Deaths: ', sum(reactive_db()$Accumulate_Death)))
        paste0(prettyNum(sum(reactive_db()$Accumulate_Death), big.mark=","), " Deaths")
    })
    
    output$reactive_recovered_count = renderText({
        print(str_c('Plot Date: ', input$plot_date, ' Recovered: ', sum(reactive_db()$Accumulate_Recovery)))
        paste0(prettyNum(sum(reactive_db()$Accumulate_Recovery), big.mark=","), " Recovered")
    })
    
    output$reactive_active_count = renderText({
        print(str_c('Plot Date: ', input$plot_date, ' Active: ', sum(reactive_db()$Active_Case)))
        paste0(prettyNum(sum(reactive_db()$Active_Case), big.mark=","), " Active")
    })
    
    output$clean_date_reactive = renderText({
        format(as.POSIXct(input$plot_date),"%d %B %Y") #10 March 2020
    })
    


    new_cases_plot = function(cv_aggregated, plot_date) {
        plot_df_new = filter(cv_aggregated, Date <= plot_date)
        g1 = ggplot(data = plot_df_new, mapping = aes(x = Date, y = New_Case, fill = Region)) + 
            geom_bar(position = "stack", stat = "identity") + 
            ylab(label = "New Cases") + xlab(label = "") + theme_classic() + 
            scale_fill_manual(values = c(covid_new)) +
            scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
            theme(legend.title = element_blank(), legend.position = "", plot.margin = margin(5, 12, 5, 5))
        ggplotly(g1, tooltip = c("Date", "New_Case")) %>% layout(showlegend = FALSE)
    }
    
    output$new_cases_curve = renderPlotly({
        ggplotly(new_cases_plot(cv_aggregated, input$plot_date))
    })
    
    
    cumulative_plot = function(cv_aggregated, plot_date) {
        plot_df = subset(cv_aggregated, Date <= plot_date)
        g1 = ggplot(plot_df, aes(x = Date, y = Accumulate_Case, color = Region)) + geom_line() + geom_line(size = 0.8, alpha = 0.8) +
            ylab(label = "Cumulative Cases") + xlab(label = "") + theme_classic() + 
            scale_colour_manual(values = c(covid_cul)) +
            scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
            theme(legend.title = element_blank(), legend.position = "", plot.margin = margin(5, 12, 5, 5))
        ggplotly(g1, tooltip = c("Date", "Accumulate_Case")) %>% layout(showlegend = FALSE)
    }
    
    output$cumulative_case_cure <- renderPlotly({
        cumulative_plot(cv_aggregated, input$plot_date)
    })

    multiple_plot = function(cv_aggregated, plot_date) {
      plot_mul_df = subset(cv_aggregated, Date <= plot_date) %>% select("Date", "New_Case", "New_Recovery", "New_Death")
      plot_mul_df_melt = reshape2::melt(plot_mul_df, id=c("Date"))
      names(plot_mul_df_melt) = c("Date", "Type", "Value")

      g1 = ggplot(plot_mul_df_melt, aes(x=Date, y=Value, col=Type)) + geom_line() +
      ylab(label = "People Number") + xlab(label = "") + theme_classic() +
      scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
      theme(legend.title = element_blank(), legend.position = c(0.3, 0.7), plot.margin = margin(5, 12, 5, 5), 
            legend.margin = margin(t=-30,r=6,b=0,l=6)) + scale_color_manual(values=c('#ff0400', '#006d2c', '#dfd8d3'), labels=c("New Cases","New Recovered","New Deaths"))
      g1
    }

    output$new_case_recovery_death_cure <- renderPlot({ #renderPlot
        multiple_plot(cv_aggregated, input$plot_date)
    })
    

    #-------------------------------- COVID-19 mapper (world map) -----------------------------
    bins = c(0, 100, 1000, 5000, 10000, 50000, 100000, 500000, 1000000, Inf)
    # c("antiquewhite", "lightpink", "indianred1", "indianred2", "indianred3", "firebrick1", "firebrick3", "firebrick4") => quite really hard to present the color, and the color is correct but from map hard to see
    cv_palette = colorBin(c("#CCFFCC", "#FFCCCC", "#CC0000"), domain = sort(unique(cv_cases$Accumulate_Case)), bins = bins, na.color = "Black")

    cv_palette1 = reactive({
        colorBin(c("#CCFFCC", "#FFCCCC", "#CC0000"), domain = sort(unique(reactive_db()$Accumulate_Case)), bins = bins, na.color = "Black")
    })

    cv_palette2 = reactive({
        range_gap = as.numeric(sort(summary(reactive_db()$Accumulate_Case)))
        bins = c(0, ceiling(range_gap[2]), ceiling(range_gap[3]), ceiling(range_gap[4]), ceiling(range_gap[5]), ceiling(range_gap[6]))
        colorBin(c("pink", "red", "violetred4"), domain = sort(unique(reactive_db()$Accumulate_Case)), bins = bins, na.color = "Black")
    })

    output$mymap <- renderLeaflet({ 
      basemap
    })
    
    # TODO: leaflet() => Actually put or not put is same. Just for ~ syntax explicitly inherit from worldcountry
    basemap = leaflet(worldcountry) %>%
              addTiles() %>% # Add default OpenStreetMap map tiles
              addProviderTiles(providers$CartoDB.Positron) %>%
              # Set the rectangular bounds of the world map, its diagonal line with the two defined geo points. 
              # [lng1, lat1] - [lng2, lat2]. The map will centralize in this rectangular.
              fitBounds(lng1 = -165.399182, lat1 = 67.804057, lng2 = 197.927184, lat2 = 0.477900) %>% 
              # removed into the auto added on the observe 
              #addLegend(position = "bottomright", pal = cv_palette, values = ~cv_cases$Accumulate_Case, title = "<small>Accumulate Case</small>")  %>%
              addLayersControl(
                  position = "bottomright",
                  overlayGroups = c("2019-COVID (active)", "2019-COVID (new)", "2019-COVID (cumulative)"),
                  # TRUE (the default), the layers control will be rendered as an icon that expands when hovered over. 
                  # Set to FALSE to have the layers control always appear in its expanded state. 
                  options = layersControlOptions(collapsed = FALSE)
              ) %>%
              hideGroup(c("2019-COVID (new)", "2019-COVID (cumulative)")) %>%
              addEasyButton(easyButton(icon="fa-globe", title="Zoom to Level 1", onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
              addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me", onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
              

    # infected country change with plot date
    reactive_polygons = reactive({
        worldcountry_plot_polygons = worldcountry[worldcountry$ISO_A3 %in% reactive_db()$alpha3, ]
        color_value = do.call('rbind', lapply(worldcountry_plot_polygons$ISO_A3, function(wd_alpha3) {
            reactive_db() %>% filter(alpha3 == wd_alpha3) %>% select(Accumulate_Case)
        }))
        worldcountry_plot_polygons$Color = as.numeric(as.character(color_value$Accumulate_Case))
        worldcountry_plot_polygons
    })

    # reactive_polygons = reactive({
    #     worldcountry[worldcountry$ISO_A3 %in% reactive_db()$alpha3, ]
    # })

    reactive_polygons_COLOR = reactive({
        color_value = do.call('rbind', lapply(reactive_polygons()$ISO_A3, function(wd_alpha3) {
            reactive_db() %>% filter(alpha3 == wd_alpha3) %>% select(Accumulate_Case)
        }))
        as.numeric(as.character(color_value$Accumulate_Case))
    })

    # new cases for the latest past 24Hs
    reactive_db_last24h = reactive({
      cv_cases %>% filter(date == input$plot_date & new_cases > 0)
    })

    color_bin_gap = function(plot_date) {
        # summary => min 1st mean median 3st max
        # eg: 2020-01-22 => 0.000000   0.000000   0.000000   0.000000   3.094972 548.000000
        range_gap = unique(as.numeric(sort(summary((cv_cases %>% filter(Date == plot_date))$Accumulate_Case))))
        for (index in 1:length(range_gap)){
            if(index == 1) {
                range_gap[index] = 0
            } else {
                range_gap[index] = ceiling(range_gap[index])
            }
        }
        range_gap
    }

    color_palette_test = reactive({colorBin(
                palette = c("#CCFFCC", "#FFCCCC", "#CC0000"), 
                domain = sort(unique((cv_cases %>% filter(Date == input$plot_date))$Accumulate_Case)), 
                bins = color_bin_gap(input$plot_date), 
                na.color = "Black"
            )})

    color_palette = function(plot_date, color_value) {
        colorBins = colorBin(
                palette = c("#CCFFCC", "#FFCCCC", "#CC0000"), 
                domain = sort(unique((cv_cases %>% filter(Date == input$plot_date))$Accumulate_Case)), 
                bins = color_bin_gap(plot_date), 
                na.color = "Black"
            )
        colorBins(color_value)
    }


    observeEvent(input$plot_date, {
        leafletProxy("mymap") %>%
        clearMarkers() %>%
        clearShapes() %>%
        removeControl("legend") %>%
        addLegend(
            position = "bottomright", 
            pal = colorBin(
                palette = c("#CCFFCC", "#FFCCCC", "#CC0000"), 
                domain = sort(unique((cv_cases %>% filter(Date == input$plot_date))$Accumulate_Case)), 
                bins = color_bin_gap(input$plot_date), 
                na.color = "Black"
            ), 
            values = (cv_cases %>% filter(Date == input$plot_date))$Accumulate_Case, 
            title = "<small>Accumulate Case</small>",
            layerId = "legend"
        ) %>%
        # here, this data overwritten the worldcountry from leaflet and only fullfil the national territory for the selected affected country
        #addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_palette(reactive_polygons_COLOR())) #%>%
        addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = color_palette(input$plot_date, reactive_polygons()$Color)) %>%
        
        # addCircleMarkers(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5), 
        #                  fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
        #                  label = sprintf("<strong>%s (past 24h)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths, reactive_db_last24h()$new_recovered, reactive_db_last24h()$newper100k) %>% lapply(htmltools::HTML),
        #                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col), textsize = "15px", direction = "auto")) %>%
        
        addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(Accumulate_Case)^(1/5), 
                         fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                         label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g<br/>Colour: %s", reactive_db()$Country, reactive_db()$Accumulate_Case, reactive_db()$Accumulate_Death,reactive_db()$Accumulate_Recovery, reactive_db()$per100k, cv_palette(reactive_db()$Accumulate_Case)) %>% lapply(htmltools::HTML),
                         labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col), textsize = "15px", direction = "auto")) 
        #%>%
        
        # addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/5), 
        #                  fillOpacity = 0.1, color = covid_col, group = "2019-COVID (active)",
        #                  label = sprintf("<strong>%s (active)</strong><br/>Confirmed cases: %g<br/>Cases per 100,000: %g<br/><i><small>Confirmdes individuals known to have<br/>recovered (%g) or died (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
        #                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col), textsize = "15px", direction = "auto"))
    })
  
    
    
    
    
    # #--------------------------------Data-----------------------------
    # printedColumn = c('country', 'date', 'cases', 'new_cases', 'deaths', 'new_deaths', 'recovered', 'new_recovered', 'active_cases')
    
    # output$rawtable = renderTable(
    #     tail(cv_cases %>% select(printedColumn), input$maxrows)
    # )
    
    # output$downloadCsv = downloadHandler(
    #     filename = function() {
    #         paste0("COVID_", cv_max_date, ".csv")
    #     },
    #     content = function(file) {
    #         write.csv(cv_cases %>% filter(date==cv_max_date) %>% select(printedColumn), file)
    #     }
    # )
    
    #--------------------------------Outbreak comparisons-----------------------------
    # add footnote for Outbreak comparisons_cases
    output$epi_notes_1 <- renderText({
        if(input$comparison_metric=="cases") { paste0("Note that the axis is on a log10 scale so moves in 10-fold increments.
                                                  The 60.8 million estimated cases of H1N1 dwarf all other outbreaks of plotted on a standard linear scale.") }
    })
    
    # add footnote for Outbreak comparisons_deaths
    output$epi_notes_2 <- renderText({
        if(input$comparison_metric=="deaths") { 
            paste0("For H1N1, the number of laboratory-confirmed deaths reported by the WHO is displayed. Subsequent modelling studies have estimated the actual number to be in the range of 123,000 to 203,000.")
        }
    })
    
    # add note for Outbreak comparisons_cfr
    output$epi_notes_3 <- renderText({
        if(input$comparison_metric=="cfr") { 
            paste0("For COVID-19, this displays the proportion of confirmed cases who have subsequently died. When factoring in mild or asymptomatic infections that are not picked up by case surveillance efforts, current estimates place the case fatality rate in the range of 0.3-1%.")
        }
    })
}
