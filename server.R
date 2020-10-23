#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:?
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    
    reactive_bbox <- reactiveValues(bbox = NULL)
    
    
    
      
  
        
        # create the map output for the property and where it's located. 
        output$mymap <- renderLeaflet({
            
            
            # create the leaflet object
            m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
              # addProviderTiles("MapBox",
              #                  options = providerTileOptions(id = "mapbox.dark",
              #                                                noWrap = FALSE,
              #                                                accessToken = "pk.eyJ1IjoiZGhlaXMxMzM3IiwiYSI6ImNrZWM1b2c0dzAwMm8yeWxpd2dwOWVhOWMifQ.Lfk0nQUVqMT14q0Idad6Xw")) %>%
                addMapboxGL(style = "mapbox://styles/mapbox/dark-v10",
                            accessToken = "pk.eyJ1IjoiZGhlaXMxMzM3IiwiYSI6ImNrZWM1b2c0dzAwMm8yeWxpd2dwOWVhOWMifQ.Lfk0nQUVqMT14q0Idad6Xw",
                            setView = FALSE) %>%
                setView(lat = 39.7392,
                        lng = -104.9903,
                        zoom = 12) %>%
                addControl(html = actionButton("explore", "Explore"), position = "topright") %>%
                addDrawToolbar(position = "topleft",
                               editOptions = editToolbarOptions(remove = TRUE),
                               polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(fill = FALSE, weight = 4)),
                               rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(fill = FALSE, weight = 4)))


            # call the leaflet object to display the map
            m
          
          # mapdeck(token = key,
          #         location = c(-104.9903, 39.7392),
          #         zoom = 10)
            
        })
        
    
    
     observe({
         
     if (is.null(reactive_bbox)) {
         NULL
     } else {
         
         
         }
     })
    
    observeEvent(input$explore, {



      df <- reactive_df()
      
      
      map_var <- reactive_df()[[input$map_variable]]

        if (is.factor(map_var)) {

          pal <- colorFactor(
            palette = "Set3",
            domain = map_var
          )
        } else {

            if (input$color_type == "bin") {
              
              pal <- colorBin(
                palette = "plasma",
                domain = map_var,
                bins = 5,
                pretty = TRUE
              )
            
          } else if (input$color_type == "quantile") {
              
              pal <- colorQuantile(
                palette = "viridis",
                domain = map_var,
                n = 4
              )
              
          } else { 
            
              pal <- colorNumeric(
                  palette = "plasma",
                  domain = map_var
              
              )
          
          }
      }


        if (input$boundary == "zip") {

          zip <- zip %>% filter(ZCTA5CE10 %in% input$zip)

          leafletProxy("mymap") %>%
            clearShapes() %>%
            removeControl(layerId = "legend") %>%
            addLegend(position = "bottomright", pal = pal, values = map_var,
                      layerId = "legend", labels = labelFormat(between = "", big.mark = "")) %>%
            addProviderTiles("MapBox",
                             options = providerTileOptions(noWrap = FALSE,
                                                           id = "mapbox.light",
                                                           accessToken = "pk.eyJ1IjoiZGhlaXMxMzM3IiwiYSI6ImNrZWM1b2c0dzAwMm8yeWxpd2dwOWVhOWMifQ.Lfk0nQUVqMT14q0Idad6Xw")) %>%
            addDrawToolbar(position = "topleft",
                           editOptions = editToolbarOptions(remove = TRUE),
                           polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(fill = FALSE, weight = 4)),
                           rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(fill = FALSE, weight = 4))) %>%
            addPolylines(data = zip, weight = 1) %>%
            addCircles(data = df, lng = ~longitude, lat = ~latitude, color= ~pal(map_var),
                       stroke = FALSE, fillOpacity = 2, radius = 7,
                       popup = paste("<h3>", 
                                     paste(df$prop_addr_num, df$prop_addr_pre,
                                           df$prop_addr_st_name, df$prop_addr_st_type,
                                           ifelse(is.na(df$prop_addr_unit), "",
                                                  df$prop_addr_unit)),
                                     df$prop_addr_city, df$prop_addr_zip, "</h3>",
                                     "<h4>", "Close Price:",  df$close_price, "<br>",
                                     "Beds:", df$beds, "<br>",
                                     "Baths:", df$baths, "<br>",
                                     "SqFt:", df$total_sqft, "<br>",
                                     "Year Built:", df$year_built, "<br>",
                                     "Year Sold:", df$year_sold, "<br>",
                                     "Property Type:", df$property_type))
          
          
        } else if (input$boundary == "city") {
            
          city <- city %>% filter(NAME10 %in% input$city)
        
          leafletProxy(data = df, "mymap") %>%
            clearShapes() %>%
            removeControl(layerId = "legend") %>%
            addLegend(position = "bottomright", pal = pal, values = map_var,
                      layerId = "legend", labels = labelFormat(between = "", big.mark = "")) %>%
            addProviderTiles(providers$CartoDB.Positron,
                             options = providerTileOptions(updateWhenIdle = FALSE)) %>%
            addDrawToolbar(position = "topleft",
                           editOptions = editToolbarOptions(remove = TRUE),
                           polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(fill = FALSE, weight = 4)),
                           rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(fill = FALSE, weight = 4))) %>%
            addPolylines(data = city, weight = 1, label = city$NAME10, 
                         group = ~NAME10,
                         color = "white") %>%
            addCircles(lng = ~longitude, lat = ~latitude, color= ~pal(map_var),
                       stroke = FALSE, fillOpacity = 2, radius = 7, group = ~prop_addr_city,
                       popup = paste("<h3>",
                                     paste(df$prop_addr_num, df$prop_addr_pre,
                                           df$prop_addr_st_name, df$prop_addr_st_type,
                                           ifelse(is.na(df$prop_addr_unit), "",
                                                  df$prop_addr_unit)),
                                     df$prop_addr_city, df$prop_addr_zip, "</h3>",
                                     "<h4>", "Close Price:",  df$close_price, "<br>",
                                     "Beds:", df$beds, "<br>",
                                     "Baths:", df$baths, "<br>",
                                     "SqFt:", df$total_sqft, "<br>",
                                     "Year Built:", df$year_built, "<br>",
                                     "Year Sold:", df$year_sold, "<br>",
                                     "Property Type:", df$property_type))

        } else {


          leafletProxy("mymap") %>%
            clearShapes() %>%
            removeControl(layerId = "legend") %>%
            addLegend(position = "bottomright", pal = pal, values = map_var,
                      layerId = "legend", labels = labelFormat(between = "", big.mark = "")) %>%
            addProviderTiles(providers$CartoDB.Positron,
                             options = providerTileOptions(updateWhenIdle = FALSE)) %>%
            addDrawToolbar(position = "topleft",
                           editOptions = editToolbarOptions(remove = TRUE),
                           polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions(fill = FALSE, weight = 4)),
                           rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(fill = FALSE, weight = 4))) %>%
            addCircles(data = df, lng = ~longitude, lat = ~latitude, color= ~pal(map_var),
                       stroke = FALSE, fillOpacity = 2, radius = 7,
                       popup = paste("<h3>", 
                                     paste(df$prop_addr_num, df$prop_addr_pre,
                                           df$prop_addr_st_name, df$prop_addr_st_type,
                                           ifelse(is.na(df$prop_addr_unit), "",
                                                  df$prop_addr_unit)),
                                           df$prop_addr_city, df$prop_addr_zip, "</h3>",
                                    "<h4>", "Close Price:",  df$close_price, "<br>",
                                    "Beds:", df$beds, "<br>",
                                    "Baths:", df$baths, "<br>",
                                    "SqFt:", df$total_sqft, "<br>",
                                    "Year Built:", df$year_built, "<br>",
                                    "Year Sold:", df$year_sold, "<br>",
                                    "Property Type:", df$property_type)) 
                        
        }
    })
    
    
    
    reactive_df <- reactive({
            
            
            sold_year_min <- input$year_sold[1]
            sold_year_max <- input$year_sold[2]
            
            month_sold_min <- input$month_sold[1]
            month_sold_max <- input$month_sold[2]
            
            
            close_price_min <- input$close_price[1]
            close_price_max <- input$close_price[2]
            
            bed_min <- input$beds[1]
            bed_max <- input$beds[2]
            
            baths_min <- input$baths[1]
            baths_max <- input$baths[2]
            
            lot_size_sqft_min <- input$lot_size[1]
            lot_size_sqft_max <- input$lot_size[2]
            
            prop_size_sqft_min <- input$prop_size[1]
            prop_size_sqft_max <- input$prop_size[2]
            
            
            year_built_min <- input$year_built[1]
            year_built_max <- input$year_built[2]
            
            prop_type_list <- input$prop_type
            
            
            if (input$boundary == "zip") {
              
             denv_explor %>% as.data.table() %>%
                         filter(prop_addr_zip %in% input$zip &
                         year_sold >= sold_year_min &
                         year_sold <= sold_year_max &
                         close_price >= close_price_min &
                         close_price <= close_price_max &
                         beds >= bed_min &
                         beds <= bed_max &
                         baths >= baths_min &
                         baths <= baths_max &
                         lot_size_sqft >= lot_size_sqft_min &
                         lot_size_sqft <= lot_size_sqft_max &
                         year_built >= year_built_min &
                         year_built <= year_built_max &
                         total_sqft >= prop_size_sqft_min &
                         total_sqft <= prop_size_sqft_max &
                         property_type %in% prop_type_list)
                         
      
            
             
             
             
             
            } else if (input$boundary == "city") {
              
              df <- denv_explor %>% as.data.table() %>%
                filter(year_sold >= sold_year_min &
                         year_sold <= sold_year_max &
                         close_price >= close_price_min &
                         close_price <= close_price_max &
                         beds >= bed_min &
                         beds <= bed_max &
                         baths >= baths_min &
                         baths <= baths_max &
                         lot_size_sqft >= lot_size_sqft_min &
                         lot_size_sqft <= lot_size_sqft_max &
                         year_built >= year_built_min &
                         year_built <= year_built_max &
                         total_sqft >= prop_size_sqft_min &
                         total_sqft <= prop_size_sqft_max &
                         prop_addr_city %in% input$city) 
                         
              
             
            } else {
              
              reactive_bbox$bbox <- unlist(input$mymap_draw_new_feature)[-c(1:4)] %>%
                as.numeric() %>%
                matrix(ncol = 2, byrow = TRUE) %>%
                list() %>%
                st_polygon() %>%
                st_sfc() %>%
                st_set_crs(4326) %>%
                st_transform(26913)
              

              
              denv_explor %>% slice(which(denv_explor %>% 
                                            st_intersects(reactive_bbox$bbox, 
                                                          sparse = FALSE) == TRUE)) %>%
                filter(year_sold >= sold_year_min &
                         year_sold <= sold_year_max &
                         close_price >= close_price_min &
                         close_price <= close_price_max &
                         beds >= bed_min &
                         beds <= bed_max &
                         baths >= baths_min &
                         baths <= baths_max &
                         lot_size_sqft >= lot_size_sqft_min &
                         lot_size_sqft <= lot_size_sqft_max &
                         year_built >= year_built_min &
                         year_built <= year_built_max &
                         total_sqft >= prop_size_sqft_min &
                         total_sqft <= prop_size_sqft_max &
                         property_type %in% prop_type_list)
              
              
            }

          
    })
    
     # output$my_df <- renderTable({
     #       df <- reactive_df()
     # 
     #       st_geometry(df) <- NULL
     # 
     # 
     #       df %>% as.data.table() %>%
     #         select(architecture_style, construction_material, days_on_market,
     #                current_price, list_price, psf_total, close_to_original_list_price,
     #                year_built, lot_size_sqft, beds, baths, property_type, year_sold,
     #                property_condition, close_to_list_price) %>%
     #         as.data.table()
     # 
     # 
     # })
    
     
     

     output$graph_1v <- renderPlot({
      
         dat <- reactive_df()
         
         input_vars <- c(input$bar_color, input$bar_facet, input$density_color,
                         input$density_facet)
         
  
         input_test <- sapply(input_vars, grepl, "Nothing")
         
         names(input_test) <- c(input$bar_color, input$bar_facet, input$density_color,
                                input$density_facet)
         
         
         not_null_vecs <- which(input_test != TRUE)
         
         
         
         map_vars <- list(bar_color = NULL,
                          bar_facet = NULL,
                          density_color = NULL,
                          density_facet = NULL)
                          
         
         
         for (i in not_null_vecs) {
           
           map_vars[[i]] <- input_vars[i]
           
           
         }
          
      
        
         if (input$graph_type_1v == "density") {
             dat <- reactive_df() 
          
           if (input$x_axis_density == "close_price") {
             
           ggplot(dat) +
             geom_density(aes_string(input$x_axis_density, fill = map_vars$density_color), size = 1,
                          alpha = .5)  +
             geom_vline(xintercept = median(dat[[input$x_axis_density]], na.rm = TRUE),
                        color = "red", size = 1) +
             geom_vline(xintercept = mean(dat[[input$x_axis_density]], na.rm = TRUE),
                        color = "blue", size = 1) +
             facet_wrap(as.formula(paste(map_vars$density_facet, "~ ."))) +
             scale_x_continuous(labels = comma) +
             scale_y_continuous(labels = comma) 
           
           } else {
             
             ggplot(dat, aes_string(input$x_axis_density, fill = map_vars$density_color)) +
               geom_density(size = 1)  +
               geom_vline(xintercept = median(dat[[input$x_axis_density]], na.rm = TRUE),
                          color = "red", size = 1) +
               geom_vline(xintercept = mean(dat[[input$x_axis_density]], na.rm = TRUE),
                          color = "blue", size = 1) +
               scale_x_continuous(breaks = pretty_breaks()) +
               facet_wrap(as.formula(paste(map_vars$density_facet, "~ ."))) +
               scale_y_continuous() 
             
             
           } 
           
    
         } else if (input$graph_type_1v == "bar") {
            
            if (is.null(map_vars$bar_color)) {
           
            dat <- reactive_df() 
             
             
    
             ggplot(dat) +
               geom_bar(aes_string(x = input$x_axis_bar, fill = map_vars$bar_color)) +
               facet_wrap(as.formula(paste(map_vars$bar_facet, "~ .")))
            
            } else {
              
              dat <- reactive_df() %>%
                drop_na(map_vars$bar_color)
              
              
              
              ggplot(dat) +
                geom_bar(aes_string(x = input$x_axis_bar, fill = map_vars$bar_color)) +
                facet_wrap(as.formula(paste(map_vars$bar_facet, "~ .")))
              
            } 
              
               
         } else {
             dat <- reactive_df()
    
             if (input$x_axis_box == "close_price") {
               
               ggplot(dat) +
                 geom_boxplot(aes(x = "", y = !!sym(input$x_axis_box))) +
                 coord_flip()  +
                 scale_y_continuous(breaks = pretty_breaks(), labels = comma)
               
             } else {
             
             
             ggplot(dat) +
               geom_boxplot(aes(x = "", y = !!sym(input$x_axis_box))) +
               coord_flip()  +
               scale_x_discrete(breaks = pretty_breaks())
               
              }
            }
         })
     
     
     reactive_maps <- reactiveValues(scatter_color = NULL)
     
     output$graph_2v <- renderPlot({
         
              
           input_vars <- c(input$scatter_color, input$scatter_size,
                           input$scatter_shape, input$scatter_facet,
                           input$line_color, input$line_facet)
           
           
           input_test <- sapply(input_vars, grepl, "Nothing")
           
           names(input_test) <- c(input$scatter_color, input$scatter_size,
                                  input$scatter_shape, input$scatter_facet,
                                  input$line_color, input$line_facet)
           
       
           
           not_null_vecs <- which(input_test != TRUE)
           
    
           
           map_vars <- list(scatter_color = NULL,
                            scatter_size = NULL,
                            scatter_shape = NULL,
                            scatter_facet = NULL,
                            line_color = NULL,
                            line_facet = NULL)
           
           
           for (i in not_null_vecs) {
             
             map_vars[[i]] <- input_vars[i]
             
             
           }
           
           
              
    
             if (input$graph_type_2v == "scatter") {
                
                             
                  dat <- reactive_df() 
                  
                  
                     ggplot(dat) +
                        geom_point(aes_string(x = input$x_axis_2v_scatter, y = input$y_axis_2v_scatter,
                                       color = map_vars$scatter_color, size = map_vars$scatter_size,
                                       shape = map_vars$scatter_shape)) +
                        scale_y_continuous(labels = comma) +
                        facet_wrap(as.formula(paste(map_vars$scatter_facet, "~ .")))
      
               
                  
                  
                
    
            } else if (input$graph_type_2v == "box_plot") {
                 
                dat <- reactive_df()
                     ggplot(dat) +
                        geom_boxplot(aes(x = factor(!!sym(input$x_axis_2v_box)), y = !!sym(input$y_axis_2v_box))) +
                        scale_y_continuous(labels = comma, breaks = pretty_breaks())
    
             } else if (input$graph_type_2v == "line") {
                 dat <- reactive_df()
                     dat <- dat %>%
                     group_by(!!sym(input$x_axis_2v_line)) %>%
                     mutate(median_var = median(!!sym(input$y_axis_2v_line), na.rm = TRUE))
    
                 ggplot(dat) +
                   geom_line(aes_string(x = input$x_axis_2v_line, y = "median_var",
                                          color = map_vars$line_color)) +
                   facet_wrap(as.formula(paste(map_vars$line_facet, "~ .")))
                 
             }
         })
     
     output$num_sold <- renderText({
       
       nrow(reactive_df())
       
     })
     
     output$med_close_price <- renderText({
       
       median(reactive_df()[["close_price"]], na.rm = TRUE)
       
     })
     
     output$mean_close_price <- renderText({
       
       mean(reactive_df()[["close_price"]], na.rm = TRUE)
       
     })
    
})



