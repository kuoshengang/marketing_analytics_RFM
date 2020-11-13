
#************************************************#
#   Student Name: ANG KUO SHENG CLEMENT          #  
#   Student Matriculation: A0177726R             #      
#************************************************#

# install.packages("flexdashboard")
# install.packages('shinydashboard')
# library(stats)
library(shiny)
library(flexdashboard)
library(shinydashboard)
library(DT)
library(RColorBrewer)
library(shinyWidgets)
library(ggplot2)
library(scales)
library(formattable)
library(lubridate)
library(rsconnect)
library(tidyverse)
library(dygraphs)
library(googleVis)
library(plotly)
library(rsconnect)
library(dplyr)
library(knitr)
library(kableExtra)
library(magrittr)
library(rfm)
library(shinythemes)


# packages = c('shiny','shinydashboard','tidyverse','dplyr', 'magrittr', 'ggplot2', 'scales', 'DT','lubridate','scales','dplyr')
# 
# for (p in packages){
#     if (!require(p,character.only = T)){
#         install.packages(p)
#     }
#     library(p,character.only = T)
# }

# setwd("D:/SCALE NUS BTech Analytics/TBA3103 - Application Systems Development for Business Analytics/Olist_RShiny/")


#Read raw data from csv file, set stringsAsFactors = F


cust <- read.csv("data/olist_customers_dataset.csv" , stringsAsFactors = F)
order_items <- read.csv("data/olist_order_items_dataset.csv", stringsAsFactors = F )
reviews <- read.csv("data/olist_order_reviews_dataset.csv", stringsAsFactors = F)
order_payment <- read.csv("data/olist_order_payments_dataset.csv" , stringsAsFactors = F )
orders <- read.csv("data/olist_orders_dataset.csv" , stringsAsFactors = F )
products <- read.csv("data/olist_products_dataset.csv" , stringsAsFactors = F )
product_category_english_name <- read.csv("data/product_category_name_translation.csv" , stringsAsFactors = F)
colnames(product_category_english_name)[1]<-"product_category_name"


# merging to create combined dataset by key field

transaction_data <- merge(x = order_items, y = order_payment, by = "order_id", all = TRUE)
transaction_data <- merge(x = transaction_data, y = orders, by = "order_id", all = TRUE)
transaction_data <- merge(x = transaction_data, y = reviews, by = "order_id", all = TRUE)
transaction_data <- merge(x = transaction_data, y = cust, by = "customer_id", all = TRUE)
transaction_data <- merge(x = transaction_data, y = products, by = "product_id", all = TRUE)
transaction_data <- merge(x = transaction_data, y = product_category_english_name, by = "product_category_name", all = TRUE)


# remove NA fields
 
transaction_data_filtered <- na.omit(transaction_data)

# Cleaning Olist  Data
transaction_data_filtered = transaction_data_filtered %>%
    filter(order_status != "canceled" & payment_value > 0) %>%
    rename(
        purchase_date = order_purchase_timestamp,
        delivered_date = order_delivered_customer_date,
        est_delivered_date = order_estimated_delivery_date,
        zip_code = customer_zip_code_prefix,
        city = customer_city,
        state = customer_state,
        product_category = product_category_name_english
    ) %>%
    mutate(
        purchase_date = lubridate::as_datetime(purchase_date),
        delivered_date = lubridate::as_datetime(delivered_date),
        est_delivered_date = lubridate::as_datetime(est_delivered_date)
    ) %>%
    distinct() %>%
    mutate(
        purchase_date = lubridate::date(purchase_date),
        delivered_date = lubridate::date(delivered_date),
        est_delivered_date = lubridate::date(est_delivered_date)
    ) %>%
    mutate(delivery_days = delivered_date - purchase_date,
           diff_estdel = est_delivered_date - delivered_date) %>%
    mutate(
        product_category = case_when(
            product_category == "home_appliances_2" ~ "home_appliances",
            product_category == "home_comfort_2" ~ "home_comfort",
            product_category == "home_confort" ~ "home_comfort",
            product_category == "fashio_female_clothing" ~ "fashion_female_clothing",
            product_category == "art" ~ "arts_and_craftmanship",
            product_category == "drinks" |
                product_category == "food" ~ "food_drink",
            TRUE ~ as.character(product_category)
        )
    )

# Generating product sub-category
transaction_data_filtered = transaction_data_filtered %>% 
    mutate(product_category = case_when(product_category %in% c("health_beauty", "perfumery", "diapers_and_hygiene") ~ "Chemists.Drugstores", 
                                        product_category %in% c("watches_gifts", "fashion_bags_accessories", "luggage_accessories", "fashion_shoes", 
                                                                "fashion_male_clothing", "fashion_female_clothing", "fashion_childrens_clothes", 
                                                                "fashion_underwear_beach") ~ "Clothing",
                                        product_category %in% c("cool_stuff", "construction_tools_construction", "home_construction", "construction_tools_lights", 
                                                                "construction_tools_safety", "arts_and_craftmanship", "costruction_tools_tools") ~ "DIY Goods",
                                        product_category %in% c("computers_accessories", "telephony", "computers", "electronics", "consoles_games", 
                                                                "fixed_telephony", "air_conditioning", "tablets_printing_image", "cine_photo") ~ "Electrical Goods",
                                        product_category %in% c("food_drink", "la_cuisine", "flowers") ~ "Food.Consumables", 
                                        product_category %in% c("furniture_decor", "office_furniture", "furniture_living_room", "home_comfort", 
                                                                "kitchen_dining_laundry_garden_furniture", "small_appliances_home_oven_and_coffee", 
                                                                "furniture_bedroom", "furniture_mattress_and_upholstery") ~ "Furniture.Carpets",
                                        product_category %in% c("garden_tools", "costruction_tools_garden") ~ "Garden Products", 
                                        product_category %in% c("bed_bath_table", "housewares", "auto", "toys", "baby", "stationery", "pet_shop",
                                                                "pet_shop", "home_appliances", "small_appliances", "party_supplies", "christmas_supplies") ~ "Household.Textiles", 
                                        product_category %in% c("musical_instruments", "audio", "cds_dvds_musicals", "dvds_blu_ray", "music", "books_imported", 
                                                                "books_technical", "books_general_interest") ~ "Music, Films.Books",
                                        product_category %in% c("sports_leisure", "fashion_sport") ~ "Sports Equipments", 
                                        product_category %in% c("market_place", "agro_industry_and_commerce", "industry_commerce_and_business", "signaling_and_security",
                                                                "security_and_services") ~ "Services", 
                                        TRUE ~ "Other")) %>%
    rename(product_sub_category = product_category)




# Generating analysis by time

# Generating analysis by time per product sub-category
sales_per_product_category_transaction_date <- transaction_data_filtered %>%
                                        filter(is.na(purchase_date) == F) %>%
                                        mutate(purchase_date = as.Date(purchase_date)) %>%
                                        group_by(product_sub_category, purchase_date) %>%
                                        summarise(sales_by_day = sum(payment_value))


# Categorical_sales_by_time <- transaction_data_filtered %>%
#                                             rename(sales = sales_by_day) %>%
#                                             mutate(product_category = "Total Sales") %>%
#                                             select(purchase_date, product_sub_category, sales)


catvalue_choices = list(
    # "Total Sales" = names(Categorical_sales_by_time)[[3]]
    "Total Sales" = sales_per_product_category_transaction_date$sales_by_day
)


# Sales by State / Region (Geographically)

state_sales_by_month <- transaction_data_filtered %>% 
    group_by(year_month=floor_date(purchase_date, "month"), state) %>%
                     summarize(sales_by_month=sum(payment_value))
 

geo_state_sales = list(
    "State" = names(state_sales_by_month)[[1]],
    "Year-Month" = names(state_sales_by_month)[[2]],
    "Total_Sales_by_State" = names(state_sales_by_month)[[3]]
    
)


sub_category = sort(unique(transaction_data_filtered$product_sub_category)) 

geo_state_loc = sort(unique(transaction_data_filtered$state)) 

total_sales_volume = sum(transaction_data_filtered$payment_value)


# RFM_Table <-transaction_data_filtered %>% 
#   group_by(customer_id, order_id, purchase_date) %>% 
#   summarise(recency_days = as.numeric(RFM_analysis_date - max(purchase_date)),
#             total_sales_revenue = sum(payment_value), frequency = n_distinct(purchase_date))



# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


intro_str =  "Marketing has always been an industry that is heavily data related. Big firms spent millions of dollars every year on analysising their marketing data, in terms of 
finding insights and make their marketing investment wisely. Because of my marketing background, discovering insights from a marketing dataset 
always pique my interest, so this is designed to understand the E-commerce Store from multiple sales & marketing perspectives. This 
shiny app has three major sections. With geographic section, you could visualize data from a geographic standpoint and see some correlations. The trends section is the part to show
sales trends in the particular time range being selected, you could also comparing sales volume between different product categories. "

intrdata_str = "The dataset is largest department store in Brazilian marketplaces, which includes over 100k orders from late 2016 to 2018.
It allows any users of this application to view their business sales performance from mutiple dimensions like order status, payments information, geographic location and so on before performing RFM analysis for each customer segment groups."

RFM_intro = "RFM (recency, frequency, monetary) analysis is a behavior based technique used to segment customers by examining their transaction history recency, frequency and monetary.
It is based on the marketing axiom that 80% of your business comes from 20% of your customers. RFM helps to identify customers who are more likely to respond to promotions by segmenting them into various categories."

ui <- fluidPage(theme = "style.css",
                shinyUI(
                    dashboardPage(
                        skin = "yellow",
                        dashboardHeader(title = "SGUnited_MarketingAnalytics", titleWidth = 290),
                        dashboardSidebar(sidebarMenu(
                          
                          tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
                          
                          # The dynamically-generated user panel
                          uiOutput("userpanel"),
                        
                            menuItem(
                                "Introduction",
                                tabName = "intro",
                                icon = icon("align-justify")
                            ),
                            menuItem("Sales Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
                            menuItem("RFM Analysis", tabName = "RFM", icon = icon("map")),
                            menuItem("Order Status Bar Chart", tabName = "Order_Status_Bar", icon = icon("bar-chart")),
                            menuItem("Product Trends", tabName = "Product_Trend", icon = icon("line-chart")),
                            menuItem(
                                "Categories",
                                tabName = "category",
                                icon = icon("dashboard")
                            ),
                            sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                              label = "Search...")
                        )),
                  
                        dashboardBody(
                            tags$style(type = "text/css", "#geo {height: calc(100vh - 80px) !important;}"),
                            tabItems(
                                tabItem(tabName = "intro",
                                        fluidRow(column(
                                            width = 12,
                                            box(
                                                title = "Marketing Analytics - Leading you to Customer Segmentation",
                                                solidHeader = T,
                                                width = NULL,
                                                status = "info",
                                                id = "intro",
                                                tags$h1("About Application Features"),
                                                tags$h3(
                                                    "Half the money I spend on advertising is wasted; the trouble is, I don't know which half' -- John Wanamaker (1838-1922)"
                                                    
                                                    
                                                ),
                                                tags$h4(intro_str),
                                                tags$h2("The Dataset"),
                                                tags$h4(intrdata_str),
                                                tags$img(src = "/image/images_rfm.png"),
                        
                                                #     width = 1100,
                                                #     height = 660
                                                # ),
                                               tags$h4(RFM_intro),
                                            )
                                            
                                        ))),
                                tabItem(tabName = "Dashboard",
                                        fluidRow(
                                          shinydashboard::valueBoxOutput("GMV_fig", width=4),
                                          shinydashboard::valueBoxOutput("AOV_fig", width=4),
                                          shinydashboard::valueBoxOutput("A30_user", width=4)
                                        ),
                                        # fluidRow(
                                        #     box(
                                        #     title = "Gross Merchandise Value (GMV)", background = "black", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                                        #     plotlyOutput("GMV_Plot", height = 200))
                                        # ),
                                        # fluidRow(
                                        #   box(
                                        #     title = "GMV by Categories", background = "light-blue", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
                                        #     column(12, (DT::dataTableOutput("topCat")))
                                        #     ),
                                        #   box(
                                        #     title = "Days to Delivery", background = "orange", solidHeader = TRUE, width = 6, collapsible = TRUE, collapsed = TRUE,
                                        #     plotlyOutput("deliveryTime")
                                        #   )
                                        # ),
                                        fluidRow(
                                            column(
                                                width = 8,
                                                box(
                                                    title = "Top 10 Product Categories by Sales ($)",
                                                    solidHeader = T,
                                                    status = "info",
                                                    plotOutput(outputId = "top10_sales_product_cat"),
                                                    width = NULL,
                                                    height = "auto"
                                                ),
                                                box(
                                                  title = "Sales Percent (%) by Product Category",
                                                  solidHeader = T,
                                                  status = "info",
                                                  plotOutput(outputId = "top10_sales_product_cat_by_percent"),
                                                  width = NULL,
                                                  height = "auto"
                                                )
                                                # box(
                                                #   title = "RFM analysis",
                                                #   solidHeader = T,
                                                #   status = "info",
                                                #   plotOutput(outputId = "RFM_Chart"),
                                                #   width = NULL,
                                                #   height = "auto"
                                                # )
                                            ),
                                            column(
                                                width = 4,
                                                box(
                                                    title = "Select States",
                                                    solidHeader = T,
                                                    width = NULL,
                                                    status = "info",
                                                    selectInput(inputId = "state_geo", label = NULL, 
                                                                choices = geo_state_loc,
                                                                selected = geo_state_loc[4],
                                                                multiple = TRUE,
                                                      
                                                    )
                                                   
                                                ),
                                                # box(
                                                #     title = "Top n Product Categories Slider",
                                                #     solidHeader = T,
                                                #     width = NULL,
                                                #     status = "info",
                                                #     sliderInput("top_n_slider", 
                                                #                 label = "Top n product Categories:",
                                                #                 min = 5, max = 20, value = c(5,20), step=5
                                                #       ),
                                                # ),
                                                box(    
                                                    title = "Date Range Input",
                                                    solidHeader = T,
                                                    width = NULL,
                                                    status = "info",
                                                    dateRangeInput(
                                                      "datein",
                                                      label = NULL,
                                                      start = head(transaction_data_filtered$purchase_date, 1),
                                                      end = tail(transaction_data_filtered$purchase_date, 1),
                                                      min = head(transaction_data_filtered$purchase_date, 1),
                                                      max = tail(transaction_data_filtered$purchase_date, 1)
                                                    )
                                                    
                                                )
                                                # ,
                                                # box(
                                                #     title = "Data",
                                                #     solidHeader = T,
                                                #     collapsible = T,
                                                #     width = NULL,
                                                #     status = "info",
                                                #     tableOutput({
                                                #         "table"
                                                #     })
                                                # )
                                            )
                                        )),
                                tabItem(tabName = "RFM",
                                        mainPanel(
                                          tabsetPanel(
                                            id = 'dataset',
                                            tabPanel("RFM Charts", 
                                               fluidRow(
                                                 column(
                                                   width = 8,
                                                   box(
                                                     title = "Customer Segmentation - RFM",
                                                     solidHeader = T,
                                                     status = "info",
                                                     plotOutput(outputId = "RFM_Bar_Chart"),
                                                     width = NULL,
                                                     height = "auto"
                                                   )
                                                 ),
                                                 column(
                                                   width = 4,
                                                   box(    
                                                     title = "RFM Analyis Date",
                                                     solidHeader = T,
                                                     width = NULL,
                                                     status = "info",
                                                     selectInput(
                                                       "RFM_analysis_date",
                                                       label = "Date:",
                                                       choices =  c("2018-12-30","2018-11-30", "2018-10-30","2018-09-30","2018-08-30","2018-07-30","2018-06-30","2018-05-30","2018-04-30","2018-03-30", "2018-02-28", "2018-01-30", "2017-12-30"),
                                                       selectize = TRUE,
                                                       multiple = FALSE
                                                       
                                                     )
                                                   )
                                                 ))
                                                   
                                            ),
                                            tabPanel("RFM DataTable", 
                                               fluidRow(
                                                    column(
                                                      width = 3,
                                                      box(    
                                                        title = "RFM Analyis Date",
                                                        solidHeader = T,
                                                        width = NULL,
                                                        style="width:100%",
                                                        collapsible = T,
                                                        status = "info",
                                                        selectInput(
                                                          "RFM_Segment_analysis_time",
                                                          label = "Date:",
                                                          choices =  c("2018-12-30","2018-11-30", "2018-10-30", "2018-09-30","2018-08-30","2018-07-30","2018-06-30","2018-05-30","2018-04-30","2018-03-30","2018-02-28","2018-01-30", "2017-12-30"),
                                                          selectize = TRUE,
                                                          multiple = FALSE
                                                          
                                                        )
                                                      )
                                                    )),
                                               fluidRow(
                                                 column(
                                                   width = 12,
                                                   box(
                                                     title = "RFM Table",
                                                     solidHeader = T,
                                                     collapsible = T,
                                                     style="width:100%",
                                                     width = 12,
                                                     status = "info",
                                                     DT::dataTableOutput("RFM_Segment_Table")
                                                     
                                                   )
                                                 ))
                                                  
                                               ) # end of tabPanel
                                          ) # end of tabset panel
                                       )),

                                tabItem(tabName = "Product_Trend",
                                        fluidRow(
                                            column(
                                                width = 8,
                                                box(
                                                    title = "Product Sales Trends",
                                                    solidHeader = T,
                                                    width = NULL,
                                                    height = 580,
                                                    status = "info",
                                                    plotOutput(outputId = "product_cat_trend")
                                                )
                                            ),
                                            column(
                                                width = 4,
                                                box(
                                                    title = "Date Range Input",
                                                    solidHeader = T,
                                                    width = NULL,
                                                    status = "info",
                                                    dateRangeInput(
                                                        "purchase_date_in",
                                                        label = NULL,
                                                        start = head(transaction_data_filtered$purchase_date, 1),
                                                        end = tail(transaction_data_filtered$purchase_date, 1),
                                                        min = head(transaction_data_filtered$purchase_date, 1),
                                                        max = tail(transaction_data_filtered$purchase_date, 1)
                                                    )
                                                )
                                            ),
                                           
                                            column(
                                                width = 4,
                                                box(
                                                    title = "Category Choices",
                                                    solidHeader = T,
                                                    width = NULL,
                                                    status = "info",
                                                    pickerInput(
                                                        inputId = "trend_subcategory",
                                                        choices = sub_category,
                                                        selected = sub_category[12],
                                                        options = list(`actions-box` = TRUE),
                                                        multiple = TRUE
                                                    )
                                                )),
                                            column(
                                                width = 8,
                                                box(
                                                    title = "State Sales Trends",
                                                    solidHeader = T,
                                                    width = NULL,
                                                    height = 420,
                                                    status = "info",
                                                    plotOutput(outputId = "state_trend")
                                                )
                                            ),
                                            column(
                                                width=4,
                                                box(
                                                    title = "Geo State",
                                                    solidHeader = T,
                                                    width = NULL,
                                                    status = "warning",
                                                    selectInput(
                                                        inputId = "state_loc", label = NULL, 
                                                        choices = geo_state_loc,
                                                        selected = geo_state_loc[4],
                                                        multiple = TRUE,
                                                    )
                                                ),
                                                box(
                                                    title = "Product Category Sales ",
                                                    solidHeader = T,
                                                    collapsible = T,
                                                    width = NULL,
                                                    status = "warning",
                                                    DT::dataTableOutput("sales_cat_table")
                                                        
                                                )
                                            )
                                           
                                        )),
                                tabItem(tabName = "Order_Status_Bar",
                                        fluidRow(
                                            column(
                                                width = 9,
                                                box(
                                                    title = "Order Status Bar Chart",
                                                    solidHeader = T,
                                                    width = NULL,
                                                    height = 1000,
                                                    status = "info",
                                                    plotOutput(outputId = "order_status_chart")
                                                )
                                            ),
                                            column(
                                                width = 3,
                                                box(
                                                  
                                                  title = "Date Range Input",
                                                  solidHeader = T,
                                                  width = NULL,
                                                  status = "info",
                                                  dateRangeInput(
                                                    "order_status_date_in",
                                                    label = NULL,
                                                    start = head(transaction_data_filtered$purchase_date, 1),
                                                    end = tail(transaction_data_filtered$purchase_date, 1),
                                                    min = head(transaction_data_filtered$purchase_date, 1),
                                                    max = tail(transaction_data_filtered$purchase_date, 1)
                                                    
                                                    # title = "Variables Input",
                                                    # solidHeader = T,
                                                    # width = NULL,
                                                    # status = "info",
                                                    # selectInput(
                                                    #     "cat_sales_value",
                                                    #     label = NULL,
                                                    #     choices = catvalue_choices,
                                                    #     selected = "total_sales"
                                                    )
                                                )
                                            ),
                                            column(
                                                width = 3,
                                                box(
                                                    title = "Product Categories Order Status ",
                                                    solidHeader = T,
                                                    width = NULL,
                                                    status = "info",
                                                    pickerInput(
                                                        inputId = "order_status_prod_category",
                                                        label = NULL,
                                                        choices = sub_category,
                                                        selected = sub_category[3],
                                                        options = list(`actions-box` = TRUE),
                                                        multiple = TRUE
                                                    )
                                                ),
                                                box(
                                                    title = "Geo State",
                                                    solidHeader = T,
                                                    width = NULL,
                                                    status = "warning",
                                                    selectInput(
                                                      inputId = "order_status_geostate_loc", label = NULL, 
                                                      choices = geo_state_loc,
                                                      selected = geo_state_loc[4],
                                                      multiple = TRUE,
                                                    )
                                                )
                                            )
                                        ))
                            )
                        )
                    )
                ))

server <- function(input, output, session) {

  
      output$userpanel <- renderUI({
        # session$user is non-NULL only in authenticated sessions
        if (!is.null(session$user)) {
          sidebarUserPanel(
            span("Logged in as ", session$user),
            subtitle = a(icon("sign-out"), "Logout", href="__logout__"))
        }
      })
    
    
# For Dashboard Display
    
    delivered_transaction_df <- transaction_data_filtered %>%
         filter(order_status == "delivered")
    
    
    output$GMV_fig <- shinydashboard::renderValueBox({
      deliveredToDate <- delivered_transaction_df %>%
        filter(delivered_date <= input$datein[2] & delivered_date >= input$datein[1])
      shinydashboard::valueBox(
        paste0("$", prettyNum(ceiling(sum(deliveredToDate$price)/1000), big.mark = ",")),
        "Total GMV ('000s)",
        color = "yellow",
        icon = icon("dollar-sign"))
    })
    
    output$AOV_fig <- shinydashboard::renderValueBox({  ## output for AOV
      deliveredToDate <- delivered_transaction_df %>%
        filter(delivered_date <= input$datein[2] & delivered_date >= input$datein[1]) ## filter for date range
      shinydashboard::valueBox(
        paste0("$", round(sum(deliveredToDate$price)/length(unique(deliveredToDate$order_id)),digits = 2)),
        "Average Order Value",
        color = "olive",
        icon = icon("shopping-basket")
      )
    })
    
    output$A30_user <- shinydashboard::renderValueBox({  ## output for A30
      UserToDate <- transaction_data_filtered %>%
        filter(purchase_date <= input$datein[2] & purchase_date >= (input$datein[2] - 30)) ## filter for date range
      shinydashboard::valueBox(
        prettyNum(length(unique(UserToDate$customer_id)),big.mark = ","),
        "No. Users within Time Range",
        color = "blue",
        icon = icon("user-alt")
      )
      
    })
    

    RFM_df = reactive({
      
      # RFM Analysis
      RFM_analysis_date<-lubridate::as_date(input$RFM_analysis_date)
      
      RFM_result<-delivered_transaction_df %>% 
        group_by(customer_id, order_id, purchase_date) %>% 
        summarise(recency_days = as.numeric(RFM_analysis_date - max(purchase_date)),
                  total_sales_revenue = sum(payment_value), frequency = n_distinct(purchase_date))
      
      # RFM_monetary_quantile<-quantile(RFM_result$total_sales_revenue, probs = c(0.25, 0.50, 0.75,1))
      # RFM_recency_quantile<-quantile(RFM_result$recency_days, probs = c(0.25, 0.50, 0.75,1))
      # RFM_frequency_quantile<-quantile(RFM_result$frequency, probs = c(0.25, 0.50, 0.75,1))
      
      # print(RFM_monetary_quantile)
      # print(RFM_frequency_quantile)
      # print(RFM_recency_quantile)
      
      RFM_result$recency_score<-NA
      RFM_result$frequency_score<-NA
      RFM_result$monetary_score<-NA
      
      #R_score
      RFM_result$recency_score[RFM_result$recency_days>469]<-1
      RFM_result$recency_score[RFM_result$recency_days>345 & RFM_result$recency_days<=470]<-2
      RFM_result$recency_score[RFM_result$recency_days>239 & RFM_result$recency_days<=345 ]<-3
      RFM_result$recency_score[RFM_result$recency_days<=239]<-4
      #F_score
      RFM_result$frequency_score[RFM_result$frequency<1]<-1
      RFM_result$frequency_score[RFM_result$frequency>=1 & RFM_result$frequency<2]<-2
      RFM_result$frequency_score[RFM_result$frequency>=2 & RFM_result$frequency<3]<-3
      RFM_result$frequency_score[RFM_result$frequency>=4]<-4
      #M_score
      RFM_result$monetary_score[RFM_result$total_sales_revenue<= 363.89]<-1 
      RFM_result$monetary_score[RFM_result$total_sales_revenue>=363.89 & RFM_result$total_sales_revenue<250.08]<-2 
      RFM_result$monetary_score[RFM_result$total_sales_revenue>=250.08 & RFM_result$total_sales_revenue<1903.08]<-3
      RFM_result$monetary_score[RFM_result$total_sales_revenue>=1903.08]<-4
      
      #RFM_score
      RFM_result<-na.omit(RFM_result %>% mutate(RFM_Score = 100*recency_score + 10*frequency_score + monetary_score))
      
      #Customer Segmentation
      RFM_result$segmentRFM<-NA
      champions <- c(444)
      loyal_customers <- c(334, 342, 343, 344, 433, 434, 443)
      potential_loyalist <- c(332,333,341,412,413,414,431,432,441,442,421,422,423,424)
      recent_customers <- c(411)
      promising <- c(311, 312, 313, 331)
      needing_attention <- c(212,213,214,231,232,233,241,314,321,322,323,324)
      about_to_sleep <- c(211)
      at_risk <- c(112,113,114,131,132,133,142,124,123,122,121,224,223,222,221)
      cant_lose <- c(134,143,144,234,242,243,244)
      hibernating <- c(141)
      lost <- c(111)
      
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% champions)] = "Champions"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% loyal_customers)] = "Loyal Customers"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% potential_loyalist)] = "Potential Loyalist"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% recent_customers)] = "Recent customers"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% promising)] = "Promising"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% needing_attention)] = "Customer Needing Attention"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% about_to_sleep)] = "About to Sleep"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% at_risk)] = "At Risk"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% cant_lose)] = "Can't Lose Them"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% hibernating)] = "Hibernating"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% lost)] = "Lost"
      
      # print(RFM_result$segmentRFM)
      
      RFM_result%>%
        count(segmentRFM) %>%
             arrange(desc(n))%>%
        rename(segmentRFM = segmentRFM, Count = n)
      
    })
    
    output$RFM_Bar_Chart<-renderPlot({
      RFM_df = RFM_df()  # important function
      ggplot(data = RFM_df) +
        aes(x=segmentRFM, fill=segmentRFM) +
        geom_bar() + labs(title = "Customer Segmentation", x = "Segment", y = "Total No. of Customer(S)") +
          coord_flip()+
            theme_minimal()
      
    }, height = "auto")
    
    
    RFM_Segment_result_df = reactive({
      
      # RFM Analysis
      RFM_analysis_date<-lubridate::as_date(input$RFM_Segment_analysis_time)
      
      RFM_result<-delivered_transaction_df %>% 
        group_by(customer_id, order_id, purchase_date) %>% 
        summarise(recency_days = as.numeric(RFM_analysis_date - max(purchase_date)),
                  total_sales_revenue = sum(payment_value), frequency = n_distinct(purchase_date))
      
      # RFM_monetary_quantile<-quantile(RFM_result$total_sales_revenue, probs = c(0.25, 0.50, 0.75,1))
      # RFM_recency_quantile<-quantile(RFM_result$recency_days, probs = c(0.25, 0.50, 0.75,1))
      # RFM_frequency_quantile<-quantile(RFM_result$frequency, probs = c(0.25, 0.50, 0.75,1))
      
      # print(RFM_monetary_quantile)
      # print(RFM_frequency_quantile)
      # print(RFM_recency_quantile)
      
      RFM_result$recency_score<-NA
      RFM_result$frequency_score<-NA
      RFM_result$monetary_score<-NA
      
      #R_score
      RFM_result$recency_score[RFM_result$recency_days>469]<-1
      RFM_result$recency_score[RFM_result$recency_days>345 & RFM_result$recency_days<=470]<-2
      RFM_result$recency_score[RFM_result$recency_days>239 & RFM_result$recency_days<=345 ]<-3
      RFM_result$recency_score[RFM_result$recency_days<=239]<-4
      #F_score
      RFM_result$frequency_score[RFM_result$frequency<1]<-1
      RFM_result$frequency_score[RFM_result$frequency>=1 & RFM_result$frequency<2]<-2
      RFM_result$frequency_score[RFM_result$frequency>=2 & RFM_result$frequency<3]<-3
      RFM_result$frequency_score[RFM_result$frequency>=4]<-4
      #M_score
      RFM_result$monetary_score[RFM_result$total_sales_revenue<= 363.89]<-1 
      RFM_result$monetary_score[RFM_result$total_sales_revenue>=363.89 & RFM_result$total_sales_revenue<250.08]<-2 
      RFM_result$monetary_score[RFM_result$total_sales_revenue>=250.08 & RFM_result$total_sales_revenue<1903.08]<-3
      RFM_result$monetary_score[RFM_result$total_sales_revenue>=1903.08]<-4
      
      #RFM_score
      RFM_result<-na.omit(RFM_result %>% mutate(RFM_Score = 100*recency_score + 10*frequency_score + monetary_score))
      
      #Customer Segmentation
      RFM_result$segmentRFM<-NA
      champions <- c(444)
      loyal_customers <- c(334, 342, 343, 344, 433, 434, 443)
      potential_loyalist <- c(332,333,341,412,413,414,431,432,441,442,421,422,423,424)
      recent_customers <- c(411)
      promising <- c(311, 312, 313, 331)
      needing_attention <- c(212,213,214,231,232,233,241,314,321,322,323,324)
      about_to_sleep <- c(211)
      at_risk <- c(112,113,114,131,132,133,142,124,123,122,121,224,223,222,221)
      cant_lose <- c(134,143,144,234,242,243,244)
      hibernating <- c(141)
      lost <- c(111)
      
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% champions)] = "Champions"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% loyal_customers)] = "Loyal Customers"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% potential_loyalist)] = "Potential Loyalist"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% recent_customers)] = "Recent customers"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% promising)] = "Promising"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% needing_attention)] = "Customer Needing Attention"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% about_to_sleep)] = "About to Sleep"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% at_risk)] = "At Risk"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% cant_lose)] = "Can't Lose Them"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% hibernating)] = "Hibernating"
      RFM_result$segmentRFM[which(RFM_result$RFM_Score %in% lost)] = "Lost"
      
      RFM_result
      
   
    })
    

    output$RFM_Segment_Table <- DT::renderDataTable(
    
      # DT::datatable(RFM_Segment_result_df())
      RFM_Segment_result_df(), options = list(autoWidth = TRUE)
      # options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
              # rownames = FALSE, ##remove index
              # options = list(lengthMenu = c(5,10), pageLength =5)

  )

    # Reactive Data For Bar Chart and Table
    # Generating analysis by time per product sub-category
    # mutate(purchase_date = as.Date(purchase_date)) %>%
    sales_product_cat_mthly_bar = reactive({

                transaction_data_filtered %>%
                    filter(purchase_date >= input$datein[1] &
                             purchase_date <= input$datein[2]) %>%
                    filter(product_sub_category %in% input$trend_subcategory)  %>%
                    group_by(sales_year_month=floor_date(purchase_date, "month"), product_sub_category) %>%
                    summarise(sales_by_month = sum(payment_value)) %>% 
                    arrange(sales_year_month, product_sub_category) -> product_category_sales
                    product_category_sales<-product_category_sales[1:20000,]
                    product_category_sales %>% 
                                    ungroup() %>% 
                                    mutate(sales_year_month = lubridate::date(sales_year_month))
                    # select(category, value = input$cat_sales_value) %>%
                    # arrange(desc(sales_by_day))
                
    })
    
    # Product Categories Bar Chart Time Trend
    
    output$product_cat_trend<-renderPlot({
        sales_product_cat_mthly_bar = sales_product_cat_mthly_bar()
        ggplot(data = sales_product_cat_mthly_bar, aes(x=sales_year_month, y=sales_by_month, fill=product_sub_category )) + 
            geom_bar(stat="identity", position="stack") +
            scale_x_date(date_breaks = '1 month', labels = date_format("%y-%m")) +
            scale_y_continuous(labels = scales::comma) +
            geom_text(aes(label = scales::comma(sales_by_month)), size = 3, check_overlap = TRUE, position = position_stack(vjust = 0.5)) + 
            labs(title="Product Category Sales over time horizon ($)", x="Year-Month", y= "Total Sales per Month")
        
    }, height = 500)
    
    
    # Sales by State Trend Chart
    state_sales_by_month = reactive({
      delivered_transaction_df %>% 
        filter(purchase_date >= input$datein[1] &
                   purchase_date <= input$datein[2]) %>% 
                   filter(state %in% input$state_loc)  %>%
        group_by(year_month=floor_date(purchase_date, "month"), state) %>%
        summarise(sales_by_month=sum(payment_value)) %>%
        arrange(year_month, state) -> state_sales
        state_sales <- state_sales[1:20000,]
        state_sales %>%
                ungroup() %>%
                mutate(year_month = lubridate::date(year_month))
            
    })

    output$state_trend<-renderPlot({
        state_sales_by_month = state_sales_by_month()
        ggplot(data = state_sales_by_month, aes(x=year_month, y=sales_by_month, fill=state)) + 
            geom_bar(stat="identity", position="stack") +
            scale_x_date(date_breaks = '1 month', labels = date_format("%y-%m")) +
            scale_y_continuous(labels = scales::comma) +
            geom_text(aes(label = scales::comma(sales_by_month)), size = 3, check_overlap = TRUE, position = position_stack(vjust = 0.5)) + 
            labs(title="State Sales over time horizon ($)", x="Year-Month", y= "Total Sales per Month")
        
    }, height = 340)
    
    output$sales_cat_table <- DT::renderDataTable(

      delivered_transaction_df %>%
            filter(purchase_date >= input$datein[1] &
                       purchase_date <= input$datein[2]) %>%
            filter(product_sub_category %in% input$trend_subcategory)  %>%
            group_by(sales_year_month=floor_date(purchase_date, "month"),product_sub_category) %>%
            summarize("sales_by_month" = sum(payment_value)),

        rownames = FALSE, ##remove index
        options = list(lengthMenu = c(5,10), pageLength =5)

    )
    
    
    # Top 10 Customers Sales by State Trend Chart
    
    top10sales_summ_by_product_category = reactive({
      transaction_data_filtered %>% 
        filter(purchase_date >= input$purchase_date_in[1] &
                 purchase_date <= input$purchase_date_in[2]) %>% 
        filter(state %in% input$state_geo) %>%
                          group_by(product_sub_category) %>%
        summarise(sales_by_product_category=sum(payment_value)) %>%
        arrange(desc(sales_by_product_category)) 
                          
        # arrange(year_month, state, desc(sales_by_product_category)) -> sales_by_product_category
        # sales_by_product_category <- sales_by_product_category[1:50000,]
      # state_sales <- state_sales[1:20000,]
      # state_sales %>%
      #   ungroup() %>%
      #   mutate(year_month = lubridate::date(year_month))
      
    })

    
    output$top10_sales_product_cat<-renderPlot({
      top10sales_summ_by_product_category = top10sales_summ_by_product_category()
      ggplot(data = top10sales_summ_by_product_category, aes(x=reorder(product_sub_category,sales_by_product_category), y=sales_by_product_category )) + 
        geom_bar(stat="identity",fill="orange", color="black") +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
        scale_y_continuous(labels = scales::comma) +
        coord_flip() + 
        geom_text(aes(label = scales::comma(sales_by_product_category)), size = 3, check_overlap = TRUE, position = position_stack(vjust = 0.5)) + 
        labs(title="Top 10 Sales by Product Sub-Category", x="Product Sub-Category Name", y= "Total Sales Value ($)")
      
    }, height = "auto")
    
    
    
    output$top10_sales_product_cat_by_percent<-renderPlot({
      top10sales_summ_by_product_category = top10sales_summ_by_product_category()
      ggplot(data = top10sales_summ_by_product_category, aes(x=reorder(product_sub_category,sales_by_product_category), y=sales_by_product_category/total_sales_volume )) + 
        geom_bar(stat="identity",fill="bisque", color="black") +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
        scale_y_continuous(labels = scales::percent_format()) +
        coord_flip() + 
        geom_text(aes(label = scales::percent(x=sales_by_product_category/total_sales_volume, y=sales_by_product_category/total_sales_volume)), size = 3, check_overlap = TRUE, position = position_stack(vjust = 0.5)) + 
        labs(title="Sales Percent (%) by Product Sub-Category", x="Product Sub-Category", y= "Sales (Percent %)")
      
    }, height = "auto")
    
    # Reactive Data For Line chart
    # cat_time_df_line = reactive({
    #     req(input$datein)
    #     # req(input$trend_category)
    #     
    #     sales_per_product_category_transaction_date %>%
    #         # select(purchase_date) %>%
    #                 # select(purchase_date, input$trend_category) %>%
    #                 filter(purchase_date >= input$datein[1] &
    #                            purchase_date <= input$datein[2])
    #     
    # })
    
    # Reactive Data for Categories Trend table
    # cat_time_table = reactive({
    #     req(input$datein)
    #     req(input$category_input)
        
#                 Categorical_sales_by_time %>%
#                 filter(purchase_date >= input$datein[1] &
#                            purchase_date <= input$datein[2]) %>%
#                 select(purchase_date, input$category_input)
                
                
    #             Categorical_sales_by_time %>%
    #                 select(purchase_date, input$category_input) %>%
    #                     filter(purchase_date >= input$datein[1] &
    #                               purchase_date <= input$datein[2])
    #               
    #             
    # })
    
    # Reactive Data For Bar Chart and Table
    # cat_df_bar = reactive({
    #     req(input$cat_sales_value)
    #     Categorical_sales_by_time %>%
    #         filter(category %in% input$category_choices) %>%
    #         select(category, value = input$cat_sales_value) %>%
    #         arrange(., desc(value))
    #     
    # })
    
    transaction_data_order_status = reactive({
      
      transaction_data_filtered %>% 
         filter(payment_value > 0) %>%
        filter(purchase_date >= input$order_status_date_in[1] &
                 purchase_date <= input$order_status_date_in[2]) %>% 
        filter(state %in% input$order_status_geostate_loc) %>%
        filter(product_sub_category %in% input$order_status_prod_category) %>%
        group_by(year_month=floor_date(purchase_date, "month"), order_status) %>%
        tally(order_status %in% c('canceled','shipped','delivered','invoiced','processing', 'unavailable')) %>%
        mutate(percentage = round(n/sum(n),2))

    })
    
    output$order_status_chart<-renderPlot({
      
      transaction_data_order_status = transaction_data_order_status()
      ggplot(data=transaction_data_order_status, aes(x=year_month, y=percentage, fill=order_status)) + 
        geom_bar(stat="identity", position="stack") +
        scale_x_date(labels = date_format("%Y-%m"),date_breaks = '1 month') +
        scale_y_continuous(labels = scales::percent) +
        geom_text(aes(label = scales::comma(percentage*100)), size = 3, check_overlap = TRUE, position = position_stack(vjust = 0.5)) + 
        labs(title="Order Status Trend over time-horizon", x="Year-Month", y= "Order Status Percentage (%)") 
    
    }, height = "auto")
  
    
    # Categories Bar Chart
    output$barcat = renderGvis(gvisBarChart(
        cat_df_bar(),
        options = list(
            width = "automatic",
            height = "800px",
            bar = "{groupWidth: '80%'}",
            hAxis = "{title:'Sales (in $BRL)', format: 'short', scaleType: 'log'}",
            animation = "{startup: true}",
            legend = "none"
        )
    ))
    
    # Categories Table
    output$cattable = renderTable({
        head(cat_df_bar(), 10)
    },
    striped = T,
    spacing = 'l',
    width = '100%',
    colnames = F)
    
}

shinyApp(ui = ui, server = server)

