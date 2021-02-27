library(shiny)
library(shinydashboard)  

shinyUI(
  dashboardPage( 
    dashboardHeader(title = "PLUS Watch"),        
    skin = "blue",
    dashboardSidebar(
      sidebarMenu(id = "menu1",
                  menuItem("Incrementality Estimation", icon = icon("dashboard"),
                           menuSubItem("Marketing View", "pureplus", icon = icon("angle-right"), selected = TRUE),
                           menuSubItem("FP&A View - Per Member", "finance_view_permbr", icon = icon("angle-right")),
                           menuSubItem("FP&A View - Total Sum", "finance_view_allsum", icon = icon("angle-right"))
                           ),
                  # menuItem("Incrementality - FP&A View", tabName = "finance_view", icon = icon("dashboard"),
                  #          menuSubItem("Pure PLUS", "pureplus_f", icon = icon("angle-right")),
                  #          menuSubItem("PLUS x Flybuys", "plusfly_f", icon = icon("angle-right")),
                  #          menuSubItem("Overall PLUS", "plusall_f", icon = icon("angle-right"))
                  # ),
                  menuItem("Cohort Facts", icon = icon("dashboard"),
                           menuSubItem("Quick Facts (Monthly Avg.)", tabName = "quick_fact", icon = icon("angle-right"))
                           # menuSubItem("Number of Members", tabName = "cohort_fact", icon = icon("angle-right"))
                  ),
                  menuItem("FT Cancel Prediction", tabName = "ft_cncl_pred", icon = icon("dashboard")),
      br(),
      uiOutput("filter1")
      # conditionalPanel(
      #   condition = "input.tabs == 'ft_cncl_pred'",
      #   # uiOutput("filter1")
      #   sliderInput("test1", "L2 Category Cutoff %", min = 0, max = 0.01, value = 0.005, step = 0.001)
      #  )
      )
    ),
    dashboardBody(
      ### For Marketing View ----
      tabItems(
        tabItem(tabName = "pureplus",
                fluidRow(
                  tabBox(id = "tabbox1", title = NULL, width = 12,
                         tabPanel("Total GMV Uplift",
                                  fluidRow(
                                    box(plotOutput("plot1", height = 500),
                                        title = "Uplift% Trend - Graph",
                                        status = "primary",
                                        width = 12,
                                        height = 600,
                                        solidHeader = TRUE)
                                  ),
                                  fluidRow(
                                    box(DT::dataTableOutput("table1"),
                                        title = "Uplift% Trend - Table",
                                        status = "warning",
                                        width = 12,
                                        height = 500,
                                        solidHeader = TRUE)
                                  )
                         ),
                         tabPanel("Coupon GMV Uplift",
                                  fluidRow(
                                    box(plotOutput("plot2", height = 500),
                                        title = "Uplift% Trend - Graph",
                                        status = "primary",
                                        width = 12,
                                        height = 600,
                                        solidHeader = TRUE)
                                  ),
                                  fluidRow(
                                    box(DT::dataTableOutput("table2"),
                                        title = "Uplift% Trend - Table",
                                        status = "warning",
                                        width = 12,
                                        height = 500,
                                        solidHeader = TRUE)
                                  )
                         ),
                         tabPanel("Organic GMV Uplift",
                                  fluidRow(
                                    box(plotOutput("plot3", height = 500),
                                        title = "Uplift% Trend - Graph",
                                        status = "primary",
                                        width = 12,
                                        height = 600,
                                        solidHeader = TRUE)
                                  ),
                                  fluidRow(
                                    box(DT::dataTableOutput("table3"),
                                        title = "Uplift% Trend - Table",
                                        status = "warning",
                                        width = 12,
                                        height = 500,
                                        solidHeader = TRUE)
                                  )
                         )
                     )
                  )
              ),
        # tabItem(tabName = "plusfly",
        #         fluidRow(tabBox(id = "tabbox2", title = NULL, width = 12,
        #                         tabPanel("Total GMB Uplift",
        #                                  fluidRow(
        #                                    box(plotOutput("plot1_2", height = 500),
        #                                        title = "Uplift% Trend - Graph",
        #                                        status = "primary",
        #                                        width = 12,
        #                                        height = 600,
        #                                        solidHeader = TRUE)
        #                                  ),
        #                                  fluidRow(
        #                                    box(DT::dataTableOutput("table1_2"),
        #                                        title = "Uplift% Trend - Table",
        #                                        status = "warning",
        #                                        width = 12,
        #                                        height = 450,
        #                                        solidHeader = TRUE)
        #                                  )
        #                         ),
        #                         tabPanel("Coupon GMB Uplift",
        #                                  fluidRow(
        #                                    box(plotOutput("plot2_2", height = 500),
        #                                        title = "Uplift% Trend - Graph",
        #                                        status = "primary",
        #                                        width = 12,
        #                                        height = 600,
        #                                        solidHeader = TRUE)
        #                                  ),
        #                                  fluidRow(
        #                                    box(DT::dataTableOutput("table2_2"),
        #                                        title = "Uplift% Trend - Table",
        #                                        status = "warning",
        #                                        width = 12,
        #                                        height = 450,
        #                                        solidHeader = TRUE)
        #                                  )
        #                         ),
        #                         tabPanel("Organic GMB Uplift",
        #                                  fluidRow(
        #                                    box(plotOutput("plot3_2", height = 500),
        #                                        title = "Uplift% Trend - Graph",
        #                                        status = "primary",
        #                                        width = 12,
        #                                        height = 600,
        #                                        solidHeader = TRUE)
        #                                  ),
        #                                  fluidRow(
        #                                    box(DT::dataTableOutput("table3_2"),
        #                                        title = "Uplift% Trend - Table",
        #                                        status = "warning",
        #                                        width = 12,
        #                                        height = 450,
        #                                        solidHeader = TRUE)
        #                                  )
        #                         )
        #         ))),
        # tabItem(tabName = "plusall",
        #         fluidRow(tabBox(id = "tabbox3", title = NULL, width = 12,
        #                         tabPanel("Total GMB Uplift",
        #                                  fluidRow(
        #                                    box(plotOutput("plot1_3", height = 500),
        #                                        title = "Uplift% Trend - Graph",
        #                                        status = "primary",
        #                                        width = 12,
        #                                        height = 600,
        #                                        solidHeader = TRUE)
        #                                  ),
        #                                  fluidRow(
        #                                    box(DT::dataTableOutput("table1_3"),
        #                                        title = "Uplift% Trend - Table",
        #                                        status = "warning",
        #                                        width = 12,
        #                                        height = 450,
        #                                        solidHeader = TRUE)
        #                                  )
        #                         ),
        #                         tabPanel("Coupon GMB Uplift",
        #                                  fluidRow(
        #                                    box(plotOutput("plot2_3", height = 500),
        #                                        title = "Uplift% Trend - Graph",
        #                                        status = "primary",
        #                                        width = 12,
        #                                        height = 600,
        #                                        solidHeader = TRUE)
        #                                  ),
        #                                  fluidRow(
        #                                    box(DT::dataTableOutput("table2_3"),
        #                                        title = "Uplift% Trend - Table",
        #                                        status = "warning",
        #                                        width = 12,
        #                                        height = 450,
        #                                        solidHeader = TRUE)
        #                                  )
        #                         ),
        #                         tabPanel("Organic GMB Uplift",
        #                                  fluidRow(
        #                                    box(plotOutput("plot3_3", height = 500),
        #                                        title = "Uplift% Trend - Graph",
        #                                        status = "primary",
        #                                        width = 12,
        #                                        height = 600,
        #                                        solidHeader = TRUE)
        #                                  ),
        #                                  fluidRow(
        #                                    box(DT::dataTableOutput("table3_3"),
        #                                        title = "Uplift% Trend - Table",
        #                                        status = "warning",
        #                                        width = 12,
        #                                        height = 450,
        #                                        solidHeader = TRUE)
        #                                  )
        #                         )
        #         ))),
        ### For FP&A View ----
        tabItem(tabName = "finance_view_permbr",
                fluidRow(
                  tabBox(id = "tabbox4", title = NULL, width = 9,
                         tabPanel("Total GMV Uplift",
                                  fluidRow(
                                    box(DT::dataTableOutput("table_f_tot"),
                                        title = "Uplift% Trend - Table",
                                        status = "primary",
                                        width = 12,
                                        height = 800,
                                        solidHeader = TRUE)
                                  )
                         ),
                         tabPanel("Coupon GMV Uplift",
                                  fluidRow(
                                    box(DT::dataTableOutput("table_f_cpn"),
                                        title = "Uplift% Trend - Table",
                                        status = "primary",
                                        width = 12,
                                        height = 800,
                                        solidHeader = TRUE)
                                  )
                         ),
                         tabPanel("Organic GMV Uplift",
                                  fluidRow(
                                    box(DT::dataTableOutput("table_f_org"),
                                        title = "Uplift% Trend - Table",
                                        status = "primary",
                                        width = 12,
                                        height = 800,
                                        solidHeader = TRUE)
                                  )
                         )
                  )
               )
        ),
        
        tabItem(tabName = "finance_view_allsum",
                fluidRow(
                  tabBox(id = "tabbox4", title = NULL, width = 9,
                         tabPanel("Total GMV Uplift",
                                  fluidRow(
                                    box(DT::dataTableOutput("table_f_tot_allsum"),
                                        title = "Uplift% Trend - Table",
                                        status = "primary",
                                        width = 12,
                                        height = 800,
                                        solidHeader = TRUE)
                                  )
                         ),
                         tabPanel("Coupon GMV Uplift",
                                  fluidRow(
                                    box(DT::dataTableOutput("table_f_cpn_allsum"),
                                        title = "Uplift% Trend - Table",
                                        status = "primary",
                                        width = 12,
                                        height = 800,
                                        solidHeader = TRUE)
                                  )
                         ),
                         tabPanel("Organic GMV Uplift",
                                  fluidRow(
                                    box(DT::dataTableOutput("table_f_org_allsum"),
                                        title = "Uplift% Trend - Table",
                                        status = "primary",
                                        width = 12,
                                        height = 800,
                                        solidHeader = TRUE)
                                  )
                         )
                  )
                )
        ),
        
        tabItem(tabName = "quick_fact",
                fluidRow(
                  # box(DT::dataTableOutput("cohort_benefit"),
                  #     title = "Cohort Facts",
                  #     status = "primary",
                  #     width = 12,
                  #     height = 700,
                  #     solidHeader = TRUE)
                  tabBox(id = "tabbox5", title = NULL, width = 12,
                         tabPanel("Overall Summary",
                                  fluidRow(
                                    box(DT::dataTableOutput("cohort_summary"),
                                        title = "Monthly Facts",
                                        status = "primary",
                                        width = 12,
                                        height = 800,
                                        solidHeader = TRUE)
                                  )
                         ),
                         tabPanel("Waiver Usage",
                                  fluidRow(
                                    box(DT::dataTableOutput("cohort_waiver"),
                                        title = "Monthly Facts",
                                        status = "primary",
                                        width = 12,
                                        height = 800,
                                        solidHeader = TRUE)
                                  )
                         ),
                         tabPanel("Transaction Related",
                                  fluidRow(
                                    box(DT::dataTableOutput("cohort_txn"),
                                        title = "Monthly Facts",
                                        status = "primary",
                                        width = 12,
                                        height = 800,
                                        solidHeader = TRUE)
                                  )
                         )
                  )
                )
        ),
        
        tabItem(tabName = "cohort_fact",
                fluidRow(
                  box(DT::dataTableOutput("cohort_user_cnt"),
                      title = "Cohort Facts",
                      status = "primary",
                      width = 9,
                      height = 700,
                      solidHeader = TRUE)
                )
        ),
        
        tabItem(tabName = "ft_cncl_pred",
                fluidRow(
                  box(
                    sliderInput("day_ctrl", label = NULL, min = 0, max = 100, value = 15, step = 1),
                    title = "Number of days to appear",
                    status = "primary",
                    width = 3,
                    height = 120,
                    solidHeader = TRUE)
                ),
                fluidRow(
                  box(DT::dataTableOutput("ft_cncl_pred_data"),
                      title = "Free-Trial Cancel Prediction",
                      status = "primary",
                      width = 12,
                      height = 750,
                      solidHeader = TRUE)
                )
        )
      )
    ) # closing of dashboardBody
  )
)
  

                    
  

