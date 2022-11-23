bs4Dash::dashboardPage(
  title = "Camping Weather",
  bs4Dash::dashboardHeader(
    title = "Camping Weather"
  ),
  bs4Dash::dashboardSidebar(
    bs4Dash::sidebarMenu(
      bs4Dash::menuItem(
        text = "Weather Station Picker",
        tabName = "tab_3txcd2oqy2",
        icon = icon("arrow-pointer")
      ),
      bs4Dash::menuItem(
        text = "Temperature",
        tabName = "tab_zao8u5lr1a",
        icon = icon("temperature-full")
      )
    )
  ),
  bs4Dash::dashboardBody(
    bs4Dash::tabItem(
      tabName = "tab_3txcd2oqy2",
      h1(
        "Setup"
      ),
      inputPanel(
        dateInput(
          inputId = "startDate",
          label = "Start Date"
        ),
        selectInput(
          inputId = "input_8o458isjpt",
          label = "Weather Station Picker",
          choices = NULL
        )
      ),
      plotOutput(
        outputId = "output_myjm0kalwm"
      )
    ),
    bs4Dash::tabItem(
      tabName = "tab_zao8u5lr1a"
    )
  )
)