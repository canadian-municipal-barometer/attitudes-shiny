library(shiny)

render_sidebar <- function(translator) {
  renderUI({
    message("\n`sidebar_contents` declared\n")

    # choices are captured up front so each menu can start with every option
    # selected (passed as both `choices` and `selected`)
    province_choices <- translator()$t(c(
      "Alberta",
      "British Columbia",
      "Manitoba",
      "New Brunswick",
      "Newfoundland and Labrador",
      "Nova Scotia",
      "Ontario",
      "Prince Edward Island",
      "Quebec",
      "Saskatchewan"
    ))
    popcat_choices <- c(
      "3000-9,999",
      "10,000-49,999",
      "50,000-249,999",
      "250,000-999,999",
      "1,000,000+"
    )
    gender_choices <- translator()$t(c(
      "Woman",
      "Man"
    ))
    agecat_choices <- c(
      "18-29",
      "30-44",
      "45-59",
      "60+"
    )
    race_choices <- translator()$t(c(
      "Racialized minority",
      "White"
    ))
    immigrant_choices <- translator()$t(c(
      "Yes",
      "No"
    ))
    homeowner_choices <- translator()$t(c(
      "Yes",
      "No"
    ))
    education_choices <- translator()$t(c(
      "Less than high school",
      "High school",
      "Associate's degree or trades",
      "Bachelor's degree",
      "Post-graduate degree"
    ))
    income_choices <- c(
      translator()$t("Less than $49,999"),
      "$50,000 - $99,999",
      "$100,000 - $149,999",
      "$150,000 - $199,999",
      translator()$t("$200,000 or more")
    )

    sidebarPanel(
      style = "
          max-width: 32vw;
          min-width: 225px;
          background-color: #e6eff7 !important;
          ",
      selectInput(
        inputId = "province",
        label = translator()$t("Province:"),
        choices = province_choices,
        selected = province_choices,
        multiple = TRUE
      ),
      selectInput(
        inputId = "popcat",
        label = translator()$t("Population:"),
        choices = popcat_choices,
        selected = popcat_choices,
        multiple = TRUE
      ),
      checkboxGroupInput(
        inputId = "gender",
        label = translator()$t("Gender:"),
        choices = gender_choices,
        selected = gender_choices,
        inline = TRUE
      ),
      selectInput(
        inputId = "agecat",
        label = translator()$t("Age:"),
        choices = agecat_choices,
        selected = agecat_choices,
        multiple = TRUE
      ),
      checkboxGroupInput(
        inputId = "race",
        label = translator()$t("Race:"),
        choices = race_choices,
        selected = race_choices
      ),
      checkboxGroupInput(
        inputId = "immigrant",
        label = translator()$t("Immigrant:"),
        choices = immigrant_choices,
        selected = immigrant_choices,
        inline = TRUE
      ),
      checkboxGroupInput(
        inputId = "homeowner",
        label = translator()$t("Homeowner:"),
        choices = homeowner_choices,
        selected = homeowner_choices,
        inline = TRUE
      ),
      selectInput(
        inputId = "education",
        label = translator()$t("Education:"),
        choices = education_choices,
        selected = education_choices,
        multiple = TRUE
      ),
      selectInput(
        inputId = "income",
        label = translator()$t("Income:"),
        choices = income_choices,
        selected = income_choices,
        multiple = TRUE
      ),
      br(),
      shinyWidgets::materialSwitch(
        inputId = "avg_switch",
        label = translator()$t("Compare to the national average"),
        value = TRUE,
        status = "primary"
      ),
    )
  })
}
