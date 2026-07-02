library(shiny)

render_sidebar <- function(translator) {
  renderUI({
    message("\n`sidebar_contents` declared\n")

    # translated choice lists, shared with the server's "Select all" observer
    choices <- demographic_choices(translator())

    sidebarPanel(
      style = "
          max-width: 32vw;
          min-width: 225px;
          background-color: #e6eff7 !important;
          ",
      tags$h4(
        translator()$t("Socio-demographics"),
        style = "margin-top: 0; font-weight: bold;"
      ),
      # bulk-selection controls (wired up in server.R)
      div(
        style = "display: flex; gap: 8px; margin-bottom: 12px;",
        actionButton(
          "select_all",
          translator()$t("Select all"),
          class = "btn-sm"
        ),
        actionButton(
          "reset_demographics",
          translator()$t("Reset"),
          class = "btn-sm"
        )
      ),
      selectInput(
        inputId = "province",
        label = translator()$t("Province:"),
        choices = choices$province,
        multiple = TRUE
      ),
      selectInput(
        inputId = "popcat",
        label = translator()$t("Population:"),
        choices = choices$popcat,
        multiple = TRUE
      ),
      checkboxGroupInput(
        inputId = "gender",
        label = translator()$t("Gender:"),
        choices = choices$gender,
        inline = TRUE
      ),
      selectInput(
        inputId = "agecat",
        label = translator()$t("Age:"),
        choices = choices$agecat,
        multiple = TRUE
      ),
      checkboxGroupInput(
        inputId = "race",
        label = translator()$t("Race:"),
        choices = choices$race
      ),
      checkboxGroupInput(
        inputId = "immigrant",
        label = translator()$t("Immigrant:"),
        choices = choices$immigrant,
        inline = TRUE
      ),
      checkboxGroupInput(
        inputId = "homeowner",
        label = translator()$t("Homeowner:"),
        choices = choices$homeowner,
        inline = TRUE
      ),
      selectInput(
        inputId = "education",
        label = translator()$t("Education:"),
        choices = choices$education,
        multiple = TRUE
      ),
      selectInput(
        inputId = "income",
        label = translator()$t("Income:"),
        choices = choices$income,
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
