library(shiny)

render_sidebar <- function(translator, years) {
  renderUI({
    message("\n`sidebar_contents` declared\n")

    tr <- translator()
    # translated choice lists, shared with the server's per-menu observers
    choices <- demographic_choices(tr)

    # each menu is rendered with a bold label. The dropdown menus additionally
    # carry compact per-menu "Select all" / "Clear" buttons beside the label
    # (wired up in server.R); the checkbox menus (few options) omit them.
    menu <- function(id, label, widget, buttons = TRUE) {
      header <- if (buttons) {
        div(
          style = "
            display: flex;
            justify-content: space-between;
            align-items: baseline;
          ",
          tags$label(label, style = "font-weight: bold; margin-bottom: 2px;"),
          div(
            style = "display: flex; gap: 4px;",
            actionButton(
              paste0("select_all_", id),
              tr$t("Select all"),
              class = "btn-xs"
            ),
            actionButton(
              paste0("clear_", id),
              tr$t("Clear"),
              class = "btn-xs"
            )
          )
        )
      } else {
        tags$label(
          label,
          style = "font-weight: bold; margin-bottom: 2px; display: block;"
        )
      }
      div(header, widget)
    }

    sidebarPanel(
      style = "
          max-width: 32vw;
          min-width: 225px;
          background-color: #e6eff7 !important;
          ",
      # Survey-year filter. Its own section above the demographics, because a
      # survey year is a property of the question, not of the respondent. All
      # years selected by default. Deliberately NOT wired into the panel-wide
      # "Clear" (server's demog_*_ids), so clearing demographics leaves it alone.
      menu(
        "year",
        tr$t("Survey year:"),
        checkboxGroupInput(
          "year",
          label = NULL,
          choices = years,
          selected = years,
          inline = TRUE
        ),
        buttons = FALSE
      ),
      tags$h4(
        tr$t("Socio-demographics"),
        style = "font-weight: bold;"
      ),
      menu(
        "province",
        tr$t("Province:"),
        selectInput(
          "province",
          label = NULL,
          choices = choices$province,
          multiple = TRUE
        )
      ),
      menu(
        "popcat",
        tr$t("Population:"),
        selectInput(
          "popcat",
          label = NULL,
          choices = choices$popcat,
          multiple = TRUE
        )
      ),
      menu(
        "gender",
        tr$t("Gender:"),
        checkboxGroupInput(
          "gender",
          label = NULL,
          choices = choices$gender,
          inline = TRUE
        ),
        buttons = FALSE
      ),
      menu(
        "agecat",
        tr$t("Age:"),
        selectInput(
          "agecat",
          label = NULL,
          choices = choices$agecat,
          multiple = TRUE
        )
      ),
      menu(
        "race",
        tr$t("Race:"),
        checkboxGroupInput("race", label = NULL, choices = choices$race),
        buttons = FALSE
      ),
      menu(
        "immigrant",
        tr$t("Immigrant:"),
        checkboxGroupInput(
          "immigrant",
          label = NULL,
          choices = choices$immigrant,
          inline = TRUE
        ),
        buttons = FALSE
      ),
      menu(
        "homeowner",
        tr$t("Homeowner:"),
        checkboxGroupInput(
          "homeowner",
          label = NULL,
          choices = choices$homeowner,
          inline = TRUE
        ),
        buttons = FALSE
      ),
      menu(
        "education",
        tr$t("Education:"),
        selectInput(
          "education",
          label = NULL,
          choices = choices$education,
          multiple = TRUE
        )
      ),
      menu(
        "income",
        tr$t("Income:"),
        selectInput(
          "income",
          label = NULL,
          choices = choices$income,
          multiple = TRUE
        )
      ),
      br(),
      shinyWidgets::materialSwitch(
        inputId = "avg_switch",
        label = tr$t("Compare to the national average"),
        value = TRUE,
        status = "primary"
      ),
      br(),
      # panel-wide clear at the bottom (empties every menu, including checkboxes)
      actionButton(
        "clear_demographics",
        tr$t("Clear"),
        class = "btn-sm"
      )
    )
  })
}
