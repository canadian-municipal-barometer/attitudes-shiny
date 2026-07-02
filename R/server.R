library(shiny)
library(shiny.i18n)

Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

# data prep --------------------

default_language <- "en"

# main voter data
svy_data <- readRDS("data/voter-data.rds")

# statement data
statements_en <- readRDS("data/statements_en.rds")
statement_tags_en <- readRDS("data/statement_tags_en.rds")
statements_fr <- readRDS("data/statements_fr.rds")
statement_tags_fr <- readRDS("data/statement_tags_fr.rds")

# national average data
natl_avg <- readRDS("data/natl_avg.rds")

# the error that is displayed if model inputs aren't present for a policy
input_err_en <- "The combination of the policy question and demographic characteristics that you have selected aren't in the data. Please make another selection." # nolint
input_err_fr <- "La combinaison de la question de politique publique et des caractéristiques démographiques que vous avez sélectionnée ne figure pas dans les données. Veuillez faire une autre sélection." # nolint

# the prompt shown when a demographic menu has been emptied
select_prompt_en <- "Please select at least one option in each menu of the Socio-demographics panel to see the opinion estimates." # nolint
select_prompt_fr <- "Veuillez sélectionner au moins une option dans chaque menu du panneau Sociodémographie pour afficher les estimations." # nolint

# load translation file to create shiny.i18n translator object
translator <- shiny.i18n::Translator$new(
  translation_csvs_path = "data/translation/"
)

server <- function(input, output, session) {
  cat("server function entered")

  # Initialize reactive values
  current_lang_r <- reactiveVal(default_language)
  statements_r <- reactiveVal(statements_en) # nolint
  statement_tags_r <- reactiveVal(statement_tags_en) # nolint
  svy_data_r <- reactiveVal(svy_data) #nolint
  input_err_r <- reactiveVal(input_err_en)
  select_prompt_r <- reactiveVal(select_prompt_en)
  lang_toggle_in_progress <- reactiveVal(FALSE)

  # Handle language toggle of data
  observeEvent(input$lang_toggle, {
    lang_toggle_in_progress(TRUE)
    # Toggle language between English and French
    if (current_lang_r() == "en") {
      current_lang_r("fr")

      statements_r(statements_fr)
      statement_tags_r(statement_tags_fr)
      input_err_r(input_err_fr)
      select_prompt_r(select_prompt_fr)

      # Update without shiny.i18n to avoid circular dependency
      updateActionButton(session, "lang_toggle", label = "EN")
    } else {
      current_lang_r("en")

      statements_r(statements_en)
      statement_tags_r(statement_tags_en)
      input_err_r(input_err_en)
      select_prompt_r(select_prompt_en)

      updateActionButton(session, "lang_toggle", label = "FR")
    }
    lang_toggle_in_progress(FALSE)
    message("\nlang_toggle complete")
    message(paste("`current_lang`:", current_lang_r(), "\n"))
  })

  # reacts to `lang_toggle` via changes to `current_lang_r`
  translator_r <- reactive({
    translator$set_translation_language(current_lang_r())
    return(translator)
  })

  # main reactive elements --------------------

  # Policy domain menu
  output$select_domain <- renderUI({
    message("`select_domain` initialized")
    selectInput(
      inputId = "select_domain",
      label = translator_r()$t("Policy domain:"),
      choices = statement_tags_r(), # nolint
      selectize = TRUE,
      width = "325px"
    )
  })

  # policy statements menu
  output$policy <- renderUI({
    message("`policy` menu rendered")
    selectInput(
      inputId = "policy",
      label = "Select a policy:",
      # updated in `server` first time `select_domain` input used
      choices = NULL,
      selectize = TRUE,
      width = "auto",
    )
  })

  observeEvent(statement_tags_r(), {
    message("\n`select_domain` UI update")
    updateSelectInput(
      session,
      "select_domain",
      choices = statement_tags_r()
    )
  })

  # update policy statement menu based on policy domain menu
  observeEvent(input$select_domain, {
    message("\n`select_domain` observer")
    statements_update(
      session = session,
      translator_r = translator_r,
      statements_r = statements_r,
      domain = input$select_domain
    )
  })

  # UI Rendering --------------------

  # title panel
  output$title <- renderUI({
    titlePanel(translator_r()$t("Canadians' Municipal Policy Attitudes"))
  })

  # sidebar

  output$sidebar_contents <- render_sidebar(translator = translator_r) # nolint

  # "Select all" / "Reset" buttons for the Socio-demographics panel.
  # ignoreInit = TRUE so they only respond to clicks, keeping menus empty on load.
  demog_select_ids <- c("province", "popcat", "agecat", "education", "income")
  demog_check_ids <- c("gender", "race", "immigrant", "homeowner")

  observeEvent(input$select_all, ignoreInit = TRUE, {
    ch <- demographic_choices(translator_r())
    for (id in demog_select_ids) {
      updateSelectInput(session, id, selected = ch[[id]])
    }
    for (id in demog_check_ids) {
      updateCheckboxGroupInput(session, id, selected = ch[[id]])
    }
  })

  observeEvent(input$reset_demographics, ignoreInit = TRUE, {
    for (id in demog_select_ids) {
      updateSelectInput(session, id, selected = character(0))
    }
    for (id in demog_check_ids) {
      updateCheckboxGroupInput(session, id, selected = character(0))
    }
  })

  # plot

  filtered_svy_r <- eventReactive(input$policy, {
    message("data filtering reactive called")
    filter_statements(
      statements = statements_r,
      svy_data_r = svy_data_r,
      policy = input$policy
    )
  })

  # un-translated inputs if they were translated to French in the UI
  user_selected <- reactive({
    message("`un_translate_input` reactive entered")
    req(!lang_toggle_in_progress())
    # req(
    #   input$province,
    #   input$agecat,
    #   input$popcat,
    #   input$gender,
    #   input$race,
    #   input$immigrant,
    #   input$homeowner,
    #   input$education,
    #   input$income
    # )
    un_translate_input(input)
  })

  plot_r <- render_attitudes_plot(
    statements_r = statements_r,
    filtered_svy_data_r = filtered_svy_r, # nolint
    natl_avg = natl_avg,
    show_natl_avg = reactive({
      input$avg_switch
    }),
    current_lang_r = current_lang_r,
    user_selected = user_selected,
    input_err_r = input_err_r,
    select_prompt_r = select_prompt_r,
    lang_toggle_in_progress
  )

  # NOTE: This might seem like a useless abstraction, but it lets me have the
  # original plot object (plot_r) and do stuff to it besides just rendering it
  # to the UI. This is used to add things to the plot when users press the
  # "download plot" button.
  output$plot <- renderPlot(
    {
      plot_r()
    },
    bg = "transparent"
  )

  # mainpanel

  output$mainpanel <- render_mainpanel(
    translator_r = translator_r,
    statements_r = statements_r
  )
}
