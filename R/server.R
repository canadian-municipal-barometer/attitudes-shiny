library(shiny)
library(shiny.i18n)

Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

# data prep --------------------

default_language <- "en"

# main voter data
svy_data <- readRDS("data/voter-data.rds")

# survey years present in the data, for the sidebar year filter
available_years <- sort(unique(svy_data$year))

# statement data. Policy-domain choices are now derived from these tables
# (filtered by the selected year) rather than from standalone statement_tags
# files, so the app naturally offers only the domains present in the chosen years.
statements_en <- readRDS("data/statements_en.rds")
statements_fr <- readRDS("data/statements_fr.rds")

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
      input_err_r(input_err_fr)
      select_prompt_r(select_prompt_fr)

      # Update without shiny.i18n to avoid circular dependency
      updateActionButton(session, "lang_toggle", label = "EN")
    } else {
      current_lang_r("en")

      statements_r(statements_en)
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

  # Statements restricted to the selected survey year(s), in the current
  # language. `input$year` holds character values from the checkbox menu; the
  # data's `year` column is integer, hence the coercion.
  statements_year_r <- reactive({
    yrs <- req(input$year)
    statements_r() |>
      dplyr::filter(year %in% as.integer(yrs))
  })

  # Policy-domain choices, derived from the year-filtered statements (this is
  # what the removed statement_tags_* files used to provide). Selecting only one
  # year therefore drops domains that exist only in the other year.
  domain_choices_r <- reactive({
    statements_year_r()$tags |>
      unlist() |>
      unique() |>
      sort()
  })

  # Policy domain menu
  output$select_domain <- renderUI({
    message("`select_domain` initialized")
    selectInput(
      inputId = "select_domain",
      label = translator_r()$t("Policy domain:"),
      choices = domain_choices_r(), # nolint
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

  # Refresh the domain menu whenever the available domains change — i.e. on a
  # language toggle or a change to the year filter.
  observeEvent(domain_choices_r(), {
    message("\n`select_domain` UI update")
    updateSelectInput(
      session,
      "select_domain",
      choices = domain_choices_r()
    )
  })

  # Update the policy-statement menu from the selected domain. Re-fires on a
  # year change too (with the same domain the statement list still differs), so
  # the policy menu never shows statements from a deselected year. Uses the
  # year-filtered statements as its source.
  observeEvent(list(input$select_domain, input$year), {
    message("\n`select_domain` observer")
    statements_update(
      session = session,
      translator_r = translator_r,
      statements_r = statements_year_r,
      domain = input$select_domain
    )
  })

  # UI Rendering --------------------

  # title panel
  output$title <- renderUI({
    titlePanel(translator_r()$t("Canadians' Municipal Policy Attitudes"))
  })

  # sidebar

  output$sidebar_contents <- render_sidebar(translator = translator_r, years = available_years) # nolint

  # Per-menu "Select all" / "Clear" buttons and the panel-wide "Clear" button.
  # ignoreInit = TRUE so they only respond to clicks, keeping menus empty on load.
  demog_select_ids <- c("province", "popcat", "agecat", "education", "income")
  demog_check_ids <- c("gender", "race", "immigrant", "homeowner")

  # per-menu select-all and clear (only the dropdown menus carry these buttons;
  # the checkbox menus have few options and none)
  lapply(demog_select_ids, function(v) {
    sel_id <- paste0("select_all_", v)
    clr_id <- paste0("clear_", v)
    # buttons reset to 0 when the sidebar re-renders (e.g. language toggle);
    # ignore that so a prior click doesn't re-fire
    observeEvent(input[[sel_id]], ignoreInit = TRUE, {
      if (is.null(input[[sel_id]]) || input[[sel_id]] == 0) {
        return(NULL)
      }
      updateSelectInput(session, v, selected = demographic_choices(translator_r())[[v]])
    })
    observeEvent(input[[clr_id]], ignoreInit = TRUE, {
      if (is.null(input[[clr_id]]) || input[[clr_id]] == 0) {
        return(NULL)
      }
      updateSelectInput(session, v, selected = character(0))
    })
  })

  # clear every menu at once
  observeEvent(input$clear_demographics, ignoreInit = TRUE, {
    if (is.null(input$clear_demographics) || input$clear_demographics == 0) {
      return(NULL)
    }
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

  # Caption under the policy statement: the survey year it was asked in. Kept in
  # the server (not render_mainpanel) so it updates on each policy/language
  # change without re-rendering the whole tabset. Reacts to the language toggle
  # via statements_r() (swaps EN/FR) and translator_r().
  output$policy_year <- renderText({
    req(input$policy)
    idx <- which(statements_r()$statement == input$policy)
    if (length(idx) == 0) {
      return("")
    }
    yr <- statements_r()$year[idx[1]]
    sprintf(translator_r()$t("Asked in %s"), yr)
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
