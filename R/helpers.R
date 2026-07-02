library(shiny)

# The nine demographic menus, in sidebar order. Shared by `build_subgroup` and
# `empty_selections` so the canonical variable list lives in one place.
demographic_vars <- c(
  "province", "popcat", "gender", "agecat", "race",
  "immigrant", "homeowner", "education", "income"
)

# Choice vectors for the nine Socio-demographics menus, translated via the given
# translator instance. Shared by the sidebar (to build the menus) and the server
# (so the "Select all" button can select every option in the current language).
demographic_choices <- function(translator) {
  list(
    province = translator$t(c(
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
    )),
    popcat = c(
      "3000-9,999",
      "10,000-49,999",
      "50,000-249,999",
      "250,000-999,999",
      "1,000,000+"
    ),
    gender = translator$t(c("Woman", "Man")),
    agecat = c("18-29", "30-44", "45-59", "60+"),
    race = translator$t(c("Racialized minority", "White")),
    immigrant = translator$t(c("Yes", "No")),
    homeowner = translator$t(c("Yes", "No")),
    education = translator$t(c(
      "Less than high school",
      "High school",
      "Associate's degree or trades",
      "Bachelor's degree",
      "Post-graduate degree"
    )),
    income = c(
      translator$t("Less than $49,999"),
      "$50,000 - $99,999",
      "$100,000 - $149,999",
      "$150,000 - $199,999",
      translator$t("$200,000 or more")
    )
  )
}

# Vectorized French -> English lookup. `x` may be a character vector (the
# sidebar menus are now multi-select), NULL, or empty. Values not found in
# `map` (e.g. values that are already English, or identical in both languages)
# are passed through unchanged. NULL / empty selections are returned as-is,
# which the plot logic treats as "all values" (no filter on that variable).
recode_vec <- function(x, map) {
  if (is.null(x) || length(x) == 0) {
    return(x)
  }
  out <- unname(map[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

# Converts the input object to a list of (possibly multi-value) selections,
# translating any French menu labels back to English, because the model only
# runs on English values. A list is required by `render_attitudes_plot` even
# when no language translation occurs.
un_translate_input <- function(input) {
  cat("---`un_translate_input` ran")

  province_map <- c(
    "Colombie-Britannique" = "British Columbia",
    "Nouveau-Brunswick" = "New Brunswick",
    "Terre-Neuve-et-Labrador" = "Newfoundland and Labrador",
    "Nouvelle-Écosse" = "Nova Scotia",
    "Île-du-Prince-Édouard" = "Prince Edward Island",
    "Québec" = "Quebec"
  )

  gender_map <- c(
    "Homme" = "Man",
    "Femme" = "Woman"
  )

  race_map <- c(
    "Minorité racisée" = "Racialized minority",
    "Blanc·che" = "White"
  )

  yes_no_map <- c(
    "Oui" = "Yes",
    "Non" = "No"
  )

  education_map <- c(
    "Moins que les études secondaires" = "Less than high school",
    "Diplôme d’études secondaires" = "High school",
    "Apprentissage/Diplôme d’études professionnelles (DEP)" = "Associate's degree or trades", # nolint
    "Baccalauréat" = "Bachelor's degree",
    "Maitrise, doctorat, diplôme professionnel" = "Post-graduate degree"
  )

  income_map <- c(
    "Moins de $49,999" = "Less than $49,999",
    "200,000 $ ou plus" = "$200,000 or more"
  )

  list(
    province = recode_vec(input$province, province_map),
    agecat = input$agecat,
    popcat = input$popcat,
    gender = recode_vec(input$gender, gender_map),
    race = recode_vec(input$race, race_map),
    immigrant = recode_vec(input$immigrant, yes_no_map),
    homeowner = recode_vec(input$homeowner, yes_no_map),
    education = recode_vec(input$education, education_map),
    income = recode_vec(input$income, income_map)
  )
}

statements_update <- function(
  session = session,
  translator_r,
  statements_r,
  domain
) {
  cat("---`statements_update` ran\n")

  filtered_statements <- statements_r() |>
    dplyr::filter(
      purrr::map_lgl(tags, function(x) any(x %in% domain))
    ) |>
    dplyr::pull(statement) # nolint

  updateSelectInput(
    session,
    "policy",
    label = translator_r()$t("Select a policy:"),
    choices = filtered_statements,
    selected = filtered_statements[1]
  )
}

filter_statements <- function(statements, svy_data_r, policy) {
  req(!is.null(policy) & policy != "")
  # Find the selected policy in statements
  index <- which(statements()$statement == policy) # input$policy
  val <- statements()$var_name[index]
  tbl <- svy_data_r() |> dplyr::filter(policy == val)
  return(tbl)
}

# Subset the survey data to the demographic subgroup the user selected. Each
# menu may hold one or more values; an empty / NULL selection means "all
# values", so that variable is simply left unfiltered. Variables are combined
# with AND, values within a variable with OR (via `%in%`).
build_subgroup <- function(svy, user_selected, demog_vars = demographic_vars) {
  keep <- rep(TRUE, nrow(svy))
  for (v in demog_vars) {
    sel <- user_selected[[v]]
    if (!is.null(sel) && length(sel) > 0) {
      keep <- keep & (svy[[v]] %in% sel)
    }
  }
  svy[keep, , drop = FALSE]
}

# Names of the demographic menus that currently have nothing selected (NULL or
# empty), in sidebar order. Menus combine with AND, so any empty menu makes the
# subgroup empty; the app uses this to prompt the user to pick something instead
# of silently pooling that whole dimension.
empty_selections <- function(user_selected, demog_vars = demographic_vars) {
  is_empty <- vapply(demog_vars, function(v) {
    sel <- user_selected[[v]]
    is.null(sel) || length(sel) == 0
  }, logical(1))
  demog_vars[is_empty]
}

# Pool a subgroup's predicted outcome probabilities into a single estimate by
# taking the survey-weighted mean of each outcome column, returned as rounded
# percentages. `pred_probs` is the `predict(..., type = "probs")` output: a
# matrix (one row per respondent) or, for a single respondent, a named vector.
pool_preds <- function(pred_probs, weights) {
  if (is.null(dim(pred_probs))) {
    # a single matching respondent yields a named vector, not a matrix
    pred_probs <- matrix(
      pred_probs,
      nrow = 1,
      dimnames = list(NULL, names(pred_probs))
    )
  }
  preds <- apply(pred_probs, 2, stats::weighted.mean, w = weights)
  round(preds * 100, 0)
}

simple_plot <- function(preds) {
  ggplot2::ggplot(
    preds,
    ggplot2::aes(x = cats, y = probs, fill = cats) # nolint
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(probs, "%")),
      hjust = -0.1,
      size = 5
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::scale_fill_manual(
      values = c(
        "#6C6E74",
        "#000",
        "#0091AC"
      )
    ) +
    ggplot2::theme(
      legend.position = "none",
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

natl_avg_plot <- function(preds) {
  ggplot2::ggplot(
    preds,
    ggplot2::aes(x = cats, y = probs, fill = fill_group, group = group) # nolint
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(probs, "%")),
      position = ggplot2::position_dodge(width = 0.9),
      hjust = -0.1,
      size = 5
    ) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

build_plot <- function(
  preds,
  filtered_svy_data_r,
  show_natl_avg,
  natl_avg,
  current_lang_r
) {
  if (show_natl_avg()) {
    # prepare data, assuming English is the current language
    # `policy` is a factor; coerce to character so `natl_avg[[...]]` looks up by
    # name (the statement id) rather than by the factor's integer level code,
    # which would return a different policy's national average.
    policy_i <- as.character(filtered_svy_data_r()$policy[1])
    natl_avg_i <- natl_avg[[policy_i]]
    natl_avg_i$fill_group <- "National average"
    preds$fill_group <- preds$cats
    preds$fill_group <- preds$fill_group |>
      factor(ordered = TRUE) |>
      forcats::fct_rev()
    preds <- dplyr::bind_rows(preds, natl_avg_i)
    preds$group <- preds$group |>
      forcats::fct_rev()

    # translate the plot data to french if current language is French
    if (current_lang_r() == "fr") {
      preds$cats <- preds$cats |>
        forcats::fct_recode(
          "Pas d'opinion" = "No opinion",
          "Désaccord" = "Disagree",
          "D'accord" = "Agree"
        )
      preds$fill_group <- preds$fill_group |>
        forcats::fct_collapse(
          "Moyenne nationale" = "National average",
          "Pas d'opinion" = "No opinion",
          "Désaccord" = "Disagree",
          "D'accord" = "Agree"
        )
    }

    # call the plot constructor
    plot <- natl_avg_plot(preds)
  } else {
    # call the plot constructor
    plot <- simple_plot(preds)
  }

  # add scale spec (dependent on current language)
  if (current_lang_r() == "en") {
    plot <- plot +
      ggplot2::scale_fill_manual(
        values = c(
          "National average" = "#c7c7c7",
          "No opinion" = "#6C6E74",
          "Disagree" = "#000000",
          "Agree" = "#0091AC"
        ),
        # This makes its contents the only label in the legend
        breaks = c("National average")
      )
  } else {
    plot <- plot +
      ggplot2::scale_fill_manual(
        values = c(
          "Moyenne nationale" = "#c7c7c7",
          "Pas d'opinion" = "#6C6E74",
          "Désaccord" = "#000000",
          "D'accord" = "#0091AC"
        ),
        # This makes its contents the only label in the legend
        breaks = c("Moyenne nationale")
      )
  }
  return(plot)
}
