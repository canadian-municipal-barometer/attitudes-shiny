library(shiny)

render_attitudes_plot <- function(
  statements_r,
  filtered_svy_data_r,
  natl_avg,
  show_natl_avg,
  current_lang_r,
  user_selected,
  input_err_r,
  select_prompt_r,
  lang_toggle_in_progress
) {
  plot <- reactive(
    {
      message("`renderPlot` called")
      req(!lang_toggle_in_progress())
      message("`renderPlot` running")

      statements_r <- isolate(statements_r)
      filtered_svy_data_r <- isolate(filtered_svy_data_r)
      user_selected <- user_selected()

      svy <- filtered_svy_data_r()

      # every menu must have at least one value selected; if any is empty, prompt
      # the user to choose rather than pooling that whole dimension
      validate(
        need(
          length(empty_selections(user_selected)) == 0,
          select_prompt_r()
        )
      )

      # Build the selected subgroup from the (policy-filtered) survey data. Each
      # menu holds one or more values; pooling over multiple selected values
      # happens by averaging the model's predicted probabilities across the
      # matching respondents (below).
      subgroup <- build_subgroup(svy, user_selected)

      # verify the selected combination matches at least one respondent
      validate(
        need(
          nrow(subgroup) > 0,
          input_err_r()
        )
      )

      # an immediately invoked function
      # the model is fit on all of the policy's data; the menu selections only
      # determine which respondents the predictions are pooled over.
      model <- (function() {
        sink("/dev/null") # disable console logging
        model <- nnet::multinom(
          factor(outcome) ~
            factor(gender) +
              factor(education) +
              factor(province) +
              factor(agecat) +
              factor(race) +
              factor(homeowner) +
              factor(income) +
              factor(immigrant) +
              factor(popcat), # nolint
          data = svy,
          weights = svy$wgt
        )
        sink()
        if (!is.null(model)) {
          message("\n---model fit successful\n")
          return(model)
        }
      })()

      validate(
        need(model, "We're sorry. There seems to have been error.")
      )

      # predict for every respondent in the selected subgroup, then pool by
      # taking the survey-weighted mean of the predicted probabilities. This
      # reflects the real demographic composition of the selected group.
      pred_probs <- predict(model, subgroup, type = "probs")
      preds <- pool_preds(pred_probs, subgroup$wgt)
      preds <- tidyr::tibble(
        cats = names(preds),
        probs = preds,
        group = as.factor("preds")
      )

      preds$cats <- factor(
        preds$cats,
        levels = c(
          "No opinion",
          "Disagree",
          "Agree"
        ),
        labels = c(
          "No opinion",
          "Disagree",
          "Agree"
        ),
        ordered = TRUE
      )

      message(paste("preds:", preds))

      build_plot(
        preds,
        filtered_svy_data_r,
        show_natl_avg,
        natl_avg,
        current_lang_r
      )
    }
  )
  return(plot)
}
