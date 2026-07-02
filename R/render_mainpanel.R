library(shiny)

render_mainpanel <- function(translator_r, statements_r) {
  renderUI({
    message("\n`mainpanel` declared\n")
    mainPanel(
      tabsetPanel(
        type = "pill",
        tabPanel(
          title = translator_r()$t("Plot"),
          # prevent lazy loading
          loadOnActivate = FALSE,
          suspendWhenHidden = FALSE,
          # spacing hack
          h1("\n"),
          p(
            translator_r()$t(
              "Please select one or more policy domains, and then choose a specific policy from the drop-down menu below."
            ) # nolint
          ),
          # select the policy group to filter by
          div(
            style = "
                    display: flex;
                    align-items: end;
                    ",
            uiOutput(
              outputId = "select_domain",
              # Make the select_domain div's formatting match the reset button # nolint
              style = "align-items: bottom;"
            ),
          ),
          # select the filtered policies
          div(
            id = "policy-div",
            uiOutput(outputId = "policy")
          ),
          # plot
          div(
            id = "plot-container",
            style = "background: transparent !important;",
            plotOutput(
              "plot",
              width = "100%",
              height = "400px"
            )
          )
        ),
        tabPanel(
          title = translator_r()$t("Instructions"),
          h1("\n"),
          p(translator_r()$t("Welcome!")),
          p(
            translator_r()$t(
              "This interactive app allows you to explore the policy attitudes of specific demographic groups on the largest and most diverse set of municipal policy issues ever included in a survey of Canadians." # nolint
            )
          ),
          p(
            translator_r()$t(
              'In the first menu of the "Plot" tab above, select one or more policy domains. The second menu contains specific policy statements belonging to the policy domains you selected. Use the second menu to view public opinion on a specific policy.' # nolint
            )
          ),
          p(
            translator_r()$t(
              "Finally, adjust the set of characteristics in the panel to the left to see how different groups view the policy you have selected. When you're done, you can select a new policy by again using the menus above the plot." # nolint
            )
          )
        ),
        tabPanel(
          title = translator_r()$t("Details"),
          h1("\n"),
          p(
            translator_r()$t(
              "The data for this app come from the Canadian Municipal Barometer's annual Citizen Survey. Currently, it uses the 2025 data. It will soon be updated with more questions from the 2025 survey, and, in future years, new surveys will be added." # nolint
            )
          ),
          p(
            translator_r()$t(
              "Weights were constructed using iterative proportional fitting (see " # nolint
            ),
            a(
              translator_r()$t("DeBell and Krosnick", ),
              href = "https://www.electionstudies.org/wp-content/uploads/2018/04/nes012427.pdf", # nolint
              .noWS = c("after")
            ),
            ")."
          ),
          p(
            translator_r()$t(
              "Note that due to there being a small number of responses in Prince Edward Island, many of the policy issues for that province do not produce reliable estimates of public opinion. Sometimes this leads to odd results when Prince Edward Island is selected." # nolint
            )
          )
        )
      )
    )
  })
}
