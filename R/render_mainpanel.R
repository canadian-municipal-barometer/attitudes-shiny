library(shiny)

render_mainpanel <- function(translator_r, statements_r) {
  renderUI({
    message("\n`mainpanel` declared\n")
    mainPanel(
      tabsetPanel(
        type = "pill",
        # open on the Instructions tab; explicit `value`s keep the default
        # stable across the FR/EN toggle (which re-renders this UI)
        selected = "instructions",
        tabPanel(
          title = translator_r()$t("Plot"),
          value = "plot",
          # prevent lazy loading
          loadOnActivate = FALSE,
          suspendWhenHidden = FALSE,
          # spacing hack
          h1("\n"),
          p(
            translator_r()$t(
              "Please select a policy domain, and then choose a specific policy from the drop-down menu below."
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
          # survey year the selected statement was asked in (populated in server)
          div(
            style = "margin: 4px 0 8px; color: #6b7280; font-style: italic;",
            textOutput("policy_year", inline = TRUE)
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
          value = "instructions",
          h1("\n"),
          p(translator_r()$t("Welcome!")),
          p(
            translator_r()$t(
              "This interactive app allows you to explore the policy attitudes of specific demographic groups on the largest and most diverse set of municipal policy issues ever included in a survey of Canadians." # nolint
            )
          ),
          p(
            translator_r()$t(
              'In the first menu of the "Plot" tab above, select a policy domain. The second menu contains specific policy statements belonging to the policy domain you selected. Use the second menu to view public opinion on a specific policy.' # nolint
            )
          ),
          p(
            translator_r()$t(
              "Select characteristics in the panel to the left to see how different groups view the selected policy. You can change which policy you are viewing at any time using the policy menus above the plot." # nolint
            )
          ),
          p(
            translator_r()$t(
              "Use the Socio-demographics panel on the left to choose the group whose opinions you want to see. Pick one or more options in each menu; selecting several options in a menu (for example, two age groups) pools them together into a single combined group. At least one option must be selected in every menu for an estimate to appear. The dropdown menus each have Select all and Clear buttons, and the Clear button at the bottom of the panel empties every menu at once." # nolint
            )
          ),
          p(
            translator_r()$t(
              "In the plot, the coloured bars show the estimate for the group you selected, and the grey bars show the national average for comparison. With everything selected, the two match, because your selection covers the whole population." # nolint
            )
          )
        ),
        tabPanel(
          title = translator_r()$t("Details"),
          value = "details",
          h1("\n"),
          p(
            translator_r()$t(
              "The data for this app come from the Canadian Municipal Barometer's annual Citizen Survey. Use the Survey year filter to choose which survey wave(s) to view; each policy statement shows the year it was asked. New survey waves are added over time." # nolint
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
