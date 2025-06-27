library(shiny)
library(shiny.i18n)

ui <- fluidPage(
  # formatting --------------------
  # set CSS for elements that don't accept a style argument in their function
  tags$head(
    tags$style(HTML(
      "
      body, html {
        height: 100%;
        margin: 0;
      }
      /* formats sidebarLayout */
      .shiny-layout {
        align-items: start; /* aligned with the top of the parent div */
        max-width: 80vw;
      }
      /* put the selected policy in bold so it serves as a title for the plot */
      #policy-div .selectize-input {
        font-weight: bold;
      }
      .col-sm-4 {
        max-width: 30%;
      }
      .col-sm-8 {
        max-width: 100%;
      }
      @media (max-width: 1100px) {
        .main-panel {
          width: auto;
          min-width: 400px;
        }
      }
    "
    )),
    tags$script(HTML(
      "
        <!-- Matomo -->
        var _paq = window._paq = window._paq || [];
        /* tracker methods like \"setCustomDimension\" should be called before \"trackPageView\" */
        _paq.push(['trackPageView']);
        _paq.push(['enableLinkTracking']);
        (function() {
          var u=\"https://cmbattitudesapp.matomo.cloud/\";
          _paq.push(['setTrackerUrl', u+'matomo.php']);
          _paq.push(['setSiteId', '1']);
          var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
          g.async=true; g.src='https://cdn.matomo.cloud/cmbattitudesapp.matomo.cloud/matomo.js'; s.parentNode.insertBefore(g,s);
        })();
      "
    ))
  ),
  # layout --------------------
  # set HTML divs to be formatted by the above CSS
  # CSS Flexbox formatting is being used on the next two divs such that the app
  # centers on the page while still stretching horizontally
  div(
    class = "flex-container",
    style = "
      display: flex;
      justify-content: center;
      height: 100%;
      width: 100%;
    ",
    div(
      class = "shiny-layout",
      div(
        id = "header",
        style = "
          display: flex;
          align-items: center;
          justify-content: space-between;
          height: 150px;
          max-width: 80vw;
        ",
        uiOutput("title"),
        a(
          img(
            src = "https://www.cmb-bmc.ca/wp-content/uploads/2024/09/logo-bmc-cmb.svg" # nolint
          ),
          href = "https://www.cmb-bmc.ca/"
        ),
      ),
      # the language toggle button is a custom feature and doesn't fit well with
      # any existing elements, so it gets its own div. It is also using some
      # cheeky CSS to overlap the title div of the tabPanel (in mainPanel).
      div(
        id = "lang_toggle",
        style = "
          display: flex;
          justify-content: flex-end;
          /* to move outside the bounds of its parent */
          position: relative;
        ",
        # language toggle
        # needs to defined statically to avoid circular reactivity
        # actionButton(
        #   "lang_toggle",
        #   "FR",
        #   style = "
        #       color: gray;
        #       font-weight: bold;
        #       border: 0px;
        #       /* to move outside the bounds of its parent */
        #       position: absolute;
        #       bottom: -37px;
        #       right: 2vw;
        #       /* to ensure the button is above the divs it overlaps */
        #       z-index: 1000;
        #     "
        # )
      ),
      sidebarLayout(
        fluid = TRUE,
        uiOutput("sidebar_contents"),
        div(
          class = "main-panel",
          style = "
            width: 80vw;
            min-width: 800px;
          ",
          uiOutput("mainpanel")
        )
      )
    )
  )
)
