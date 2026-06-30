# Unit tests for the pure helpers behind the multi-select / data-pooling
# feature in `R/helpers.R`.
#
# NOTE: like `test-load-data.R`, this repo isn't a real package, so we reach
# the app code by path. We resolve absolute paths up front (the working
# directory is still `tests/testthat/` at this point) and avoid `setwd()` so
# we don't disturb `test-load-data.R`, which runs afterwards and does its own
# relative `setwd("../../")`.
helpers_path <- normalizePath(testthat::test_path("..", "..", "R", "helpers.R"))
voter_data_path <- normalizePath(
  testthat::test_path("..", "..", "data", "voter-data.rds")
)
source(helpers_path)

test_that("recode_vec translates French labels and passes everything else through", {
  province_map <- c(
    "Québec" = "Quebec",
    "Nouvelle-Écosse" = "Nova Scotia"
  )
  # known French values are translated
  expect_equal(
    recode_vec(c("Québec", "Nouvelle-Écosse"), province_map),
    c("Quebec", "Nova Scotia")
  )
  # already-English / unmatched values are returned unchanged, even when mixed
  # in with translatable ones
  expect_equal(
    recode_vec(c("Québec", "Ontario"), province_map),
    c("Quebec", "Ontario")
  )
  # NULL / empty selections pass through untouched (treated as "all" downstream)
  expect_null(recode_vec(NULL, province_map))
  expect_equal(recode_vec(character(0), province_map), character(0))
})

test_that("un_translate_input translates multi-value selections and keeps empties", {
  input <- list(
    province = c("Québec", "Ontario"), # one French, one already English
    agecat = c("18-29", "30-44"), # never translated
    popcat = NULL, # empty -> "all" downstream
    gender = "Femme",
    race = c("Minorité racisée", "Blanc·che"),
    immigrant = "Non",
    homeowner = NULL,
    education = c("Baccalauréat", "High school"),
    income = "200,000 $ ou plus"
  )
  out <- un_translate_input(input)

  # the nine demographic slots are returned, in order
  expect_equal(
    names(out),
    c(
      "province", "agecat", "popcat", "gender", "race",
      "immigrant", "homeowner", "education", "income"
    )
  )
  # multi-value French/English selections come back fully in English
  expect_equal(out$province, c("Quebec", "Ontario"))
  expect_equal(out$race, c("Racialized minority", "White"))
  expect_equal(out$education, c("Bachelor's degree", "High school"))
  expect_equal(out$gender, "Woman")
  expect_equal(out$income, "$200,000 or more")
  # untranslated and empty slots are preserved as-is
  expect_equal(out$agecat, c("18-29", "30-44"))
  expect_null(out$popcat)
  expect_null(out$homeowner)
})

test_that("build_subgroup treats no selection as all values and pools by union", {
  svy <- readRDS(voter_data_path)
  # work within a single policy, mirroring how the app filters before pooling
  svy <- svy[svy$policy == svy$policy[1], , drop = FALSE]

  # an empty selection list leaves every variable unfiltered -> all rows
  expect_equal(nrow(build_subgroup(svy, list())), nrow(svy))

  # a NULL selection on a variable is the same as not selecting it
  expect_equal(nrow(build_subgroup(svy, list(agecat = NULL))), nrow(svy))

  # a single value filters to just that group
  one_age <- build_subgroup(svy, list(agecat = "18-29"))
  expect_equal(unique(as.character(one_age$agecat)), "18-29")

  # multiple values pool together (OR within a variable): the union of rows
  two_ages <- build_subgroup(svy, list(agecat = c("18-29", "30-44")))
  expect_equal(
    sort(unique(as.character(two_ages$agecat))),
    c("18-29", "30-44")
  )
  expect_equal(nrow(two_ages), sum(svy$agecat %in% c("18-29", "30-44")))

  # different variables combine with AND
  cross <- build_subgroup(
    svy,
    list(province = "Ontario", agecat = c("18-29", "30-44"))
  )
  expect_true(all(as.character(cross$province) == "Ontario"))
  expect_true(all(as.character(cross$agecat) %in% c("18-29", "30-44")))

  # a combination no respondent matches yields zero rows (drives the app's
  # "not in the data" message)
  expect_equal(
    nrow(build_subgroup(svy, list(province = "Ontario", agecat = "nope"))),
    0L
  )
})

test_that("pool_preds survey-weights probabilities into rounded percentages", {
  pred_probs <- matrix(
    c(
      0.8, 0.1, 0.1,
      0.2, 0.5, 0.3
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("Agree", "Disagree", "No opinion"))
  )

  # unequal weights: the first respondent counts three times as much
  # Agree = (0.8*3 + 0.2*1) / 4 = 0.65 -> 65, etc.
  expect_equal(
    pool_preds(pred_probs, weights = c(3, 1)),
    c(Agree = 65, Disagree = 20, "No opinion" = 15)
  )

  # equal weights reduce to a simple column mean
  expect_equal(
    pool_preds(pred_probs, weights = c(1, 1)),
    c(Agree = 50, Disagree = 30, "No opinion" = 20)
  )

  # a single matching respondent arrives as a named vector, not a matrix, and
  # must still pool to its own rounded percentages
  one <- c(Agree = 0.55, Disagree = 0.35, "No opinion" = 0.10)
  expect_equal(
    pool_preds(one, weights = 1),
    c(Agree = 55, Disagree = 35, "No opinion" = 10)
  )
})

test_that("empty_selections flags the menus with nothing selected, in order", {
  full <- list(
    province = "Ontario", popcat = "1,000,000+", gender = "Man",
    agecat = c("18-29", "30-44"), race = "White", immigrant = "No",
    homeowner = "Yes", education = "Bachelor's degree",
    income = "$50,000 - $99,999"
  )

  # every menu populated -> nothing flagged
  expect_equal(empty_selections(full), character(0))

  # both a NULL and a zero-length selection count as "nothing selected"
  expect_equal(empty_selections(modifyList(full, list(agecat = NULL))), "agecat")
  expect_equal(
    empty_selections(modifyList(full, list(race = character(0)))),
    "race"
  )

  # multiple empty menus are returned in sidebar (demographic_vars) order
  some <- modifyList(full, list(gender = NULL, income = character(0)))
  expect_equal(empty_selections(some), c("gender", "income"))
})
