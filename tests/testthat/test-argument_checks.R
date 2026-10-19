# check_flag ------------------------------------------------------------------------------------

test_that("check_flag accepts TRUE and FALSE and returns them invisibly", {
  expect_invisible(check_flag(TRUE))
  expect_identical(
    object = check_flag(FALSE),
    expected = FALSE
  )
})

test_that("check_flag rejects NA, non-logicals and non-scalars", {
  flag = NA
  two_flags = c(TRUE, FALSE)

  expect_error(
    object = check_flag(flag),
    regexp = "Invalid argument flag: expected TRUE or FALSE, received logical of length 1 (NA)",
    fixed = TRUE
  )
  expect_error(
    object = check_flag("yes"),
    regexp = "received character of length 1 (\"yes\")",
    fixed = TRUE
  )
  expect_error(
    object = check_flag(two_flags),
    regexp = "received logical of length 2 (TRUE, FALSE)",
    fixed = TRUE
  )
  expect_error(
    object = check_flag(NULL),
    regexp = "received NULL",
    fixed = TRUE
  )
  expect_error(
    object = check_flag(
      x = 1,
      arg = "overwrite"
    ),
    regexp = "Invalid argument overwrite:",
    fixed = TRUE
  )
})


# check_number ----------------------------------------------------------------------------------

test_that("check_number accepts finite numbers within bounds", {
  expect_invisible(check_number(0.5))
  expect_identical(
    object = check_number(3L),
    expected = 3L
  )
  expect_identical(
    object = check_number(
      x = 1,
      lower = 1,
      upper = 1
    ),
    expected = 1
  )
  expect_identical(
    object = check_number(
      x = 4,
      integerish = TRUE
    ),
    expected = 4
  )
  expect_null(
    check_number(
      x = NULL,
      allow_null = TRUE
    )
  )
})

test_that("check_number rejects non-numbers, non-finite values and wrong lengths", {
  n = "1"
  two_numbers = c(1, 2)

  expect_error(
    object = check_number(n),
    regexp = "Invalid argument n: expected a single finite number, received character of length 1 (\"1\")",
    fixed = TRUE
  )
  expect_error(
    object = check_number(NA_real_),
    regexp = "received numeric of length 1 (NA)",
    fixed = TRUE
  )
  expect_error(
    object = check_number(Inf),
    regexp = "expected a single finite number, received numeric of length 1 (Inf)",
    fixed = TRUE
  )
  expect_error(
    object = check_number(two_numbers),
    regexp = "received numeric of length 2 (1, 2)",
    fixed = TRUE
  )
  expect_error(
    object = check_number(TRUE),
    regexp = "received logical of length 1 (TRUE)",
    fixed = TRUE
  )
  expect_error(
    object = check_number(NULL),
    regexp = "received NULL",
    fixed = TRUE
  )
})

test_that("check_number enforces bounds and integerish", {
  expect_error(
    object = check_number(
      x = 2,
      upper = 1
    ),
    regexp = "expected a number in [-Inf, 1], received numeric of length 1 (2)",
    fixed = TRUE
  )
  expect_error(
    object = check_number(
      x = -1,
      lower = 0
    ),
    regexp = "expected a number in [0, Inf]",
    fixed = TRUE
  )
  expect_error(
    object = check_number(
      x = 1.5,
      integerish = TRUE
    ),
    regexp = "expected a single finite whole number, received numeric of length 1 (1.5)",
    fixed = TRUE
  )
})


# check_named_list ------------------------------------------------------------------------------

test_that("check_named_list accepts uniquely named lists", {
  named_list = list(
    a = 1,
    b = 2
  )

  expect_invisible(check_named_list(named_list))
  expect_identical(
    object = check_named_list(list()),
    expected = list()
  )
  expect_null(
    check_named_list(
      x = NULL,
      allow_null = TRUE
    )
  )
})

test_that("check_named_list rejects non-lists, data frames and disallowed empty lists", {
  cfg = c(a = 1)
  df = data.frame(a = 1)

  expect_error(
    object = check_named_list(cfg),
    regexp = "Invalid argument cfg: expected a named list, received numeric of length 1 (1)",
    fixed = TRUE
  )
  expect_error(
    object = check_named_list(df),
    regexp = "received data.frame of length 1",
    fixed = TRUE
  )
  expect_error(
    object = check_named_list(NULL),
    regexp = "received NULL",
    fixed = TRUE
  )
  expect_error(
    object = check_named_list(
      x = list(),
      allow_empty = FALSE
    ),
    regexp = "expected a non-empty named list, received an empty list",
    fixed = TRUE
  )
})

test_that("check_named_list reports unnamed and duplicated elements", {
  unnamed_list = list(1, 2)
  partly_named_list = list(a = 1, 2, b = 3)
  duplicated_list = list(a = 1, b = 2, a = 3)

  expect_error(
    object = check_named_list(unnamed_list),
    regexp = "expected every element to be named, received unnamed elements at positions: 1, 2",
    fixed = TRUE
  )
  expect_error(
    object = check_named_list(partly_named_list),
    regexp = "unnamed elements at positions: 2",
    fixed = TRUE
  )
  expect_error(
    object = check_named_list(duplicated_list),
    regexp = "expected unique element names, received duplicated names: a",
    fixed = TRUE
  )
})
