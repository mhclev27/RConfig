# gsave_Output_Check ----------------------------------------------------------------------------

local_test_dir = function(env = parent.frame()) {
  test_dir = tempfile("gsave_test_")
  dir.create(test_dir)
  cleanup_call = call(
    "unlink",
    x = test_dir,
    recursive = TRUE
  )
  do.call(
    what = on.exit,
    args = list(
      expr = cleanup_call,
      add = TRUE
    ),
    envir = env
  )
  normalizePath(
    path = test_dir,
    winslash = "/"
  )
}

make_test_plot = function() {
  plot_df = data.frame(
    x = 1:3,
    y = c(2, 4, 3)
  )
  ggplot2::ggplot(
    data = plot_df,
    mapping = ggplot2::aes(
      x = .data[["x"]],
      y = .data[["y"]]
    )
  ) +
    ggplot2::geom_point()
}

test_that("gsave passes the output check on a normal save", {
  out_dir = local_test_dir()
  test_plot = make_test_plot()
  plot_dfs = list(points = test_plot$data)

  expect_no_error(
    suppressMessages(
      capture.output(
        gsave(
          title = "normal_plot",
          plot = test_plot,
          dir = out_dir,
          w = 2,
          h = 2,
          dfs = plot_dfs,
          dpi = 72,
          desc = "test plot",
          type = ".png"
        )
      )
    )
  )

  component_dir = file.path(out_dir, "normal_plot")
  expected_paths = c(
    file.path(out_dir, "normal_plot.png"),
    file.path(component_dir, "plot.pdf"),
    file.path(component_dir, "plotObject.RDS"),
    file.path(component_dir, "plotDF_points.csv"),
    file.path(component_dir, "readme.txt")
  )
  expect_true(all(file.exists(expected_paths)))
})

test_that("gsave errors when the PNG path is 260+ characters on Windows", {
  skip_on_os(c("mac", "linux"))
  skip_if_not_installed("ragg")

  long_dir = file.path(
    local_test_dir(),
    strrep("a", 100),
    strrep("b", 100)
  )
  dir.create(
    path = long_dir,
    recursive = TRUE
  )
  long_title = strrep("c", 80)
  png_path = file.path(long_dir, paste0(long_title, ".png"))
  expect_gte(nchar(png_path), 260)

  test_plot = make_test_plot()
  save_error = expect_error(
    suppressMessages(
      capture.output(
        gsave(
          title = long_title,
          plot = test_plot,
          dir = long_dir,
          w = 2,
          h = 2,
          dfs = NULL,
          dpi = 72,
          additional_info = FALSE,
          type = ".png"
        )
      )
    ),
    regexp = "missing or empty"
  )
  expect_match(
    object = conditionMessage(save_error),
    regexp = png_path,
    fixed = TRUE
  )
  expect_match(
    object = conditionMessage(save_error),
    regexp = paste0("(", nchar(png_path), " characters)"),
    fixed = TRUE
  )
})

test_that("gsave errors listing each output a mocked device failed to write", {
  out_dir = local_test_dir()
  local_mocked_bindings(
    ggsave = function(...) invisible(NULL),
    .package = "RConfig"
  )
  test_plot = make_test_plot()
  plot_path = file.path(out_dir, "mocked_plot.jpeg")
  pdf_path = file.path(out_dir, "mocked_plot", "plot.pdf")

  save_error = expect_error(
    suppressMessages(
      capture.output(
        gsave(
          title = "mocked_plot",
          plot = test_plot,
          dir = out_dir,
          w = 2,
          h = 2,
          dfs = NULL
        )
      )
    ),
    regexp = "260 characters or longer"
  )
  save_message = conditionMessage(save_error)
  expect_match(
    object = save_message,
    regexp = plot_path,
    fixed = TRUE
  )
  expect_match(
    object = save_message,
    regexp = pdf_path,
    fixed = TRUE
  )
  expect_no_match(
    object = save_message,
    regexp = "plotObject.RDS",
    fixed = TRUE
  )
})
