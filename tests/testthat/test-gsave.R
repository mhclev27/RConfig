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

png_pixel_dims = function(path) {
  if (requireNamespace("png", quietly = TRUE)) {
    image = png::readPNG(path)
    return(c(width = dim(image)[2], height = dim(image)[1]))
  }
  header = readBin(
    con = path,
    what = "raw",
    n = 24
  )
  ihdr_bytes = header[17:24]
  width_bytes = ihdr_bytes[1:4]
  height_bytes = ihdr_bytes[5:8]
  byte_weights = 256^(3:0)
  c(
    width = sum(as.integer(width_bytes) * byte_weights),
    height = sum(as.integer(height_bytes) * byte_weights)
  )
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

test_that("gsave PNG pixel dimensions match an explicit Cairo png device", {
  skip_if_not(isTRUE(capabilities("cairo")))
  out_dir = local_test_dir()
  test_plot = make_test_plot()

  suppressMessages(
    capture.output(
      gsave(
        title = "dims_plot",
        plot = test_plot,
        dir = out_dir,
        w = 3,
        h = 2,
        dfs = NULL,
        dpi = 150,
        additional_info = FALSE,
        type = ".png"
      )
    )
  )

  reference_path = file.path(out_dir, "reference.png")
  grDevices::png(
    filename = reference_path,
    width = 3,
    height = 2,
    units = "in",
    res = 150,
    type = "cairo"
  )
  print(test_plot)
  grDevices::dev.off()

  gsave_dims = png_pixel_dims(file.path(out_dir, "dims_plot.png"))
  reference_dims = png_pixel_dims(reference_path)
  expect_equal(
    object = gsave_dims,
    expected = reference_dims
  )
  expect_equal(
    object = unname(gsave_dims),
    expected = c(450, 300)
  )
})

test_that("gsave writes a PNG at a 333-character path with ragg installed", {
  skip_on_os(c("mac", "linux"))
  skip_if_not_installed("ragg")
  skip_if_not(isTRUE(capabilities("cairo")))

  long_dir = file.path(
    local_test_dir(),
    strrep("a", 100),
    strrep("b", 100)
  )
  dir.create(
    path = long_dir,
    recursive = TRUE
  )
  title_length = 333 - nchar(long_dir) - nchar("/.png")
  long_title = strrep("c", title_length)
  png_path = file.path(long_dir, paste0(long_title, ".png"))
  expect_equal(
    object = nchar(png_path),
    expected = 333
  )

  test_plot = make_test_plot()
  expect_no_error(
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
    )
  )
  expect_true(file.exists(png_path))
  expect_gt(
    object = file.size(png_path),
    expected = 0
  )
  expect_equal(
    object = unname(png_pixel_dims(png_path)),
    expected = c(144, 144)
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
