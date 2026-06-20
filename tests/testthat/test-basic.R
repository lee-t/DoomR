test_that("Functions are exported and exist", {
  exports <- getNamespaceExports("DoomR")
  expect_true("read_lump_directory" %in% exports)
  expect_true("read_vertexes" %in% exports)
  expect_true("read_linedefs" %in% exports)
  expect_true("read_sidedefs" %in% exports)
  expect_true("read_segs" %in% exports)
  expect_true("read_ssectors" %in% exports)
  expect_true("read_nodes" %in% exports)
  expect_true("read_sectors" %in% exports)
  expect_true("doom_render" %in% exports)
})

test_that("doom_render fails gracefully with missing WAD", {
  expect_error(doom_render(wad_path = "nonexistent.wad"))
})

test_that("doom_render returns expected output structure with valid DOOM.WAD", {
  # This test requires DOOM.WAD to run end-to-end.
  # If DOOM.WAD is not present, it will gracefully skip, ensuring compatibility
  # across different checking environments.
  # Look for DOOM.WAD in parent directories or current directory
  possible_paths <- c(
    "DOOM.WAD",
    "../DOOM.WAD",
    "../../DOOM.WAD"
  )
  github_workspace <- Sys.getenv("GITHUB_WORKSPACE")
  if (github_workspace != "") {
    possible_paths <- c(possible_paths, file.path(github_workspace, "DOOM.WAD"))
  }
  wad_path <- NULL
  for (path in possible_paths) {
    if (file.exists(path)) {
      wad_path <- path
      break
    }
  }

  if (!is.null(wad_path)) {
    res <- doom_render(wad_path = wad_path, verbose = FALSE)
    expect_type(res, "list")
    expect_s3_class(res$map_plot, "ggplot")
    expect_s3_class(res$ray_plot, "ggplot")
    expect_s3_class(res$render_plot, "ggplot")
    expect_type(res$player_sector, "integer")
    expect_type(res$player_subsector, "integer")
  } else {
    skip("DOOM.WAD not found, skipping end-to-end test.")
  }
})
