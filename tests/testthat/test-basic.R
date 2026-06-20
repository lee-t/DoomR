test_that("Functions are exported and exist", {
  expect_true(exists("read_lump_directory"))
  expect_true(exists("read_vertexes"))
  expect_true(exists("read_linedefs"))
  expect_true(exists("read_sidedefs"))
  expect_true(exists("read_segs"))
  expect_true(exists("read_ssectors"))
  expect_true(exists("read_nodes"))
  expect_true(exists("read_sectors"))
  expect_true(exists("doom_render"))
})

test_that("doom_render fails gracefully with missing WAD", {
  expect_error(doom_render(wad_path = "nonexistent.wad"))
})

test_that("doom_render works with the local DOOM.WAD if present", {
  # Look for DOOM.WAD in parent directories or current directory
  possible_paths <- c(
    "DOOM.WAD",
    "../DOOM.WAD",
    "../../DOOM.WAD",
    "/home/runner/work/DoomR/DoomR/DOOM.WAD"
  )
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
