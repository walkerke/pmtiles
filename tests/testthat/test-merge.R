# pm_merge(): input validation plus a real end-to-end merge when tippecanoe
# is available to build fixture archives.

test_that("pm_merge validates its inputs", {
  expect_error(pm_merge("only-one.pmtiles", "out.pmtiles"), "at least two")
  expect_error(pm_merge(character(), "out.pmtiles"), "at least two")
  expect_error(
    pm_merge(c("nope-a.pmtiles", "nope-b.pmtiles"), "out.pmtiles"),
    "not found"
  )
  a <- tempfile(fileext = ".pmtiles")
  file.create(a)
  on.exit(unlink(a), add = TRUE)
  expect_error(pm_merge(c(a, "nope-b.pmtiles"), "out.pmtiles"), "nope-b")
  expect_error(pm_merge(c(a, a), output = ""), "single file path")
})

test_that("pm_merge combines two disjoint archives", {
  skip_if(Sys.which("tippecanoe") == "", "tippecanoe not available")

  # two points per file so each archive gets non-degenerate bounds
  points_geojson <- function(lon1, lon2) {
    pt <- '{"type":"Feature","properties":{"name":"pt"},"geometry":{"type":"Point","coordinates":[%f,%f]}}'
    sprintf(
      '{"type":"FeatureCollection","features":[%s,%s]}',
      sprintf(pt, lon1, 39), sprintf(pt, lon2, 41)
    )
  }

  dir <- tempfile("merge-test")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  west_gj <- file.path(dir, "west.geojson")
  east_gj <- file.path(dir, "east.geojson")
  writeLines(points_geojson(-101, -100), west_gj)
  writeLines(points_geojson(100, 101), east_gj)

  west <- file.path(dir, "west.pmtiles")
  east <- file.path(dir, "east.pmtiles")
  # zoom 4 only: points on opposite sides of the world share no tiles,
  # so the archives are disjoint as pm_merge requires
  suppressMessages({
    pm_create(west_gj, west, min_zoom = 4, max_zoom = 4, quiet = TRUE)
    pm_create(east_gj, east, min_zoom = 4, max_zoom = 4, quiet = TRUE)
  })
  expect_true(file.exists(west) && file.exists(east))

  merged <- file.path(dir, "merged.pmtiles")
  suppressMessages(out <- pm_merge(c(west, east), merged))

  expect_identical(out, merged)
  expect_true(file.exists(merged))
  # the merged archive is structurally valid and spans both points
  expect_output(pm_verify(merged))
  header <- pm_show(merged, header_json = TRUE)
  expect_lt(header$bounds[[1]], -99)
  expect_gt(header$bounds[[3]], 99)
})
