.dir <- c("../Rminor/data"
          , "../Rminor_elevated/data"
)
purrr::walk(.dir, ~{
  .d <- .x
  if (!dir.exists(.d)) {
    dir.create(.d)
  }
  purrr::walk(list.files("images", full.names = TRUE), ~{
    .path <- fs::path(.d, basename(.x))
    if (file.exists(.path)) file.remove(.path)
    file.copy(.x, .path)
  })
})