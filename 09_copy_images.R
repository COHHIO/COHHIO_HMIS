.dir <- c("../Rminor/data"
          , "../Rminor_elevated/data"
)
purrr::walk(.dir, ~{
  .d <- .x
  if (!dir.exists(.d)) {
    dir.create(.d)
  }
  purrr::walk(list.files("images", full.names = TRUE), ~{
    file.copy(.x, fs::path(.d, basename(.x)))
  })
})