# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

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

