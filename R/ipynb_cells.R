ipynb_cell_markdown <- function(md) {
  list(cell_type = "markdown",
       metadata = NULL,
       source = md)
}

ipynb_cell_code <- function(code, outputs) {
  list(
    cell_type = "code",
    execution_count = NA_integer_,
    metadata = list(collapsed = F,
                    scrolled = "auto"),
    source = code,
    outputs = outputs
  )
}

