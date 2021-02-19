
#' @importFrom readr read_file write_file
#' @importFrom rmarkdown pandoc_convert
html_to_md <- function(html_text) {
  in_file <- tempfile(fileext = ".html")
  on.exit(file.remove(in_file))
  html_text %>% write_file(in_file)

  out_file <- tempfile(fileext = ".md")
  on.exit(if(file.exists(out_file)) file.remove(out_file),
          add = T)

  pandoc_convert(in_file, from = "html", to = "markdown", output = out_file)
  read_file(out_file)
}

#' @importFrom purrr transpose
#' @importFrom jsonlite fromJSON
#' @importFrom rlang set_names
parse_pagedtable_json <- function(json) {
  table_js <- fromJSON(json, simplifyVector = F)
  tab <- table_js$data %>%
    transpose() %>%
    map(unlist) %>%
    as_tibble()
  cols <- table_js$columns %>%
    transpose() %>%
    map(unlist) %>%
    as_tibble()
  tibble(name = tab %>% colnames()) %>%
    inner_join(cols %>%
                 mutate(
                   label = glue::glue("{label} <{type}>"),
                   name = as.character(name)
                 ), by = "name") %>%
    with(label) %>%
    set_names(tab, .)
}

#' @importFrom xml2 read_html
#' @importFrom readr write_lines
parse_html <- function(html_text) {
  html_file <- tempfile(fileext = ".html")
  on.exit(file.remove(html_file))
  html_text %>% write_lines(html_file)
  read_html(html_file)

}
