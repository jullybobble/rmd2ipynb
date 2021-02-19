#' Convert an RMarkdown HTML notebook to an IPython (or Jupyter) notebook.
#'
#' Note that you must pass the HTML file with extension `.nb.html` that is rendered
#' automatically for RMarkdown documents with output with `html_notebook` output.
#' Passing the RMarkdown document directly will fail.
#'
#' @param html_notebook_file path to the RMarkdown HTML document to be converted
#' @param ipynb_file path to the desired output IPython notebook to be generated
#'
#' @return the resulting JSON of the IPython notebook, invisibly, as a character
#' vector
#'
#' @export
#' @importFrom magrittr `%>%`
#' @importFrom readr write_file
#' @importFrom purrr map map_df
#' @importFrom stringr str_replace
#' @importFrom rmarkdown parse_html_notebook
#' @importFrom xml2 read_html xml_find_first
#' @importFrom rvest html_text
#' @import dplyr tidyr
html_nb_to_ipynb <- function(html_notebook_file, ipynb_file = str_replace(html_notebook_file, "\\.nb\\.html$", ".ipynb"), verbose = F, remove_html_comments = T) {
  stopifnot(file.exists(html_notebook_file))
  parsed <- parse_html_notebook(html_notebook_file)
  title <- parsed$header %>% parse_html() %>% xml_find_first("/html/head/title") %>% html_text()
  json <- parsed %>%
    rmd_annotations() %>%
    ipynb_from_rmd(title, remove_html_comments)

  write_file(json, ipynb_file)
  if(verbose) {
    message("wrote file ", ipynb_file)
  }
  invisible(json)
}

#' @importFrom jsonlite toJSON
ipynb_from_rmd <- function(annotations, title = NULL, remove_html_comments = T) {
  cells <- annotations %>%
    rowwise %>%
    group_map( ~ ipynb_cell_from_rmd_part(.x$label, .x$source, .x$outputs, remove_html_comments)) %>%
    discard(is.null)

  title_cell <- if(is.null(title)) {
    list()
  } else {
    list(ipynb_cell_markdown(paste("#", title)))
  }

  list(
    nbformat = 4,
    nbformat_minor = 4,
    metadata = list(
      kernelspec = list(
        display_name = "R",
        language = "R",
        name = "ir"
      ),
      language_info = list(
        codemirror_mode = "r",
        file_extension = ".r",
        mimetype = "text/x-r-source",
        name = "R",
        pygments_lexer = "r",
        version = paste(version$major, sep = ".", version$minor)
      )
    ),
    cells = c(title_cell, cells)
  ) %>%
    toJSON(auto_unbox = T,
           pretty = T,
           na = "null")
}

#' @importFrom xml2 xml_find_first
#' @importFrom rvest html_text html_attr
#' @importFrom stringr str_detect str_remove str_remove_all
#' @importFrom purrr discard
ipynb_cell_from_rmd_part <- function(label, source, outputs, remove_html_comments) {
  if (label == "text") {
    if(source %>% str_detect('^<div id="rmd-source-code">')) {
      NULL
    } else {
      md <- html_to_md(source)
      if(remove_html_comments) {
        md <- str_remove_all(md, "(?m)^:::.*\n")
      }
      if(str_detect(md, "^\\s*$")) {
        NULL
      } else {
        ipynb_cell_markdown(md)
      }
    }
  } else if (label == "chunk") {
    code <- if(source == "") {
      NULL
    } else {
      source %>% read_html() %>% html_text()
    }
    out <- if(!is.null(outputs[[1]])) {
      outputs[[1]] %>%
        rowwise %>%
        group_map( ~ ipynb_cell_from_rmd_part(.x$label, .x$source, .x$outputs, remove_html_comments)) %>%
        discard(is.null)
    } else {
      NULL
    }
    ipynb_cell_code(code, out)
  } else if (label == "plot")  {
    img_base64 <- source %>%
      read_html %>%
      xml_find_first("//img") %>%
      html_attr("src") %>%
      str_remove("data:image/png;base64,")
    ipynb_out_img(img_base64)
  } else if (label == "output")  {
    text <- source %>%
      read_html %>%
      xml_find_first("//pre/code") %>%
      html_text()
    ipynb_out_stream(text)
  } else if (label == "frame")  {
    table <- source %>%
      read_html() %>%
      html_text() %>%
      parse_pagedtable_json()
    ipynb_out_table(table)
  } else  {
    NULL
  }
}

