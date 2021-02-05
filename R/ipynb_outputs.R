ipynb_out_stream <- function(text, stdout = T) {
  list(
    output_type = "stream",
    metadata = NULL,
    name = if (stdout)
      "stdout"
    else
      "stderr",
    text = text
  )
}

ipynb_out_img <- function(img_base64) {
  list(
    output_type = "display_data",
    metadata = NULL,
    data = list(
      `image/png` = img_base64
    )
  )
}

ipynb_out_table <- function(table) {
  list(
    output_type = "display_data",
    metadata = NULL,
    data = list(
      `text/markdown` = pander::pandoc.table.return(table, style = "rmarkdown")
    )
  )
}
