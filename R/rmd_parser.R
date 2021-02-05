rmd_annotations <- function(parsed) {
  annotations <- parsed$annotations %>%
    map_df(~ as_tibble(.[c("row", "label", "state")])) %>%
    mutate(assign_ids(state)) %>%
    spread(state, row) %>%
    arrange(begin) %>%
    mutate(begin = begin + 1, end = end - 1) %>%
    rowwise() %>%
    mutate(source = parsed$source[begin:end] %>% paste(collapse = "\n")) %>%
    select(-begin,-end)

  chunks <- annotations %>%
    filter(!is.na(parent_id), label == "source") %>%
    transmute(parent_id, code = source) %>%
    full_join(
      annotations %>%
        select(-id) %>%
        filter(!is.na(parent_id), label != "source") %>%
        group_by(parent_id) %>%
        nest(outputs = c(label, source)) %>%
        ungroup,
      by = "parent_id"
    ) %>%
    ungroup %>%
    mutate(code = code %>% coalesce('<pre class="r"><code></code></pre>'))

  annotations %>%
    filter(is.na(parent_id)) %>%
    select(-parent_id) %>%
    left_join(chunks,
              by = c("id" = "parent_id")) %>%
    mutate(source = code %>% coalesce(source)) %>%
    select(-code)
}

assign_ids <- function(state) {
  tibble(state) %>%
    mutate(depth = cumsum(state == "begin") - lag(cumsum(state == "end"), default = 0),
           id_1 = cumsum(state == "begin" & depth == 1),
           id_2 = max(id_1) + cumsum(state == "begin" & depth == 2)) %>%
    transmute(id = if_else(depth == 1, id_1, id_2),
              parent_id = if_else(depth == 1, NA_integer_, id_1))
}
