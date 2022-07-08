

#' Title
#'
#' @param tabyl
#'
#' @return
#' @export
#'
#' @examples
#' datasets::Titanic %>%
#'   data.frame() %>%
#'   janitor::clean_names() %>%
#'   tidyr::uncount(weights = freq) %>%
#'   janitor::tabyl(survived, class, sex) %>%
#'   janitor::adorn_totals(c("row", "col")) %>%
#'   janitor::adorn_percentages(denominator = "col") %>%
#'   janitor::adorn_pct_formatting() %>%
#'   janitor::adorn_ns(position = "front") %>%
#'   tabyl_three_way_flatten()
#'
#' datasets::Titanic %>%
#'   data.frame() %>%
#'   janitor::clean_names() %>%
#'   tidyr::uncount(weights = freq) %>%
#'   janitor::tabyl(survived, class, sex) %>%
#'   janitor::adorn_totals(c("row", "col")) %>%
#'   tabyl_three_way_flatten(list_var = "sex")
#'
#'
#' @param tabyl

tabyl_three_way_flatten <- function(tabyl, list_var = NULL){

  names(tabyl) ->
    tabyl_names

  for (i in 1:length(tabyl)){

  tabyl[[i]] %>%
      dplyr::mutate(cat = tabyl_names[i]) %>%
      dplyr::select(cat, everything()) ->
  tabyl[[i]]

  }

  my_tibble <- data.frame()

  for (i in 1:length(tabyl)){

    my_tibble %>%
      dplyr::bind_rows(tabyl[[i]]) ->
    my_tibble

  }

  if(!is.null(list_var)){

   names(my_tibble)[1] <- list_var

  }

  return(my_tibble)

}
