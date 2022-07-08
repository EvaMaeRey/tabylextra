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
#'   janitor::tabyl(survived, sex) %>%
#'   janitor::adorn_totals(c("row", "col")) %>%
#'   janitor::adorn_percentages(denominator = "col") %>%
#'   janitor::adorn_pct_formatting() %>%
#'   janitor::adorn_ns(position = "front") %>%
#'   tabyl_pivot_longer(col_var = "sex")
#'
#'  datasets::Titanic %>%
#'   data.frame() %>%
#'   janitor::clean_names() %>%
#'   tidyr::uncount(weights = freq) %>%
#'   janitor::tabyl(survived, sex,class) %>%
#'   janitor::adorn_totals(c("row", "col")) %>%
#'   janitor::adorn_percentages(denominator = "col") %>%
#'   janitor::adorn_pct_formatting() %>%
#'   janitor::adorn_ns(position = "front") %>%
#'   tabyl_pivot_longer(col_var = "sex", list_var = "class")
#'
#'library(ggplot2)
#'  datasets::Titanic %>%
#'   data.frame() %>%
#'   janitor::clean_names() %>%
#'   tidyr::uncount(weights = freq) %>%
#'   janitor::tabyl(survived, sex, class)  %>%
#'   tabyl_pivot_longer(col_var = "sex", list_var = "class") %>%
#'   ggplot() +
#'   aes(x = sex, y = survived) +
#'   facet_wrap(~class) +
#'   geom_tile() +
#'   aes(fill = value) +
#'   geom_text(aes(label = value), color = "white")
#'
#'  datasets::Titanic %>%
#'   data.frame() %>%
#'   janitor::clean_names() %>%
#'   tidyr::uncount(weights = freq) %>%
#'   janitor::tabyl(survived, sex,class) %>%
#'   janitor::adorn_totals(c("row", "col")) %>%
#'   janitor::adorn_percentages(denominator = "col") ->
#'   percentages_prep
#'
#'   percentages_prep %>%
#'   janitor::adorn_pct_formatting() %>%
#'   janitor::adorn_ns(position = "front") %>%
#'   tabyl_pivot_longer(col_var = "sex", list_var = "class") %>%
#'   ggplot() +
#'   aes(x = sex, y = survived %>% forcats::fct_rev()) +
#'   facet_wrap( ~ class) +
#'   geom_tile(data = percentages_prep %>%
#'   tabyl_pivot_longer(col_var = "sex", list_var = "class"),
#'   aes(fill = value)) +
#'   geom_text(aes(label = value), color = "white") +
#'   scale_fill_viridis_c(limits = 0:1)


tabyl_pivot_longer <- function(tabyl, col_var = NULL, list_var = NULL){

  if(class(tabyl)[1] != "list"){

  tabyl %>%
    tidyr::pivot_longer(cols = -1) %>%
    dplyr::mutate(across(1:2, ~ .x %>% forcats::fct_inorder())) ->
  out

  if(!is.null(col_var)){names(out)[2] <- col_var}

  out

  } else {

    tabyl %>%
      tabyl_three_way_flatten() %>%
      tidyr::pivot_longer(cols = 3:(ncol(tabyl[[1]])+1),
                   names_to = "cat2") %>%
      dplyr::mutate(across(1:3, ~ .x %>% forcats::fct_inorder())) ->
      out

    if(!is.null(col_var)){names(out)[3] <- col_var}
    if(!is.null(list_var)){names(out)[1] <- list_var}

    out

  }

}
