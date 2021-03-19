#' Pull Down Industry Information
#' @param url the url needed
#' @param industry_code the industry code to search
#' @param id the id to be associated
#' @export
#'

pull_industry_information <- function(url, industry_code, id){
  url <- "https://accessnc.nccommerce.com/business/business_custom_search_infogroup.html"

  main_page <- rvest::html_session(url)

  form.unfilled <- main_page %>%
    rvest::html_node("form") %>%
    rvest::html_form()

  form.filled <- form.unfilled %>%
    rvest::set_values("naics" = industry_code)

  session <- rvest::submit_form(main_page, form.filled)

  extra_out<-list()
  i<- 1
  while(session$response$status_code==200){


      extra_out[[i]] <- session %>%
        rvest::html_nodes("table") %>%
        .[[1]] %>%
        rvest::html_table() %>%
        janitor::clean_names() %>%
        tibble::as_tibble() %>%
        dplyr::mutate(industry_code = industry_code,
                      id = id)

      i <- i +1
      session <- follow_next(session = session)
    }


  out_2 <- dplyr::bind_rows(extra_out)

  output <- out_2

  output

}
#test <- pull_industry_information(url = url, industry_code = "4543", "test")

follow_next <- function(session, text ="Next", ...) {
  link <- html_node(session, css = sprintf("body > div.container.body-content > div > ul > li.PagedList-skipToNext > a", text))
  url <- html_attr(link, "href")
  url = trimws(url)
  url = gsub("^\\.{1}/", "", url)
  message("Navigating to ", url)
  jump_to(session, url, ...)
}
