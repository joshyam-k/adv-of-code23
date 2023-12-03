get_input <- function(day) {
  session <- Sys.getenv("ADV_SESSION")
  url <- paste0("https://adventofcode.com/2023/day/", day, "/input")
  req <- httr2::request(url)
  
  body_string <- req |>
    httr2::req_options(cookie = paste0("session=", session)) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    stringr::str_split("\n")
  
  tibble::tibble(input = head(body_string[[1]], -1))
}






