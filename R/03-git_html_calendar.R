#' Get all projects from Github Organisation
#'
#' Obtains list from Github Organisation
#'
#' @param token token with access to repos and projects
#' @param org Organisation name
#'
#' @importFrom ghql Query GraphqlClient
#' @importFrom dplyr filter relocate select mutate rename as_tibble n distinct summarise group_by arrange ungroup
#' @importFrom tidyr spread unnest gather
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate as_datetime hours
#'
#'
#' @return Projects
#' @export
#'
#'



git_html_calendar = function(projects){
 statuses = c("To Do", "Working On", "Review", "Done")
 all <- projects %>%
  filter(!is.na(Due)) %>%
  group_by(project, title, url, Status, Due) %>%
  summarise(
    # Make lists at deepest point
    staff = p(li_flatten(login)),
    .groups = "drop_last"
  ) %>%
  ungroup() %>%
  mutate(Status = factor(Status, statuses, ordered = T)) %>%
  arrange(Status) %>%
  mutate(Status = p(as.character(Status))) %>%
  transform(url = paste('<a href = ', shQuote(url), '>', title, '</a>')) %>%
  gather(type, html, - c(project, Due, title)) %>%
  mutate(type = factor(type, unique(type, ordered = T))) %>%
  group_by(project,Due, title) %>%
  arrange(type) %>%
  summarise(body = (li_flatten(na.omit(html)))) %>%
  summarise(body = ifelse(grepl("<p>Done</p>", body), s(body), body)) %>%
  group_by(project,Due) %>%
  summarise(body = div(li_flatten(na.omit(body)))) %>%
  select(project, body, Due) %>%
  distinct() %>%
  mutate(calendarId = 1:n(),
         backgroundColor = NA,
         color = NA,
         borderColor = NA,
         recurrenceRule = NA,
         start = as_datetime(Due) + hours(6),
         end = start,
         category = "time",
         title = project) %>%
  filter(!is.na(start))
  return(all)
}

make_html_tag <- function(tag) {
  left  <- sprintf("<%s>",  tag)
  right <- sprintf("</%s>", tag)
  function(xs) {
    ifelse(!is.na(xs), paste0(left, xs, right), NA_character_)
  }
}

ul <- make_html_tag("ul")
li <- make_html_tag("li")
p  <- make_html_tag("p")
div  <- make_html_tag("div")
s <- make_html_tag("s")

# tag elements as li's and flatten
li_flatten <- function(xs) {
  stringr::str_flatten((xs), "\n")
}
