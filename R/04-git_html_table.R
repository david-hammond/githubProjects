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
#' @importFrom lubridate as_datetime hours today
#' @importFrom bizdays bizdays create.calendar remove_calendars
#' @importFrom kableExtra cell_spec
#'
#'
#' @return Projects
#' @export
#'
#'



git_html_table = function(projects){
 statuses = c("To Do", "Working On", "Review", "Done")
 create.calendar("Actual")
 all <- projects %>%
   group_by(project, title, url, Status, Due) %>%
   summarise(
     # Make lists at deepest point
     staff = (li_flatten(login)),
     .groups = "drop_last"
   ) %>%
   ungroup() %>%
   mutate(Status = factor(Status, statuses, ordered = T)) %>%
   mutate(task = cell_spec(title, "html", link = url)) %>%
   select(project, task, Status, staff, Due) %>%
   arrange((Due)) %>%
   filter(Due >= today()) %>%
   mutate(bizdays_to_go = bizdays(from = today(), to = Due, "Actual")) %>%
   mutate(Due = format(Due, "%a %d-%b-%y"))
 remove_calendars("Actual")
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
