#' Get all projects from Github Organisation
#'
#' Obtains list from Github Organisation
#'
#' @param token token with access to repos and projects
#' @param org Organisation name
#'
#' @importFrom ghql Query GraphqlClient
#' @importFrom dplyr filter relocate select mutate rename as_tibble n
#' @importFrom tidyr spread unnest
#' @importFrom jsonlite fromJSON
#'
#'
#' @return Projects
#' @export
#'

git_issues = function(projects){
  qry <- Query$new()
  ## define query
  qry$query('query', paste0(
    'query{ node(id: "', projects$id[1], '") {
     ... on ProjectV2 {
      items(first: 100) {
        nodes {
          id
          fieldValues(first: 80) {
            nodes {
              ... on ProjectV2ItemFieldDateValue {
                date
                field {
                  ... on ProjectV2FieldCommon {
                    name
                  }
                }
              }
              ... on ProjectV2ItemFieldSingleSelectValue {
                name
                field {
                  ... on ProjectV2FieldCommon {
                    name
                  }
                }
              }
            }
          }
          content {
            ... on DraftIssue {
              title
              assignees(first: 100) {
                nodes {
                  login
                }
              }
            }
            ... on Issue {
              title
              url
              assignees(first: 100) {
                nodes {
                  login
                }
              }
            }
          }
        }
      }
    }
  }
}'))
  con <- GraphqlClient$new(
    url = "https://api.github.com/graphql",
    headers = list(Authorization = paste0("Bearer ", projects$token[1]))
  )
  res = fromJSON(con$exec(qry$queries$query))
  res = try(res$data$node$items$nodes %>% as_tibble() %>%
              unnest(fieldValues, keep_empty = T) %>%
              unnest(nodes, keep_empty = T) %>%
              rename(type = name) %>%
              unnest(field, keep_empty = T) %>%
              unnest(content, keep_empty = T) %>%
              unnest(assignees, keep_empty = T) %>%
              unnest(nodes, keep_empty = T) %>%
              mutate(value = ifelse(is.na(type), date, type)) %>%
              select(id, title, login, url, value, name)  %>%
              filter(name %in% c('Status', "Due")) %>%
              mutate(name = factor(name, na.omit(unique(name)), ordered = T)) %>%
              spread(name, value) %>%
              mutate(id = 1:n(), project = projects$title[1]) %>%
              relocate(project))
  if(class(res)[[1]] == "try-error"){
    message(paste("Check that all tasks have a status and at least one has a due date in ",projects$title[1]))
    res = NULL
  }
  return(res)
}
