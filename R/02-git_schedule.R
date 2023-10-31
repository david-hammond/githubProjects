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

git_schedule = function(token, org){
  projects = git_projects(token, org)  %>%
    filter(!template, !closed)
  all = NULL
  for (i in 1:nrow(projects)){
    print(i)
    res = git_issues(projects[i,])
    all = all %>% rbind(res)
  }
  all = all %>% mutate(Due = as.Date(Due))
  return(all)
}
