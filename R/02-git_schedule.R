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
#' @importFrom parallel makeCluster detectCores stopCluster parLapply
#'
#'
#' @return Projects
#' @export
#'

git_schedule = function(token, org){
  projects = git_projects(token, org)  %>%
    filter(!template, !closed)
  all = NULL
  projects = split(projects, projects$id)
  cores <- max(c(1, detectCores()-1))
  cl <- makeCluster(cores)
  all = parLapply(cl, projects, git_issues)
  stopCluster(cl)
  all = all %>% bind_rows()
  all = all %>% mutate(Due = as.Date(Due))
  return(all)
}
