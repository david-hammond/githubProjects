#' Get all projects from Github Organisation
#'
#' Obtains list from Github Organisation
#'
#' @param token token with access to repos and projects
#' @param org Organisation name
#'
#' @importFrom ghql Query GraphqlClient
#' @importFrom dplyr filter
#' @importFrom jsonlite fromJSON
#'
#'
#' @return Projects
#' @export
#'



git_projects = function(token, org){
  qry <- Query$new()
  ## define query
  qry$query('query', paste0('{organization(login: "', org, '"){projectsV2(first: 100)
  { nodes { id title closed closedAt template}}}} '))

  con <- GraphqlClient$new(
    url = "https://api.github.com/graphql",
    headers = list(Authorization = paste0("Bearer ", token))
  )
  project = fromJSON(con$exec(qry$queries$query))
  project = project$data$organization$projectsV2$nodes
  project$token = token
  return(project)
}
