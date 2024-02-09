#' Process spectral clusters
#'
#' @param lightData The light data
#' @param datetimeVar The name of the datetime variable. Defaults to "datetime".
#'
#' @return
#' @export
#'
#' @examples
spectrace_process_clusters = function(lightData,
                                      datetimeVar = datetime,
                                      clusterVar = cluster_id,
                                      regularise = TRUE){
  groups <- lightData %>% dplyr::group_vars()
  if(regularise){
    lightData <- lightData %>%
      spectrace_regularise_data({{datetimeVar}})
  }

  lightData <- lightData %>%
    dplyr::ungroup() %>%
    dplyr::nest_by(dplyr::pick(dplyr::all_of(groups))) %>%
    dplyr::mutate(
      data = list(
        mutate(data, find_cluster_timings({{cluster_id}}, {{datetimeVar}}))
      )
    ) %>%
    tidyr::unnest(cols = data) %>%
    dplyr::ungroup()
}
