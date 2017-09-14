
#' @title Download PDF documents belonging to lawsuits
#' @description This function accesses lawsuits' ESAJ pages and
#' tries to download documents (if IDs are provided to `data`
#' and a lawsuit isn't found, shows error but skips it).
#' @param data A character vector with one or more lawsuit IDs or
#' the tibble returned by [get_metadata()]
#' @param path Path to directory where to save files
#' @param login ESAJ system login (if left `NULL` and your're
#' not logged in yet, will ask for it)
#' @param password ESAJ system password (if left `NULL` and your're
#' not logged in yet, will ask for it)
#' @param only_petitions Whether to only download petitions
#' @seealso [get_metadata()]
#' @export
download_documents <- function(data, path, login = NULL, password = NULL,
                               only_petitions = FALSE) {

  # Depending on data's type, get ready for download
  if (dplyr::is.tbl(data)) {
    login_esaj(login, password)
    r_cpopg <- httr::GET(
      "https://esaj.tjsp.jus.br/cpopg/open.do?gateway=true",
      trt:::vfpr_f)
  } else {
    data <- get_metadata(data, login, password, only_petitions)
  }

  # Create directories if necessary
  data <- dplyr::mutate(data, file = str_c(
    normalizePath(path), "/",
    replace_all(id, "[\\.\\-]", "")))
  purrr::walk(data$file, dir.create, FALSE, TRUE)

  # Download documents
  for (i in seq_along(data$title)) {
    data$file[i] <- str_c(
      data$file[i], "/", replace_all(data$number[i], "-", "_"),
      "_", data$title[i], ".pdf")
    httr::GET(
      data$link[i], trt:::vfpr_f,
      httr::write_disk(data$file[i], TRUE))
  }

  invisible(data)
}

#' @title Get metadata from documents belonging to lawsuits
#' @description This function accesses lawsuits' ESAJ pages and
#' tries to get the metadata of all documents (if lawsuit
#' isn't found, shows error but skips it).
#' @param id A character vector with one or more lawsuit IDs
#' @param login ESAJ system login (if left `NULL` and your're
#' not logged in yet, will ask for it)
#' @param password ESAJ system password (if left `NULL` and your're
#' not logged in yet, will ask for it)
#' @param only_petitions Whether to only get petitions
#' @seealso [download_documents()]
#' @export
get_metadata <- function(id, login = NULL, password = NULL,
                         only_petitions = FALSE) {

  # Get metadata for one ID
  get_metadata_ <- function(id) {

    # Initial access
    base <- "https://esaj.tjsp.jus.br/cpopg/"
    r_cpopg <- httr::GET(str_c(base, "open.do?gateway=true"), trt:::vfpr_f)

    # Parameters for GET query
    query_get <- list(
      conversationId = "",
      dadosConsulta.localPesquisa.cdLocal = "-1",
      cbPesquisa = "NUMPROC",
      dadosConsulta.tipoNuProcesso = "UNIFICADO",
      numeroDigitoAnoUnificado = stringr::str_sub(id, 1, 15),
      foroNumeroUnificado = stringr::str_sub(id, 22),
      dadosConsulta.valorConsultaNuUnificado = id,
      dadosConsulta.valorConsulta = "")

    # Get lawsuit code
    lwst_code <- str_c(base, "search.do") %>%
      httr::GET(query = query_get, trt:::vfpr_f) %>%
      purrr::pluck("all_headers", 1, "headers", "location") %>%
      stringr::str_match("processo\\.codigo=([^&]+)&") %>%
      magrittr::extract(1, 2)

    # Get page with all PDFs
    f_folder <- base %>%
      str_c("abrirPastaDigital.do?processo.codigo=", lwst_code) %>%
      httr::GET(trt:::vfpr_f) %>%
      purrr::pluck("all_headers", 1, "headers", "location") %>%
      httr::GET(trt:::vfpr_f)

    # Convert relevant content into JSON
    json <- f_folder %>%
      httr::content("text") %>%
      sub_between("requestScope", "requestScopeArvore") %>%
      stringr::str_sub(5, -9) %>%
      jsonlite::fromJSON()

    # Create data frame with all documents found
    docs <- json$data %>%
      tibble::as_tibble() %>%
      tibble::rownames_to_column() %>%
      dplyr::mutate(rowname = as.integer(rowname)) %>%
      dplyr::group_by(title, rowname) %>%
      dplyr::do(link = {
        json$children[[.$rowname]]$data$parametros }) %>%
      tidyr::unnest(link) %>%
      dplyr::arrange(rowname) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        link = str_c(
          "https://esaj.tjsp.jus.br/pastadigital/getPDF.action?", link),
        number = link %>%
          stringr::str_match('numInicial=([0-9]+)') %>%
          magrittr::extract(1, 2) %>%
          as.integer(),
        title = title %>%
          rm_accent() %>%
          stringr::str_to_lower() %>%
          stringr::str_trim() %>%
          replace_all('[ +/]', '_') %>%
          replace_all('_+', '_')) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        id = id,
        number = number %>%
          sprintf('%03d-%03d', ., dplyr::lead(.)-1) %>%
          gsub('0NA', 'inf', .)) %>%
      dplyr::select(title, number, id, link) %>%
      dplyr::arrange(number)

    # Filter columns if necessary
    docs <-
      if (only_petitions) {
        dplyr::filter(docs, detect(title, 'peticao|ajuizamento|contestacao'))
      } else { docs }

    return(docs)
  }
  get_metadata_ <- purrr::safely(get_metadata_, dplyr::tibble(), FALSE)

  # Login to ESAJ system
  login_esaj(login, password)

  # Map download over all IDs
  purrr::map_dfr(id, ~get_metadata_(.x)$result)
}
