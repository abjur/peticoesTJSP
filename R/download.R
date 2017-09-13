
#' Download PDF documents belonging to lawsuits
#' @param id A character vector with one or more lawsuit IDs
#' @param path Path to directory where to save files
#' @param login ESAJ system login (if left `NULL`, will ask for it)
#' @param password ESAJ system password (if left `NULL`, will ask for it)
#' @param only_petitions Whether to only download petitions
#' @param verbose Whether to output messages while downloading
#' @export
download_documents <- function(id, path, login = NULL, password = NULL,
                               only_petitions = FALSE, verbose = FALSE) {

  # Download documents belonging to a lawsuit
  download_ <- function(id) {

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

    # Message
    if (verbose) { message("Fetched lawsuit code") }

    # Get page with all PDFs
    f_folder <- base %>%
      str_c("abrirPastaDigital.do?processo.codigo=", lwst_code) %>%
      httr::GET(trt:::vfpr_f) %>%
      purrr::pluck("all_headers", 1, "headers", "location") %>%
      httr::GET(trt:::vfpr_f)

    # Message
    if (verbose) { message("Fetched links to PDFs") }

    # Create directory if necessary
    path <- str_c(
      normalizePath(path), "/",
      replace_all(id, "[\\.\\-]", ""))
    dir.create(path, FALSE, TRUE)

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
          gsub(' NA', 'inf', .)) %>%
      dplyr::select(title, number, id, link) %>%
      dplyr::arrange(number)

    # Filter columns if necessary
    docs <-
    if (only_petitions) {
      dplyr::filter(docs, detect(title, 'peticao|ajuizamento|contestacao'))
    } else { docs }

    # Message
    if (verbose) { message("Downloading documents") }

    # Download documents
    for (i in seq_along(docs$title)) {
      file <- str_c(
        path, "/", replace_all(docs$number[i], "-", "_"),
        "_", docs$title[i], ".pdf")
      httr::GET(docs$link[i], trt:::vfpr_f, httr::write_disk(file, TRUE))
    }

    return(docs)
  }

  # Login to ESAJ system
  login_esaj(login, password)

  # Map download over all IDs
  purrr::map_dfr(id, download_)
}
