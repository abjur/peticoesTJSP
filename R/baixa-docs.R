# @export
visualize <- function (r) {
  ct <- r[["headers"]][["content-type"]]
  if (!grepl("html", ct)) {
    stop("Not a HTML file.")
  }
  if (interactive()) {
    dir <- tempfile()
    dir.create(dir)
    html_file <- file.path(dir, "index.html")
    h <- httr::content(r, "text")
    enc <- stringi::stri_enc_detect(h)[[1]]$Encoding[1]
    # h <- iconv(h, from = "UTF-8", to = "ISO-8859-1")
    cat(h, file = html_file)
    rstudioapi::viewer(html_file)
  }
}

build_url_cpo_pg <- function(p, tj, captcha = NULL, tipo_processo = 'UNIFICADO', uid = NULL) {
  #  p <- gsub("[^0-9]", "", as.character(p))

  dados_url <- list(conversationId = "",
                    dadosConsulta.localPesquisa.cdLocal = "-1",
                    cbPesquisa = "NUMPROC",
                    dadosConsulta.tipoNuProcesso = tipo_processo,
                    numeroDigitoAnoUnificado = "",
                    foroNumeroUnificado = "",
                    dadosConsulta.valorConsultaNuUnificado = "",
                    dadosConsulta.valorConsulta = "")

  if(tipo_processo == 'UNIFICADO'){

    dados_url[["numeroDigitoAnoUnificado"]] <- stringr::str_sub(p, start = 1, end = 15)
    dados_url[["foroNumeroUnificado"]] <- stringr::str_sub(p, start = 22)
    dados_url[["dadosConsulta.valorConsultaNuUnificado"]] <- p

  } else {

    dados_url[["dadosConsulta.valorConsulta"]] <- p

  }
  if (tj == 'TJSP') {
    url1 <- "https://esaj.tjsp.jus.br/cpopg/search.do"
  } else if (tj == 'TJAL') {
    url1 <- 'http://www2.tjal.jus.br/cpopg/search.do'
  } else if (tj == 'TJSC') {
    url1 <- "https://esaj.tjsc.jus.br/cpopg/search.do"
    if(!is.null(captcha)){
      dados_url[['vlCaptcha']] = tolower(captcha)
      dados_url[['uuidCaptcha']] = uid
      dados_url[['novoVlCaptcha']] = ''
    }
    # No TJSC o cpopg_um não consegue baixar via link quando tem captcha,
    # precisa fazer a requisição via formulário, com os parâmetros
    # de dados_url.
    return(dados_url)
  }
  parametros <- paste(names(dados_url), unlist(dados_url), sep = "=")

  paste(url1, paste0(parametros, collapse = "&"), sep = "?")
}

# @export
login_esaj <- function(cpf, pwd) {
  httr::GET('https://esaj.tjsp.jus.br/esaj/portal.do?servico=740000',
            httr::config(ssl_verifypeer = FALSE))
  u <- 'https://esaj.tjsp.jus.br/sajcas/login?service='
  u2 <- 'https://esaj.tjsp.jus.br/esaj/j_spring_cas_security_check'
  u <- paste0(u, utils::URLencode(u2, reserved = TRUE))
  # ssl <- httr::config(ssl_verifypeer = FALSE, maxredirs = 10L)
  r_inicial <- httr::GET(u, httr::config(ssl_verifypeer = FALSE))
  lt <- r_inicial %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_node(xpath = '//input[@name="lt"]') %>%
    rvest::html_attr('value')
  e2 <- r_inicial %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_node(xpath = '//input[@name="execution"]') %>%
    rvest::html_attr('value')
  dados <- list(
    'username' = cpf,
    'password' = pwd,
    'lt' = lt,
    'execution' = e2,
    '_eventId' = 'submit',
    'pbEntrar' = 'Entrar',
    'signature' = ''
  )
  # uu <- 'https://esaj.tjsp.jus.br/sajcas/iniciarHabilitacao.do'
  r_login <- httr::POST(u, body = dados,
                        httr::config(ssl_verifypeer = FALSE),
                        encode = 'form')
  # visualize(r_login)
  # logou <- r_login %>%
  #   httr::content('text') %>%
  #   stringr::str_detect(stringr::regex('marcelo', ignore_case = TRUE))
  logou <- httr::GET('https://esaj.tjsp.jus.br/sajcas/verificarLogin.js',
                     httr::config(ssl_verifypeer = FALSE)) %>%
    httr::content('text') %>%
    stringr::str_detect('true')

  if (logou) {
    cat('Login realizado com sucesso!\n')
  } else {
    cat(':(')
  }
  invisible()
}

# @export
pega_docs_processo <- function(p, path = '.', somente_peticoes = FALSE) {
  ssl <- httr::config(ssl_verifypeer = FALSE, maxredirs = 10L)
  uu <- 'https://esaj.tjsp.jus.br/cpopg/open.do?gateway=true'
  r_cpopg <- httr::GET(uu, config = ssl)
  u_processo <- build_url_cpo_pg(p, 'TJSP')
  r_processo <- httr::GET(u_processo, ssl)
  cdprocesso <- r_processo %>%
    with(all_headers) %>%
    dplyr::first() %>%
    with(headers) %>%
    with(location) %>%
    stringr::str_match('processo\\.codigo=([^&]+)&') %>%
    `[`(1, 2)
  cat("acessei a pagina do processo, cdprocesso:", cdprocesso, '\n')
  # visualize(r_processo)
  # u_link <- paste0('https://esaj.tjsp.jus.br/cpopg/',
  #                  'autorizarAcessoRecursoProcessoParaUsuario.do?')
  # args <- c(sprintf('processoPK.cdProcesso=%s', cdprocesso),
  #           'processoPK.cdForo=8',
  #           'processoPK.tpOrigemProcesso=2',
  #           'processoPK.flOrigem=P',
  #           'processoPK.aliasDaBase=PG5REG',
  #           sprintf('numeroProcesso=%s', p),
  #           'cdVara=101',
  #           'origemRecurso=P',
  #           'urlAcessoRecurso=%23')
  u_link <- sprintf('https://esaj.tjsp.jus.br/cpopg/%s=%s',
                    'abrirPastaDigital.do?processo.codigo',
                    cdprocesso)
  # u_link <- paste0(u_link, paste(args, collapse = '&'))
  r_docs <- httr::GET(u_link, httr::config(ssl_verifypeer = FALSE))
  u_certo <- r_docs %>%
    with(all_headers) %>%
    dplyr::first() %>%
    with(headers) %>%
    with(location)
  r_certo <- httr::GET(u_certo, httr::config(ssl_verifypeer = FALSE))
  # link_pdfs <- httr::content(r_certo, 'text')
  cat('peguei o link dos pdfs\n')
  path_p <- sprintf('%s/%s_%s', path, p, cdprocesso)
  dir.create(path_p, showWarnings = FALSE)
  # r_pdfs <- httr::GET(link_pdfs, config = ssl)
  js <- httr::content(r_certo, 'text') %>%
    stringr::str_sub(
      start = stringr::str_locate(., 'var requestScope')[, 2] + 4,
      end = stringr::str_locate(., 'var requestScopeArvoreSigi')[, 1] - 4
    ) %>%
    jsonlite::fromJSON()
  d_docs <- js$data %>%
    tibble::as_tibble() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(rowname = as.integer(rowname)) %>%
    dplyr::group_by(title, rowname) %>%
    dplyr::do(link = {
      js$children[[.$rowname]]$data$parametros
    }) %>%
    tidyr::unnest(link) %>%
    dplyr::arrange(rowname) %>%
    dplyr::mutate(
      numero = stringr::str_match(link, 'numInicial=([0-9]+)')[, 2],
      numero = as.integer(numero),
      numero = sprintf('%03d-%03d', numero, lead(numero) - 1),
      numero = gsub(' NA', 'inf', numero)
    ) %>%
    dplyr::select(title, numero, link) %>%
    dplyr::mutate(
      link = paste0('https://esaj.tjsp.jus.br/pastadigital/getPDF.action?', link),
      title = gsub(' +', '_', stringr::str_trim(tolower(abjutils::rm_accent(title)))),
      title = gsub('/', '_', title)
    ) %>%
    dplyr::arrange(numero)
  if (somente_peticoes) {
    baixar <- d_docs %>%
      dplyr::filter(stringr::str_detect(title, 'peticao|ajuizamento|contestacao'))
  } else {
    baixar <- d_docs
  }
  for(ii in seq_len(nrow(baixar))) {
    a <- sprintf('%s/%s_%s.pdf', path_p, baixar$numero[ii], baixar$title[ii])
    if(!file.exists(a)) {
      cat(a, '\n')
      httr::GET(baixar$link[ii], config = ssl, httr::write_disk(a))
    }
  }
  d_docs
}

# @export
pega_docs_processos <- function(p, login, senha, path, somente_peticoes = FALSE) {
  login_esaj(login, senha)
  f <- dplyr::failwith(dplyr::data_frame(title = NA), pega_docs_processo)
  d <- dplyr::data_frame(n_processo = p) %>%
    dplyr::group_by(n_processo) %>%
    dplyr::do(f(.$n_processo, path = path, somente_peticoes = somente_peticoes)) %>%
    dplyr::ungroup()
  d
}
