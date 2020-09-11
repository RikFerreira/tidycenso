#' Read microdata from Censo 2010
#'
#' Read microdata and parses using the awesome for operator
#'
#' @param microdata microdata file
#' @param topic which topic to read
#'
#' @return `data.frame()` with the results
#' @export
#'
read_microdata <- function(microdata, topic = c("domicilios", "pessoas", "migracao", "mortalidade")) {
  # Apesar do argumento 'topic', a função ainda não faz tratamento sobre qual arquivo será aberto
  match.arg(topic)

  # Download da documentação do Censo Demográfico
  doc_url <- "ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/Resultados_Gerais_da_Amostra/Microdados/Documentacao.zip"

  tempf <- tempfile()
  tempd <- tempdir()

  raw <- readxl::read_xls(list.files(tempd, pattern = "Layout_microdados_Amostra\\.xls$", recursive = TRUE, full.names = TRUE), sheet = 2)

  # Tratamento da planilha com as larguras das colunas
  layout <- raw %>%
    dplyr::select(
      VAR = 1,
      START = `...8`,
      END = `...9`,
      INT = `...10`,
      DEC = `...11`,
      TYPE = `...12`
    ) %>%
    dplyr::slice(-1) %>%
    dplyr::mutate(
      START = as.integer(START),
      END = as.integer(END),
      INT = as.integer(INT),
      DEC = tidyr::replace_na(DEC, 0) %>% as.integer(),
      TYPE = stringr::str_replace_all(TYPE, "\n", "") %>%
        stringr::str_replace("A|C", "c") %>%
        stringr::str_replace("N", "d")
    )

  # Leitura dos microdados. Todos foram lidos como sendo do tipo character
  microdata <- readr::read_fwf(
    file = microdata,
    col_positions = readr::fwf_positions(layout$START, layout$END, layout$VAR),
    col_types = paste0(rep("c", nrow(layout)), collapse = "")
  )

  # Tratamento dos valores reais
  for(i in seq_along(microdata)) {
    if(layout[which(layout$VAR == names(microdata[,i])),]$DEC > 0) {
      int <- layout[i, ]$INT
      dec <- layout[i, ]$DEC

      integer_part <- stringr::str_sub(dplyr::pull(microdata, i), 1, int)
      decimal_part <- stringr::str_sub(dplyr::pull(microdata, i), int+1, int+dec)

      float_number <- paste0(integer_part, ".", decimal_part) %>%
        tibble::as_tibble() %>%
        readr::type_convert(col_types = "d")

      microdata[, i] <- float_number
    }
  }

  # Conversão aos tipos definidos na documentação
  microdata <- readr::type_convert(
    microdata,
    col_types = paste0(layout$TYPE, collapse = "")
  )

  return(microdata)
}
