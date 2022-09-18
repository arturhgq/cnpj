#' @title Make cnpj strings consistent across different patterns
#' @description `r lifecycle::badge("stable")`
#'
#' This function makes cnpj strings consistent across different patterns
#' @param .cnpj cnpj string
#' @examples
#'   fake_cnpj <- c(
#'    "01223446000126",
#'    "0,3.5;666123,5--2-3.12"
#'  )
#'  fix_cnpj(fake_cnpj)
#' @export
fix_cnpj <- \(.cnpj) {
  dtools::extract_numbers(.cnpj) |>
    add_digits() |>
    stringr::str_replace(
      pattern = "(.{2})(.{3})(.{3})(.{4})(.{2})",
      replacement = "\\1.\\2.\\3/\\4-\\5")
}

#' @title Add leading zeros in cnpjs
#' @description `r lifecycle::badge("experimental")`
#'
#' This function adds leading zeros in cnpjs
#' @param .cnpj cnpjs
#' @examples
#' add_digits("213565398")
#' @export
add_digits <- \(.cnpj){
  dtools::extract_numbers(.cnpj) -> cnpj
  stringr::str_split(cnpj, "") -> cnpj_split
  purrr::map_dbl(cnpj_split, length) -> cnpj_length

  purrr::map2_dbl(
    .x = cnpj,
    .y = cnpj_length,
    ~
      if (!is.na(.x)) {
        return(.y)
      } else {
        return(NA_real_)
      }
  ) -> cnpj_length

  14 - cnpj_length -> cnpj_length_remainder
  stringr::str_dup("0", cnpj_length_remainder) -> n_zeros

  purrr::map2_chr(
    .x = n_zeros,
    .y = cnpj,
    ~
      if (!is.na(.x)) {
        paste0(.x, .y)
      } else {
        NA
      }
  )
}


