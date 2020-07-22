# Functions to manipulate the odds display

#' Get the `IdSport` for each listed game
#'
#' @param odds A xml_object
#'
#' @return A character vector
#' @noRd
#'
#' @importFrom xml2 xml_find_all xml_attr
#'
bm_get_sport <- function(odds) {
  odds %>%
    xml2::xml_find_all("/Data/Leagues/league") %>%
    xml2::xml_attr("IdSport") %>%
    unique() %>%
    sort()
}

#' Helper function to join spread or total and us_odds
#'
#' @param num Point spread or total
#' @param us_odds The odds offered on either spread or total
#' @param sep Value to place between `num` and `us_odds`. Use for total to place O or U before odds.
#'
#' @return A character vector
#' @noRd
#'
#' @importFrom tidyr replace_na
#'
bm_join_odds <- function(num, us_odds, sep = NULL) {
  num <- replace_na(num, "")
  us_odds <- replace_na(us_odds, "")
  if (is.null(sep)) {
    paste(num, us_odds)
  } else {
    paste(num, paste0(ifelse(num == "", "", sep), us_odds))
  }
}

#' Helper function to determine how many money lines (if any) a league is offering
#'
#' @param df A dataframe
#'
#' @return A dataframe with either no money lines, only `h_ml` and `v_ml`, or home and visitor plus draw.
#' @noRd
#'
#' @importFrom dplyr select all_of contains
#'
bm_drop_ml_s <- function(df) {
  if ((sum(is.na(df[["h_ml"]])) / nrow(df) == 1)) {
    return(df %>%
             dplyr::select(-dplyr::all_of(c('h_ml','p_ml','v_ml'))))
    
  }
  if (head(df$id_sprt, 1) != "SOC") {
    return(df %>% dplyr::select(-dplyr::contains('p_ml')))
  }
  df
}

#' Choose between soccer layout, 2 way money line, or no money line
#'
#' @param df dataframe
#'
#' @return dataframe
#' @noRd
#'
#' @importFrom wrapr qchar_frame %.>%
#' @importFrom cdata rowrecs_to_blocks_spec
#' @importFrom dplyr select contains
bm_pick_layout <- function(df) {
  n_cols <- sum(colnames(df) %in% c("v_ml", "p_ml", "h_ml"))
  if (n_cols < 3) {
    if (n_cols == 0) {
      c_table <- wrapr::qchar_frame(
        "location", "team", "point_spread", "total" |
          "v", v, v_ps, o |
          "h", h, h_ps, u
      )
    } else {
      c_table <- wrapr::qchar_frame(
        "location", "team", "ml", "point_spread", "total" |
          "v", v, v_ml, v_ps, o |
          "h", h, h_ml, h_ps, u
      )
    }
    layout <-
      cdata::rowrecs_to_blocks_spec(c_table,
                                    recordKeys = c("dt", "period")
      )
    tbl <- df %.>%
      layout
    return(tbl %>% dplyr::select(-dplyr::contains('location')))
  }
  df %>%
    dplyr::select(dplyr::all_of(
      c('dt',
        'period',
        'h',
        'v',
        'h_ml',
        'p_ml',
        'v_ml',
        'h_ps',
        'v_ps',
        'o',
        'u')
    )
    )
}

#' Join the moneyline odds to either the spread or total
#'
#' @param df dataframe
#'
#' @return
#' @noRd
#'
#' @importFrom dplyr transmute
#'
bm_odds_for_tbl <- function(df) {
  df %>%
    dplyr::transmute(
      dt,
      period,
      id_sprt,
      h,
      v,
      h_ml,
      p_ml,
      v_ml,
      h_ps = bm_join_odds(h_sprd, h_odd),
      v_ps = bm_join_odds(v_sprd, v_odd),
      o = bm_join_odds(ou, o, "O"),
      u = bm_join_odds(ou, u, "U")
    )
}

#' Parse xml odds and convert to dataframe
#'
#' @param odds xml object
#' @param odds_type futures or matchups
#' @param league league or sport
#'
#' @return dataframe
#' @noRd
#'
bm_xml_parse_pipeline <- function(odds, odds_type, league) {
  odds %>%
    bm_filter_by_btype(odds_type) %>%
    bm_filter_on_desc(league) %>%
    bm_convert_to_df()
}

#' Extract columns for futures
#'
#' @param df dataframe
#'
#' @return dataframe
#' @noRd
#'
#' @importFrom dplyr select all_of
#' @importFrom purrr set_names
bm_futures_tbl_pipeline <- function(df) {
  df %>%
    dplyr::select(dplyr::all_of(c("dt", "prop", "selection", "odds"))) %>%
    purrr::set_names(c("dt","contest","selection","odds"))
}

#' Pipeline to extract and reshape odds
#'
#' @param df dataframe
#'
#' @return dataframe
#' @noRd
#'
bm_matchup_tbl_pipeline <- function(df) {
  bm_odds_for_tbl(df) %>%
    bm_drop_ml_s() %>%
    bm_pick_layout()
}

#' Select correct odds pipeline, futures or matchups
#'
#' @param df dataframe
#'
#' @return dataframe
#' @noRd
#'
bm_tbl_select_format <- function(df) {
  is_matchup <- sum(colnames(df) %in% c("h_ml")) %>%
    as.character()
  tbl_fn <- switch(is_matchup,
                   "1" = bm_matchup_tbl_pipeline,
                   "0" = bm_futures_tbl_pipeline
  )
  tbl_fn(df)
}

#' One line pipeline xml to clean dataframe
#'
#' @param odds xml object
#' @param odds_type futures or matchups
#' @param league sport or league
#'
#' @return dataframe
#' @noRd
#'
bm_tbl_ouput_pipeline <- function(odds, odds_type, league) {
  bm_xml_parse_pipeline(odds, odds_type, league) %>%
    bm_tbl_select_format()
}

#' Reactable defaults
#'
#' @param df dataframe
#' @param grp_by columns to group reactable by
#'
#' @return reactable table
#' @export
#'
#' @importFrom reactable reactable colDef

bm_mk_reactable_tbl <- function(df, grp_by){
  reactable::reactable(
    data = df,
    defaultColDef = reactable::colDef(
      align = "center",
      minWidth = 50
    ),
    defaultPageSize = 20,
    groupBy = grp_by,
    resizable = TRUE,
    filterable = TRUE,
    defaultExpanded = TRUE
  )
}
