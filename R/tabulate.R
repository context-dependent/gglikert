#' @export
reframe_for <- function(
  .data,
  ...,
  .by = NULL,
  .subgroups = list(all ~ TRUE)
) {
  .subgroups |>
    purrr::map_dfr(
      \(.formula) {
        label <- rlang::f_lhs(.formula)
        i <- eval_condition(.data, rlang::f_rhs(.formula))
        .data[i, ] |>
          dplyr::reframe(
            subgroup_label = rlang::as_string(label),
            subgroup_condition = rlang::expr_text(rlang::f_rhs(.formula)),
            ...,
            .by = !!.by
          )
      }
    ) |>
    dplyr::mutate(
      subgroup_label = forcats::fct_inorder(subgroup_label) |>
        forcats::fct_rev()
    )
}

#' @export
tabulate_likerts <- function(
  .data,
  items,
  item_labels = unlist(labelled::var_label(.data)),
  subgroups = list(all ~ TRUE),
  crush_responses = identity()
) {
  if (is.null(item_labels)) {
    item_labels <- setNames(items, items)
  }

  .data |>
    tidyr::pivot_longer(
      cols = {{ items }},
      names_to = "item",
      values_to = "response"
    ) |>
    dplyr::mutate(
      item_label = item_labels[item],
      response_category = crush_responses(response)
    ) |>
    dplyr::group_by(item, item_label, response_category) |>
    reframe_for(
      n = dplyr::n(),
      response_values = stringr::str_c(
        unique(sort(response)),
        collapse = "; "
      ),
      .subgroups = subgroups
    ) |>
    dplyr::group_by(subgroup_label, item) |>
    dplyr::mutate(p = n / sum(n))
}

#' @export
crush_likert <- function(
  x,
  affirm,
  negate,
  labels,
  ignore_case = TRUE,
  regexp = FALSE
) {
  if (ignore_case) {
    levels(x) <- tolower(levels(x))
    affirm <- tolower(affirm)
    negate <- tolower(negate)
  }

  if (regexp) {
    affirm <- levels(x)[grep(affirm, levels(x))]
    negate <- levels(x)[grep(negate, levels(x))]
  }

  x |>
    forcats::fct_collapse(
      !!labels[1] := affirm,
      !!labels[3] := negate,
      other_level = labels[2]
    ) |>
    factor(levels = labels)
}

#' @export
likert_crusher <- function(
  preset = c("default", "agree_5", "recommend_6"),
  ...
) {
  params <- scale_presets[[match.arg(preset)]]$crush |>
    modifyList(rlang::list2(...))

  \(.x) {
    params$x <- .x
    rlang::exec(crush_likert, !!!params)
  }
}

eval_condition <- function(.data, .condition, .env = rlang::caller_env()) {
  rlang::eval_tidy(
    .condition,
    data = .data,
    env = .env
  )
}
