scale_presets <- list(
  default = list(
    crush = list(
      ignore_case = TRUE,
      regexp = FALSE
    )
  ),
  agree_5 = list(
    vals = forcats::fct_inorder(c(
      "Strongly agree",
      "Agree",
      "Neutral",
      "Disagree",
      "Strongly disagree"
    )),
    crush = list(
      affirm = c("Strongly agree", "Agree"),
      negate = c("Disagree", "Strongly disagree"),
      labels = c("Affirm", "Neutral", "Dissent"),
      ignore_case = TRUE
    )
  ),
  recommend_6 = list(
    vals = forcats::fct_inorder(c(
      "I've already recommended",
      "Definitely will",
      "Probably will",
      "Might or might not",
      "Probably won't",
      "Definitely won't"
    )),
    crush = list(
      affirm = "^(Definitely will|I've already recommended)",
      negate = "^(Probably won't|Definitely won't)",
      labels = c("Promoter", "Neutral", "Detractor"),
      ignore_case = TRUE
    )
  )
)

usethis::use_data(
  scale_presets,
  overwrite = TRUE,
  internal = TRUE
)
