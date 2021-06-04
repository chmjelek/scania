#' Asserts if dataset has required columns
#' @param data dataset to check
#' @param req_cols chr; vector containing required column names
assert_cols <- function(data,
                        req_cols) {
  assertthat::assert_that(all(req_cols %in% colnames(data)),
    msg = cat(paste0(
      "Column \"",
      req_cols[which(!req_cols %in% colnames(data))],
      "\" not found in data\n"
    ))
  )
}

#' Reads data and modifies columns types
#' @param train_path chr; path to training dataset
#' @param test_path chr; path to test dataset
#' @importFrom dplyr %>%
#' @return list of 2 lists; train and test
#' \itemize{
#'   \item train - training dataset
#'   \item test - testing dataset
#' }
read_data <- function(train_path, test_path) {
  cat("\nReading the data...")

  assertthat::assert_that(file.exists(train_path), msg = cat("\nTraining dataset does not exist in:", train_path, "\n"))
  assertthat::assert_that(file.exists(test_path), msg = cat("\nTest dataset does not exist in:", test_path, "\n"))

  train <- read.csv(train_path, na.strings = "na", skip = 20)
  test <- read.csv(test_path, na.strings = "na", skip = 20)

  data <- list(train, test)
  names(data) <- c("train", "test")
  purrr::map(data, ~ assert_cols(., "class"))

  data <- data %>%
    purrr::map_at(
      c("train", "test"),
      ~ .x %>% dplyr::mutate_at(
        dplyr::vars(class),
        ~ dplyr::recode_factor(., `neg` = 0, `pos` = 1)
      )
    ) %>%
    purrr::map(., ~ purrr::modify_if(., is.integer, as.numeric))

  cat(" Done!\n\n")
  return(data)
}

#' Removes columns where over x% of instances are: zeros or NA
#' takes columns with sd value that is more than x
#' @param data list of data.frames; original dataset
#' @param zeros_perc float; zeros ratio in attribute (above that value attribute will we removed)
#' @param na_perc float; NA ratio in attribute (above that value attribute will we removed)
#' @param sd_val float; sd value of attribute (below that value attribute will we removed)
#' @importFrom dplyr %>%
#' @return list of 3 tibbles; all, save and rm
#' \itemize{
#'   \item all - original set of attributes
#'   \item save - attributes to save
#'   \item rm - attributes to remove
#' }
feature_selection <- function(data, zeros_perc = 0.95, na_perc = 0.95, sd_val = 1) {
  data_train <- data[["train"]]
  data_train <- dplyr::select_if(data_train, is.numeric)

  attrs <- data_train %>% {
    tibble::tibble(
      col_name = colnames(.),
      zeros_ratio = purrr::map_dbl(., ~ sum(. == 0, na.rm = T) / length(.)),
      na_ratio = purrr::map_dbl(., ~ (sum(is.na(.)) / length(.))),
      sd = purrr::map_dbl(., ~ sd(., na.rm = T))
    )
  }

  attrs_save <- attrs %>%
    dplyr::filter(
      zeros_ratio < zeros_perc,
      na_ratio < na_perc,
      sd > sd_val
    )

  cat("Using:", nrow(attrs_save), "attributes\n")

  attrs_rm <- suppressMessages(dplyr::anti_join(attrs, attrs_save))

  cat("Removed:", nrow(attrs_rm), "attributes\n")

  attrs <- list(all = attrs, save = attrs_save, rm = attrs_rm)

  return(attrs)
}

#' Fills NA's with the median of the column
#' @param data list of 2 lists; original train and test datasetes
#' @param data_sub list of 3 tibbles; all, save and rm
#' @importFrom dplyr %>%
#' @return list of 2 lists; attrs and data
#' \itemize{
#'   \item attrs - attributes to save and remove
#'   \item data - data for the model
#' }
replace_w_median <- function(data, data_sub) {
    data_sub_na <- purrr::map(data, ~ .[c("class", data_sub$save$col_name)])

  data_sub_na <- purrr::map(data_sub_na, ~ {
    .x %>% dplyr::mutate_at(dplyr::vars(-class), ~ {
      ifelse(is.na(.), median(., na.rm = TRUE), .)
    })
  })

  return(list(attrs = data_sub, data = data_sub_na))
}

#' Solves undersampling problem
#' copies minor class and adds noise to it, major class is sampled to match length of minor class
#' multiple noise_levels can be set, noise_level = c(0, 0.1, 0.3) will result in original minor class and two copies with added noise (thus 6000 obs, 3000 pos and 3000 neg)
#' @param data_sub_na list of 2 data.frames; train and test
#' @param noise_level float or vector of floats; by default c(0, 0.1, 0.2)
#' @importFrom dplyr %>%
#' @return list of 3;
#' \itemize{
#'   \item noises; list of data.frames containing noised training records
#'   \item train; noised training set
#'   \item test; testing set
#' }
sampling <- function(data_sub_na, noise_level) {
  pos <- data_sub_na$data[["train"]] %>% dplyr::filter(., class == 1)

  std <- purrr::map(noise_level, ~ {
    pos %>%
      dplyr::select(-class) %>%
      apply(2, function(i) {
        sd(i) * .x
      })
  }) %>% setNames(noise_level)

  n <- nrow(pos)
  noise <- purrr::map(std, ~ purrr::map(., ~ rnorm(n, mean = 0, sd = .)))

  copy_pos_noise <- purrr::map(noise, ~ {
    pos %>%
      dplyr::select(-class) %>%
      +.x %>%
      dplyr::mutate(class = pos$class, .before = 1)
  })

  data_samp_pos <- data.table::rbindlist(copy_pos_noise)

  data_samp <- suppressMessages(data_sub_na$data[["train"]] %>% {
    tibble::tibble(
      pos = data_samp_pos,
      neg = dplyr::filter(., class == 0) %>% dplyr::sample_n(., nrow(pos))
    ) %>% purrr::reduce(dplyr::full_join)
  })

  return(list(preprocessed = data_sub_na, noises = copy_pos_noise, train = data_samp, test = data_sub_na$data[["test"]]))
}

#' Prepares data for the models
#' @param train_path chr; path to training dataset
#' @param test_path chr; path to testing dataset
#' @param noise_level float or vector of floats; by default c(0, 0.1, 0.2)
#' @param zeros_perc float; zeros ratio in attribute (above that value attribute will we removed); by default 0.95
#' @param na_perc float; NA ratio in attribute (above that value attribute will we removed); by default 0.95
#' @param sd_val float; sd value of attribute (below that value attribute will we removed); by default 1
#' @return list of 4; preprocessed, noises, train, test
#' \itemize{
#'   \item preprocessed - preprocessed data.frames
#'   \item noises - list of dataframes with noised records
#'   \item train - training data table
#'   \item test - testint data frame
#' }
#' @export
preprocessing <- function(train_path, test_path, noise_level = c(0, 0.1, 0.2), ...) {
  data <- read_data(
    train_path = train_path,
    test_path = test_path
  )

  data_sub <- feature_selection(
    data = data,
    ...
  )

  data_sub_na <- replace_w_median(
    data = data,
    data_sub = data_sub
  )

  data_sampled <- sampling(
    data_sub_na = data_sub_na,
    noise_level = noise_level
  )

  return(data_sampled)

}

