help_button <- function (id) {
  ns = NS(id)
  actionBttn(ns("help"), "?", size = "sm")
}

help_click <- function (id, helpfile) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$help, {
      showModal(modalDialog(
        includeHTML(helpfile),
        easyClose = TRUE,
        footer = NULL,
        size = "xl"
      ))
    })
  })
}

update_button <- function (id) {
  ns = NS(id)
  actionBttn(ns("updates"), "See updates log", size = "sm")
}

update_click <- function (id) {
moduleServer(id, function(input, output, session) {
observeEvent(input$updates, {
  showModal(modalDialog(
    title = strong("ICARUS CHANGE LOG"),
includeHTML("./data/help/Updates.html"),
    easyClose = TRUE,
    footer = NULL,
    size = "xl",
  ))
})
  })
}

ladder_rsq_warning_helper <- function(framents_trace,
                                     rsq_threshold) {
  rsq <- sapply(framents_trace$mod_parameters, function(x) suppressWarnings(summary(x$mod)$r.squared))
  if (any(rsq < rsq_threshold)) {
    size_ranges <- sapply(framents_trace$mod_parameters, function(x) x$mod$model$yi)
    size_ranges <- size_ranges[, which(rsq < rsq_threshold), drop = FALSE]
    size_ranges_vector <- vector("numeric", ncol(size_ranges))
    for (j in seq_along(size_ranges_vector)) {
      size_ranges_vector[j] <- paste0(size_ranges[1, j], "-", size_ranges[3, j])
    }
    warning(
      call. = FALSE,
      paste(
        framents_trace$unique_id)
      )
  }
}

local_southern_fit <- function(x, y) {
  # do some quality control. There should be no missing values and vectors should be same length
  if (length(x) != length(y)) {
    stop(
      call. = FALSE,
      "local_southern_fit error: ladder scan and size vectors different lengths"
    )
  } else if (any(is.na(x)) | any(is.na(y))) {
    stop(
      call. = FALSE,
      "local_southern_fit error: missing values in ladder scan or size"
    )
  }


  # Sort the data points by x values
  sorted_indices <- order(x)
  x_sorted <- x[sorted_indices]
  y_sorted <- y[sorted_indices]

  # Function to calculate the fitting constants for each group of three neighboring points
  mod_list <- vector("list", length = length(x_sorted) - 2)

  for (i in 1:(length(x_sorted) - 2)) {
    xi <- x_sorted[i:(i + 2)]
    yi <- y_sorted[i:(i + 2)]
    mod_list[[i]] <- list(
      mod = lm(yi ~ xi),
      first = xi[1],
      last = xi[3]
    )
  }

  return(mod_list)
}

local_southern_predict <- function(local_southern_fit, scans) {
  # total number of groups to brake the scans into:
  ladder_scan_pos <- sapply(local_southern_fit, function(fit) fit$first)

  # Find the nearest ladder position for each scan position
  nearest_ladder_index <- sapply(scans, function(scan) which.min(abs(scan - ladder_scan_pos)))

  # Assign the scan positions to corresponding groups based on nearest ladder position
  group_assignments <- rep(NA, length(scans))
  for (i in seq_along(nearest_ladder_index)) {
    group_assignments[i] <- nearest_ladder_index[i]
  }

  scan_split <- split(scans, group_assignments)

  size_split <- vector("list", length = length(scan_split))
  for (i in seq_along(scan_split)) {
    if (i == 1 | i == length(scan_split)) {
      size_split[[i]] <- stats::predict(local_southern_fit[[i]]$mod, data.frame(xi = scan_split[[i]]))
    } else {
      lower_prediction <- stats::predict(local_southern_fit[[i - 1]]$mod, data.frame(xi = scan_split[[i]]))
      upper_prediction <- stats::predict(local_southern_fit[[i]]$mod, data.frame(xi = scan_split[[i]]))
      size_split[[i]] <- (lower_prediction + upper_prediction) / 2
    }
  }

  size <- unlist(size_split)

  return(size)
}

ladder_fix_helper <- function(fragments_trace,
                              replacement_ladder_df) {
  fragments_trace_copy <- fragments_trace$clone()

  fragments_trace_copy$ladder_df <- replacement_ladder_df
  ladder_df <- fragments_trace_copy$ladder_df[which(!is.na(fragments_trace_copy$ladder_df$size)), ]
  ladder_df <- ladder_df[which(!is.na(ladder_df$scan)), ]
  fragments_trace_copy$mod_parameters <- local_southern_fit(ladder_df$scan, ladder_df$size)

  predicted_size <- local_southern_predict(local_southern_fit = fragments_trace_copy$mod_parameters, scans = fragments_trace_copy$scan)

  fragments_trace_copy$trace_bp_df <- data.frame(
    unique_id = rep(fragments_trace_copy$unique_id, length(fragments_trace_copy$scan)),
    scan = fragments_trace_copy$scan,
    size = predicted_size,
    signal = fragments_trace_copy$raw_data,
    ladder_signal = fragments_trace_copy$raw_ladder,
    off_scale = fragments_trace_copy$scan %in% fragments_trace_copy$off_scale_scans
  )

  # make a warning if one of the ladder modes is bad
  ladder_rsq_warning_helper(fragments_trace_copy,
                            rsq_threshold = 0.998
  )

  return(fragments_trace_copy)
}

metrics_override_helper <- function(fragments_list,
                                    index_override_dataframe) {


  index_override_dataframe <- as.data.frame(index_override_dataframe)

  if(!any(index_override_dataframe[, 1] %in% names(fragments_list))){
    missing_unique_ids <- which(!index_override_dataframe[, 1] %in% names(fragments_list))

    warning(call. = FALSE,
            paste0("The following unique ids from the index override data frame are not in the repeats list:",
                   paste0(index_override_dataframe[, 1], collapse = ", ")
            )
    )
  }

  lapply(fragments_list, function(x) {
    # if there is nothing to override, then just return the existing index values
    if (any(index_override_dataframe[, 1] == x$unique_id)) {
      index_delta <- as.numeric(x$repeat_table_df$repeats) - as.numeric(index_override_dataframe[which(index_override_dataframe[, 1] == x$unique_id), 2])

      closest_peak <- which(abs(index_delta) == min(abs(index_delta)))
      if (length(closest_peak) == 1) {
        x$index_repeat <- x$repeat_table_df$repeats[closest_peak]
        x$index_height <- x$repeat_table_df$height[closest_peak]
      } else {
        tallest_candidate <- closest_peak[which(x$repeat_table_df$height[closest_peak] == max(x$repeat_table_df$height[closest_peak]))]
        x$index_repeat <- x$repeat_table_df$repeats[tallest_candidate]
        x$index_height <- x$repeat_table_df$height[tallest_candidate]
      }
    }
    return(x)
  })
}

hline <- function(y = 0, color = "black") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color)
  )
}


