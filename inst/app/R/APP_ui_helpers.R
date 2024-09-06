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

ladder_rsq_warning_helper <- function(
    framents_trace,
    rsq_threshold) {
  rsq <- sapply(framents_trace$local_southern_mod, function(x) suppressWarnings(summary(x$mod)$r.squared))
  if (any(rsq < rsq_threshold)) {
    size_ranges <- sapply(framents_trace$local_southern_mod, function(x) x$mod$model$yi)
    size_ranges <- size_ranges[, which(rsq < rsq_threshold), drop = FALSE]
    size_ranges_vector <- vector("numeric", ncol(size_ranges))
    for (j in seq_along(size_ranges_vector)) {
      size_ranges_vector[j] <- paste0(size_ranges[1, j], "-", size_ranges[3, j])
    }
    warning(
      call. = FALSE,
      paste(
        framents_trace$unique_id
      )
    )
  }
}

# bp sizing ---------------------------------------------------------------


local_southern <- function(x, y) {
  # do some quality control. There should be no missing values and vectors should be same length
  if (length(x) != length(y)) {
    stop(
      call. = FALSE,
      "local_southern error: ladder scan and size vectors different lengths"
    )
  } else if (any(is.na(x)) | any(is.na(y))) {
    stop(
      call. = FALSE,
      "local_southern error: missing values in ladder scan or size"
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

local_southern_predict <- function(local_southern_output, scans) {
  # total number of groups to brake the scans into:
  ladder_scan_pos <- sapply(local_southern_output, function(fit) fit$first)

  # Find the nearest ladder position for each scan position
  nearest_ladder_index <- sapply(scans, function(scan) which.min(abs(scan - ladder_scan_pos)))

  # Assign the scan positions to corresponding groups based on nearest ladder position
  scan_split <- split(scans, nearest_ladder_index)
  size_split <- vector("list", length = length(scan_split))
  for (i in seq_along(scan_split)) {
    if (i == 1 | i == length(scan_split)) {
      size_split[[i]] <- stats::predict(local_southern_output[[i]]$mod, data.frame(xi = scan_split[[i]]))
    } else {
      lower_prediction <- stats::predict(local_southern_output[[i - 1]]$mod, data.frame(xi = scan_split[[i]]))
      upper_prediction <- stats::predict(local_southern_output[[i]]$mod, data.frame(xi = scan_split[[i]]))
      size_split[[i]] <- (lower_prediction + upper_prediction) / 2
    }
  }

  size <- unlist(size_split)

  return(size)
}

ladder_fix_helper <- function(fragments_trace,
                              replacement_ladder_df) {

  fragments_trace$ladder_df <- replacement_ladder_df
  ladder_df <- fragments_trace$ladder_df[which(!is.na(fragments_trace$ladder_df$size)), ]
  ladder_df <- ladder_df[which(!is.na(ladder_df$scan)), ]
  fragments_trace$local_southern_mod <- local_southern(ladder_df$scan, ladder_df$size)

  predicted_size <- local_southern_predict(local_southern_output = fragments_trace$local_southern_mod, scans = fragments_trace$scan)

  fragments_trace$trace_bp_df <- data.frame(
    unique_id = rep(fragments_trace$unique_id, length(fragments_trace$scan)),
    scan = fragments_trace$scan,
    size = predicted_size,
    signal = fragments_trace$raw_data,
    ladder_signal = fragments_trace$raw_ladder,
    off_scale = fragments_trace$scan %in% fragments_trace$off_scale_scans
  )

  # make a warning if one of the ladder modes is bad
  ladder_rsq_warning_helper(fragments_trace,
                            rsq_threshold = 0.998
  )

  return(fragments_trace)
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
        x$.__enclos_env__$private$index_repeat <- x$repeat_table_df$repeats[closest_peak]
        x$.__enclos_env__$private$index_height <- x$repeat_table_df$height[closest_peak]
      } else {
        tallest_candidate <- closest_peak[which(x$repeat_table_df$height[closest_peak] == max(x$repeat_table_df$height[closest_peak]))]
        x$.__enclos_env__$private$index_repeat <- x$repeat_table_df$repeats[tallest_candidate]
        x$.__enclos_env__$private$index_height <- x$repeat_table_df$height[tallest_candidate]
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

cbind.fill<-function(...){
  nm <- list(...)
  nm<-lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
}

plot_trace_helper <- function(fragments,
                              show_peaks,
                              x_axis,
                              ylim,
                              xlim,
                              height_color_threshold,
                              plot_title) {
  if (is.null(fragments$trace_bp_df)) {
    stop(
      call. = FALSE,
      paste(fragments$unique_id, "This sample does not have trace data. Use fsa files as inputs to pipeline to plot trace.")
    )
  }

  # there must be a simpler way of the following if else below
  if (is.null(x_axis) && is.null(fragments$repeat_table_df)) {
    data <- fragments$trace_bp_df
    data$x <- data$size
    x_axis_label <- "Size"
  } else if (is.null(x_axis) && !is.null(fragments$repeat_table_df)) {
    data <- fragments$trace_bp_df
    data$x <- data$calculated_repeats
    x_axis_label <- "Repeats"
  } else if (x_axis == "size") {
    data <- fragments$trace_bp_df
    data$x <- data$size
    x_axis_label <- "Size"
  } else {
    data <- fragments$trace_bp_df
    data$x <- data$calculated_repeats
    x_axis_label <- "Repeats"
  }

  if (!is.null(xlim)) {
    data <- data[which(data$x < xlim[2] & data$x > xlim[1]), ]
  }

  plot(data$x,
       data$signal,
       main = ifelse(is.null(plot_title), fragments$unique_id, plot_title),
       type = "l",
       xlab = x_axis_label,
       ylab = "Signal",
       ylim = ylim
  )


  if (any(data$off_scale)) {
    abline(v = data[which(data$off_scale), "x"], col = adjustcolor("red", alpha.f = 0.3), lwd = 2.5)
  }

  # add points onto plot showing peaks
  if (!is.null(fragments$peak_table_df) && show_peaks) {
    if (is.null(x_axis) && is.null(fragments$repeat_table_df)) {
      peak_table <- fragments$peak_table_df
      peak_table$x <- peak_table$size
    } else if (is.null(x_axis) && !is.null(fragments$repeat_table_df)) {
      peak_table <- fragments$repeat_table_df
      peak_table$x <- peak_table$repeats
    } else if (x_axis == "size") {
      peak_table <- fragments$peak_table_df
      peak_table$x <- peak_table$size
    } else {
      peak_table <- fragments$repeat_table_df
      peak_table$x <- peak_table$repeats
    }

    # exit early if the peak table is empty
    if (nrow(peak_table) == 0) {
      return()
    }

    if (!is.null(xlim)) {
      peak_table <- peak_table[which(peak_table$x < xlim[2] & peak_table$x > xlim[1]), ]
    }

    tallest_peak_height <- peak_table[which(peak_table$height == max(peak_table$height)), "height"]
    tallest_peak_x <- peak_table[which(peak_table$height == tallest_peak_height), "x"]
    if (!is.null(fragments$get_alleles()$allele_1_height) && !is.na(fragments$get_alleles()$allele_1_height)) {
      tallest_peak_height <- fragments$get_alleles()$allele_1_height
      # find the tallest peak x axis position
      if (is.null(x_axis) && is.na(fragments$get_alleles()$allele_1_repeat)) {
        tallest_peak_x <- fragments$get_alleles()$allele_1_size
      } else if (is.null(x_axis) && !is.na(fragments$get_alleles()$allele_1_repeat)) {
        tallest_peak_x <- fragments$get_alleles()$allele_1_repeat
      } else if (x_axis == "size") {
        tallest_peak_x <- fragments$get_alleles()$allele_1_size
      } else {
        tallest_peak_x <- fragments$get_alleles()$allele_1_repeat
      }
    }

    peaks_above <- peak_table[which(peak_table$height > tallest_peak_height * height_color_threshold), ]
    peaks_below <- peak_table[which(peak_table$height < tallest_peak_height * height_color_threshold), ]

    # Adding peaks
    points(peaks_above$x,
           peaks_above$height,
           col = "blue"
    )
    points(peaks_below$x,
           peaks_below$height,
           col = "purple"
    )
    points(tallest_peak_x,
           tallest_peak_height,
           col = "green"
    )

    # Draw horizontal dotted lines to connect repeats to their actual place on the plot
    if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
      for (i in 1:nrow(peak_table)) {
        segments(
          x0 = peak_table$repeats[i],
          y0 = peak_table$height[i],
          x1 = peak_table$calculated_repeats[i],
          y1 = peak_table$height[i],
          lty = 2
        )
      }
    }
  }


  if (!is.null(fragments$get_index_peak()$index_repeat) && !is.na(fragments$get_index_peak()$index_repeat)) {
    abline(v = fragments$get_index_peak()$index_repeat, col = "black", lwd = 2, lty = 3)
  }
}
