help_button <- function (id) {
  ns = NS(id)
  actionBttn(ns("help"), "?", size = "sm")
}

help_click <- function (id, helpfile) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$help, {
      showModal(modalDialog(
        h4(includeHTML(helpfile)),
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

assign_index_peaks <- function(
    fragments_list,
    grouped = FALSE,
    index_override_dataframe = NULL) {
  if (grouped == TRUE) {
    # what we're doing here is pulling out the key data for all the samples that are metrics controls
    # each sample will then have the data for their appropriate control inserted inside
    # that can then be used in the calculation of instability metrics

    # we need to insert the whole peak table of the control because the calculation of the weighted mean
    # looks at a specific subset of the table, which is not set until the calculate metrics function

    # make a list of dataframes and alleles for each of the controls for the groups
    metrics_group_ids <- sapply(fragments_list, function(x) x$metrics_group_id)
    unique_metrics_group_ids <- unique(metrics_group_ids)

    baseline_control_list <- vector("list", length(unique_metrics_group_ids))
    names(baseline_control_list) <- unique_metrics_group_ids

    for (i in seq_along(fragments_list)) {
      if (!is.na(fragments_list[[i]]$metrics_group_id) && fragments_list[[i]]$metrics_baseline_control == TRUE) {
        # since there can be more than one control, make a list of them
        baseline_control_list[[fragments_list[[i]]$metrics_group_id]] <- c(
          baseline_control_list[[fragments_list[[i]]$metrics_group_id]],
          list(
            list(
              fragments_list[[i]]$get_allele_peak()$allele_repeat,
              fragments_list[[i]]$repeat_table_df,
              fragments_list[[i]]$batch_run_id
            )
          )
        )
      }
    }

    # do some quality control

    for (i in seq_along(baseline_control_list)) {
      controls_missing_allele <- all(sapply(baseline_control_list[[fragments_list[[i]]$metrics_group_id]], function(x) is.na(x[[1]])))

      if (length(baseline_control_list[[i]]) == 0) {
        warning(paste0("Group '", names(baseline_control_list)[[i]], "' has no 'metrics_baseline_control'. Instability metrics won't be calculated for this group in subsequent calculations."),
                call. = FALSE
        )
      }  else if (controls_missing_allele == TRUE) {
        warning(paste0("Group '", names(baseline_control_list)[[i]], "' control has no allele called. Instability metrics won't be calculated for this group in subsequent calculations."),
                call. = FALSE
        )
      }
    }

    # loop over each sample and put data inside
    for (i in seq_along(fragments_list)) {
      # if the group has no metrics_baseline_control it will be NULL so length == 0
      if(length(baseline_control_list[[fragments_list[[i]]$metrics_group_id]]) > 0){
        control_index_median_repeat <- median(sapply(baseline_control_list[[fragments_list[[i]]$metrics_group_id]], function(x) x[[1]]), na.rm = TRUE)
      } else{
        control_index_median_repeat <- NA_real_
      }
      # set index peak
      # samples with no data are skipped inside set_index_peaks and if NA value is provided index peak will be set to NA
      fragments_list[[i]]$set_index_peak(control_index_median_repeat)
      fragments_list[[i]]$.__enclos_env__$private$index_samples <- baseline_control_list[[fragments_list[[i]]$metrics_group_id]]
      fragments_list[[i]]$.__enclos_env__$private$assigned_index_peak_grouped <- TRUE

      # check if the index samples are from a different batch and the samples were not batch corrected
      index_sample_batch_ids <- unique(sapply(baseline_control_list[[fragments_list[[i]]$metrics_group_id]], function(x) x[[3]]))
      if(length(index_sample_batch_ids) > 0 && !fragments_list[[i]]$batch_run_id %in% index_sample_batch_ids){
        # so we've established that the index samples are from different run batch.
        # now check if they are they were batch corrected
        if(is.na(fragments_list[[i]]$.__enclos_env__$private$batch_correction_factor)){
          warning(
            call. = FALSE,
            paste0(fragments_list[[i]]$unique_id, " was grouped for index assignment, but its 'metrics_baseline_control' appears to be from a different 'batch_run_id'. ",
                   "Please run use 'batch_correction' in 'call_repeats()' to correct systematic differences between runs that may impact correct index peak assignment.")
          )
        }
      }
    }
  } else {
    # otherwise just use the modal peak as the index peak
    fragments_list <- lapply(fragments_list, function(x) {
      x$set_index_peak(x$get_allele_peak()$allele_repeat)
      x$.__enclos_env__$private$assigned_index_peak_grouped <- FALSE
      return(x)
    })
  }

  # override index peak with manually supplied values
  if (!is.null(index_override_dataframe)) {
    index_override_dataframe <- as.data.frame(index_override_dataframe)

    if (!any(index_override_dataframe[, 1] %in% names(fragments_list))) {
      missing_unique_ids <- which(!index_override_dataframe[, 1] %in% names(fragments_list))

      warning(
        call. = FALSE,
        paste0(
          "The following unique ids from the index override data frame are not in the repeats list:",
          paste0(index_override_dataframe[, 1], collapse = ", ")
        )
      )
    }

    lapply(fragments_list, function(x) {
      # if there is nothing to override, then just return the existing index values
      if (any(index_override_dataframe[, 1] == x$unique_id)) {
        x$set_index_peak(as.numeric(index_override_dataframe[which(index_override_dataframe[, 1] == x$unique_id), 2]))
      }
      return(x)
    })
  }
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
