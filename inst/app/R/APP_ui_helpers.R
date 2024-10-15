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
        size = "l"
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
        title = strong("traceShiny CHANGE LOG"),
        includeHTML("./data/help/Updates.html"),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
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

extract_fragment_summary <- function(fragments_list) {
  extracted <- lapply(fragments_list, function(x) {
    data.frame(
      unique_id = x$unique_id,
      number_of_peaks = nrow(x$repeat_table_df),
      modal_repeat = x$get_allele_peak()$allele_repeat,
      modal_height = x$get_allele_peak()$allele_height
    )
  })
  extracted_df <- do.call(rbind, extracted)

  return(extracted_df)
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

extract_fragments <- function(fragments_list) {
  suppressWarnings(
    extracted <- lapply(fragments_list, function(x) {
      if (is.null(x$peak_table_df) & is.null(x$repeat_table_df)) {
        return(NULL)
      } else if (!is.null(x$peak_table_df) & is.null(x$repeat_table_df)) {
        df_length <- nrow(x$peak_table_df)
        data.frame(
          unique_id = rep(x$unique_id, df_length),
          main_peak_size = rep(x$get_allele_peak()$allele_size, df_length),
          main_peak_height = rep(x$get_allele_peak()$allele_height, df_length),
          height = x$peak_table_df$height,
          index_repeat = rep(x$get_index_peak()$index_repeat, df_length),
          size = x$peak_table_df$size
          #peak_region = x$.__enclos_env__$private$peak_regions
        )
      } else if (!is.null(x$repeat_table_df) && nrow(x$repeat_table_df > 0)) {
        df_length <- nrow(x$repeat_table_df)
        data.frame(
          unique_id = rep(x$unique_id, df_length),
          main_peak_repeat = rep(x$get_allele_peak()$allele_repeat, df_length),
          main_peak_height = rep(x$get_allele_peak()$allele_height, df_length),
          height = x$repeat_table_df$height,
          index_repeat = rep(x$get_index_peak()$index_repeat, df_length),
          repeats = x$repeat_table_df$repeats
          #peak_region = x$.__enclos_env__$private$peak_regions
        )
      }
    })
  )
  extracted_df <- do.call(rbind, extracted)


  return(extracted_df)
}

extract_trace_table <- function (fragments_trace_list) {
  plate_list <- lapply(fragments_trace_list, function(x) {
    df_length <- nrow(x$trace_bp_df)
    data.frame(
    x$trace_bp_df,
    index_repeat = rep(x$get_index_peak()$index_repeat, df_length)
    )
  })
  plate_combined_df <- do.call(rbind, plate_list)
  return(plate_combined_df)
}

plot_repeat_correction_model <- function(fragments_list, batch_run_id_subset = NULL) {
  # Check if all models in the list are the same
  first_model_df <- fragments_list[[1]]$.__enclos_env__$private$repeat_correction_mod
  identical_model_test <- logical(length(fragments_list))
  for (i in seq_along(fragments_list)) {
    identical_model_test[i] <- identical(first_model_df, fragments_list[[i]]$.__enclos_env__$private$repeat_correction_mod)
  }

  if (!all(identical_model_test)) {
    stop("The supplied fragments list must come from the same 'call_repeats' function output", call. = FALSE)
  }

  controls_repeats_df <- fragments_list[[1]]$.__enclos_env__$private$repeat_correction_mod$model
  controls_repeats_df$unique_id <- sub("\\.[0-9]+$", "", row.names(controls_repeats_df))
  # add back in batch_run_id if it's not there (because a different lm is made when just one run)
  # assume that all the samples are the same batch since they have identical model
  if(!"batch_run_id" %in% names(controls_repeats_df)){
    controls_repeats_df$batch_run_id <- rep(fragments_list[[1]]$batch_run_id, nrow(controls_repeats_df))
  }

  # Plotting
  unique_batch_run_ids <- unique(controls_repeats_df$batch_run_id)

  if(!is.null(batch_run_id_subset) && is.numeric(batch_run_id_subset)){
    if(batch_run_id_subset > length(unique_batch_run_ids)){
      stop(call. = FALSE, paste0("The 'batch_run_id_subset' number was too large. There are only ",length(unique_batch_run_ids), " 'batch_run_id'."))
    }
    unique_batch_run_ids <- unique_batch_run_ids[batch_run_id_subset]
  } else if(is.character(batch_run_id_subset)){
    unique_batch_run_ids <- unique_batch_run_ids[which(unique_batch_run_ids %in% batch_run_id_subset)]
  }

  for (i in 1:length(unique_batch_run_ids)) {
    plate_data <- controls_repeats_df[which(controls_repeats_df$batch_run_id == unique_batch_run_ids[i]),]
  }

  p <- ggplot(plate_data, aes(x=size, y=validated_repeats)) +
    geom_smooth(method = "lm") +
    geom_point(aes(x=size, y=validated_repeats, colour = unique_id,
                   text=paste0("Repeats: ", size)),
               shape = 21, fill = "white", size = 5, stroke = 1, alpha = 0.8) +
    stat_regline_equation(output.type = "text", label.x.npc = 0.5, label.y.npc = 0.8) +
    stat_cor(output.type = "text", label.x.npc = 0.5, label.y.npc = 0.9) +
    scale_shape(solid = FALSE) +
    xlab("Repeats") +
    ylab("User Supplied Repeat Length") +
    theme_bw()

  ggplotly(p, tooltip="text")

}

reactivity_trigger <- function(
    fragments_trace_list) {

  fragments_list <- lapply(fragments_trace_list, function(x) {

    # generate new class
    new_fragments_repeats <- trace:::fragments_repeats$new(unique_id = x$unique_id)
    new_fragments_repeats$trace_bp_df <- x$trace_bp_df
    new_fragments_repeats$peak_table_df <- x$peak_table_df
    new_fragments_repeats <- trace:::transfer_metadata_helper(x, new_fragments_repeats)
    new_fragments_repeats$.__enclos_env__$private$min_bp_size <- x$min_bp_size
    new_fragments_repeats$.__enclos_env__$private$max_bp_size <- x$max_bp_size

    return(new_fragments_repeats)
  })

  return(fragments_list)
}
