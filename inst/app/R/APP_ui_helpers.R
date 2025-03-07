help_button <- function (id) {
  ns = NS(id)
  actionBttn(ns("help"), "?", size = "sm")
}

help_click <- function (id, helpfile) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$help, {
      showModal(modalDialog(
        h5(includeHTML(helpfile)),
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

debounce <- function(expr, millis, env = parent.frame(), quoted = FALSE,
                     domain = getDefaultReactiveDomain()) {

  force(millis)

  f <- exprToFunction(expr, env, quoted)
  label <- sprintf("debounce(%s)", paste(deparse(body(f)), collapse = "\n"))

  v <- reactiveValues(
    trigger = NULL,
    when = NULL # the deadline for the timer to fire; NULL if not scheduled
  )

  # Responsible for tracking when f() changes.
  observeEvent(f(), {
    # The value changed. Start or reset the timer.
    v$when <- Sys.time() + millis/1000
  }, ignoreNULL = FALSE)

  # This observer is the timer. It rests until v$when elapses, then touches
  # v$trigger.
  observe({
    if (is.null(v$when))
      return()

    now <- Sys.time()
    if (now >= v$when) {
      v$trigger <- runif(1)
      v$when <- NULL
    } else {
      invalidateLater((v$when - now) * 1000, domain)
    }
  })

  # This is the actual reactive that is returned to the user. It returns the
  # value of f(), but only invalidates/updates when v$trigger is touched.
  eventReactive(v$trigger, {
    f()
  }, ignoreNULL = FALSE)
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
      modal_signal = x$get_allele_peak()$allele_signal
    )
  })
  extracted_df <- do.call(rbind, extracted)

  allele_table <- extract_alleles(fragments_list)
  allele_table <- allele_table[,-which(colnames(allele_table) %in% "allele_size")]
  if (any(colnames(allele_table) %in% "allele_2_size")) {
  allele_table <- allele_table[,-which(colnames(allele_table) %in% "allele_2_size")]
  }
  extracted_df_final <- right_join(extracted_df, allele_table)

  return(extracted_df_final)
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
                              signal_color_threshold,
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

    tallest_peak_signal <- peak_table[which(peak_table$signal == max(peak_table$signal)), "signal"]
    tallest_peak_x <- peak_table[which(peak_table$signal == tallest_peak_signal), "x"]
    if (!is.null(fragments$get_allele_peak()$allele_signal) && !is.na(fragments$get_allele_peak()$allele_signal)) {
      tallest_peak_signal <- fragments$get_allele_peak()$allele_signal
      # find the tallest peak x axis position
      if (is.null(x_axis) && is.na(fragments$get_allele_peak()$allele_repeat)) {
        tallest_peak_x <- fragments$get_allele_peak()$allele_size
      } else if (is.null(x_axis) && !is.na(fragments$get_allele_peak()$allele_repeat)) {
        tallest_peak_x <- fragments$get_allele_peak()$allele_repeat
      } else if (x_axis == "size") {
        tallest_peak_x <- fragments$get_allele_peak()$allele_size
      } else {
        tallest_peak_x <- fragments$get_allele_peak()$allele_repeat
      }
    }

    peaks_above <- peak_table[which(peak_table$signal > input$minimum_peak_signal), ]
    peaks_below <- peak_table[which(peak_table$signal < input$minimum_peak_signal), ]

    # Adding peaks
    points(peaks_above$x,
           peaks_above$signal,
           col = "blue"
    )
    points(peaks_below$x,
           peaks_below$signal,
           col = "purple"
    )
    points(tallest_peak_x,
           tallest_peak_signal,
           col = "green"
    )

    # Draw horizontal dotted lines to connect repeats to their actual place on the plot
    if (!is.null(peak_table$repeats) && !is.null(peak_table$calculated_repeats)) {
      for (i in 1:nrow(peak_table)) {
        segments(
          x0 = peak_table$repeats[i],
          y0 = peak_table$signal[i],
          x1 = peak_table$calculated_repeats[i],
          y1 = peak_table$signal[i],
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
          main_peak_signal = rep(x$get_allele_peak()$allele_signal, df_length),
          signal = x$peak_table_df$signal,
          index_repeat = rep(x$get_index_peak()$index_repeat, df_length),
          size = x$peak_table_df$size
          #peak_region = x$.__enclos_env__$private$peak_regions
        )
      } else if (!is.null(x$repeat_table_df) && nrow(x$repeat_table_df > 0)) {
        df_length <- nrow(x$repeat_table_df)
        data.frame(
          unique_id = rep(x$unique_id, df_length),
          main_peak_repeat = rep(x$get_allele_peak()$allele_repeat, df_length),
          main_peak_signal = rep(x$get_allele_peak()$allele_signal, df_length),
          signal = x$repeat_table_df$signal,
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

####Fastq helpers
repeat_sizer <- function(
    fastq_df,
    left_flank_seq = "CAAGTCCTTC",
    right_flank_seq = "CAACAGCCGCCACCG",
    repeat_unit_seq = "CAG",
    max.distance = 0.01,
    min_n_repeats = 10,
    interruptions_repeat_no = 3,
    codon_start = 2
){
  repeat_matcher <- function(seq){
    repeat_match <- aregexec(paste0(left_flank_seq, paste0("(", repeat_unit_seq, ")", "{", min_n_repeats, ",}"),  right_flank_seq), seq, max.distance = max.distance)
    match_seq_list <- regmatches(seq, repeat_match)
    return(match_seq_list)
  }

  calculation <- function(x) {
    value <- as.numeric(x) > interruptions_repeat_no
    return(sum(as.numeric(x)[which(value)[1]:which(value)[length(which(value))]]))
  }

  f_match_seq_list <- repeat_matcher(fastq_df$Sequence)
  f_found_match_test <- sapply(f_match_seq_list, function(x) length(x) > 0)

  # if hasn't found match search reverse complement
  r_subset <- fastq_df[!f_found_match_test, ]
  r_subset$Sequence <-  microseq::reverseComplement(r_subset$Sequence)

  r_match_seq_list <- repeat_matcher(r_subset$Sequence)

  f_subset <- fastq_df
  f_subset$matched_sequence <- f_match_seq_list
  r_subset$matched_sequence <- r_match_seq_list


  r_subset <- r_subset[sapply(r_match_seq_list, function(x) length(x) > 0), ]
  f_subset <- f_subset[f_found_match_test, ]

  df <- rbind(f_subset, r_subset)
  df$matched_sequence <- sapply(df$matched_sequence, function(x) x[[1]])
  # df$repeat_length <- paste0(round((nchar(df$matched_sequence)- nchar(paste0(left_flank_seq, right_flank_seq)))/nchar(repeat_unit_seq)))
  # df$repeat_length <- as.numeric(df$repeat_length)
  df$short_seq <- sapply(df$matched_sequence, function(seq){
    seq_subset <- substr(seq, codon_start, nchar(seq))
    codon_list <- paste0(
      rle(unlist(strsplit(seq_subset, paste0("(?<=.{", str_length(repeat_unit_seq), "})"), perl = TRUE)))$values,
      rle(unlist(strsplit(seq_subset, paste0("(?<=.{", str_length(repeat_unit_seq), "})"), perl = TRUE)))$lengths
    )
    codon_vector <- paste(codon_list, collapse = " ")
    return(codon_vector)
  })

  df <- transform(df, repeat_length = str_extract_all(short_seq, '\\d+(\\.\\d+)?')) |>
    transform(short_seq_longest_repeat_length = map_dbl(repeat_length, ~max(as.numeric(.x)))) |>
    transform(repeat_length = sapply(repeat_length, calculation)) |>
    transform(Error_in_LFS = ifelse(grepl(paste0(left_flank_seq), df$Sequence), "No", "Yes")) |>
    transform(Error_in_RFS = ifelse(grepl(paste0(right_flank_seq), df$Sequence), "No", "Yes")) |>
    transform(Error_in_Repeat = ifelse(short_seq_longest_repeat_length < repeat_length, "Yes", "No"))

  return(df)
}

# instability index ---------------------------------------------------------
instability_index <- function(repeats,
                              signals,
                              index_peak_signal,
                              index_peak_repeat,
                              peak_threshold,
                              abs_sum = FALSE) {
  # apply signal threshold
  peak_over_threshold <- which(signals / index_peak_signal > peak_threshold)
  repeats <- repeats[peak_over_threshold]
  signals <- signals[peak_over_threshold]

  # normalized peak signal
  signals_normalized <- signals / sum(signals)

  # distance to index peak
  repeat_delta <- repeats - index_peak_repeat
  if (abs_sum == FALSE) {
    sum(signals_normalized * repeat_delta)
  } else if (abs_sum == TRUE) {
    sum(abs(signals_normalized * repeat_delta))
  }
}

# function for finding quantiles -----------------------------------------------

find_percentiles <- function(repeats,
                             signals,
                             index_peak_repeat,
                             type, # "percentile" or "repeat"
                             range,
                             col_prefix) {
  # if there are double peaks select the tallest of the peaks, otherwise approx interpolation doesn't work
  # also if there are no main peak called, filter out (for example samples used to call repeats but irrelevant for metrics)
  df_names <- paste(col_prefix, range, sep = "_")

  # Deal with case when there are no expansion peaks by returning 0s
  if (sum(repeats > index_peak_repeat) <= 1) {
    percentile_df <- as.data.frame(setNames(as.list(rep(0, length(range))), df_names))
  } else {
    unique_repeat_df <- aggregate(signals ~ repeats, FUN = max)
    cumsum_pct <- cumsum(unique_repeat_df$signals) / sum(unique_repeat_df$signals)
    repeat_delta <- unique_repeat_df$repeats - index_peak_repeat

    values <- vector("numeric", length(range))

    if (type == "percentile") {
      for (i in seq_along(range)) {
        values[[i]] <- approx(cumsum_pct,
                              repeat_delta,
                              xout = range[[i]],
                              yleft = min(repeat_delta)
        )$y
      }
    } else if (type == "repeat") {
      for (i in seq_along(range)) {
        values[[i]] <- approx(repeat_delta,
                              cumsum_pct,
                              xout = range[[i]],
                              yleft = min(cumsum_pct)
        )$y
      }
    }

    percentile_df <- as.data.frame(setNames(as.list(values), df_names))
  }

  return(percentile_df)
}


# skewness ------------------------------------------------------------------

fishers_skewness <- function(x, y) {
  mean_val <- sum(x * y)
  sd_val <- sqrt(sum(y * (x - mean_val)^2))

  skewness <- sum(y * (x - mean_val)^3) / sd_val^3

  return(skewness)
}


# kurtosis -----------------------------------------------------------------

fishers_kurtosis <- function(x, y) {
  mean_val <- sum(x * y)
  sd_val <- sqrt(sum(y * (x - mean_val)^2))

  kurtosis <- (sum(y * (x - mean_val)^4) / sd_val^4) - 3
  return(kurtosis)
}

# subsetting repeat table ---------------------------------------------------

repeat_table_subset <- function(repeat_table_df,
                                allele_signal,
                                index_repeat,
                                peak_threshold,
                                window_around_index_peak) {
  # Filter to include only the peaks above the certain threshold
  # signal threshold is set on the modal peak rather than the index peak
  repeat_table_df$peak_percent <- repeat_table_df$signal / allele_signal
  signal_filtered_df <- repeat_table_df[which(repeat_table_df$peak_percent > peak_threshold), ]

  # Ensure window_around_index_peak is exactly length 2
  if (length(window_around_index_peak) != 2) {
    stop("window_around_index_peak must be a vector of length 2")
  }

  # Filter to include only peaks of a certain size
  lower_lim <- ifelse(is.na(window_around_index_peak[1]),
                      min(signal_filtered_df$repeats),
                      index_repeat - abs(window_around_index_peak[1])
  )
  upper_lim <- ifelse(is.na(window_around_index_peak[1]),
                      max(signal_filtered_df$repeats),
                      index_repeat + abs(window_around_index_peak[2])
  )
  size_filtered_df <- signal_filtered_df[which(signal_filtered_df$repeats >= lower_lim & signal_filtered_df$repeats <= upper_lim), ]

  return(size_filtered_df)
}

# Calculate metrics -------------------------------------------------------

calculate_instability_metrics_fastq <- function(
    fragments_list,
    peak_threshold = 0.05,
    window_around_index_peak = c(NA, NA),
    percentile_range = c(0.5, 0.75, 0.9, 0.95),
    repeat_range = c(2, 5, 10, 20)) {
  # calculate metrics
  metrics_list <- lapply(fragments_list, function(fragments_repeats) {
    # check to make sure all the required steps for the function have been done
    # if(fragments_repeats$.__enclos_env__$private$find_main_peaks_used == FALSE){
    #   stop(paste0(fragments_repeats$unique_id, " requires called alleles to calculate repeat instability metrics. Use 'find_alleles()'."),
    #        call. = FALSE
    #   )
    # }
    if(fragments_repeats$.__enclos_env__$private$assigned_index_peak_used == FALSE){
      stop(paste0(fragments_repeats$unique_id, " requires an index peak to calculate repeat instability metrics. Use 'assign_index_peaks' to set the index peaks."),
           call. = FALSE
      )
    }

    # return early under different situations and record a reason why
    if (nrow(fragments_repeats$repeat_table_df) == 0) {
      fragments_repeats$.__enclos_env__$private$metrics_qc_message <- "Skip reason: sample has no data"
      return(NULL)
    } else if (is.na(fragments_repeats$get_allele_peak()$allele_repeat)) {
      fragments_repeats$.__enclos_env__$private$metrics_qc_message <- "Skip reason: no allele found in sample"
      return(NULL)
    } else if (fragments_repeats$.__enclos_env__$private$assigned_index_peak_grouped == TRUE &&  is.na(fragments_repeats$get_index_peak()$index_repeat)){
      # because of the warning above we know that there is data in this sample, but the issue came from the index grouping
      fragments_repeats$.__enclos_env__$private$metrics_qc_message <- "Skip reason: Invalid index peak in sample grouping. Issue likely with `metrics_baseline_control` sample(s) that pairs with this sample."
      return(NULL)
    }

    # no issues so set this as blank in case calculate_instability_metrics was run with an issue previously
    fragments_repeats$.__enclos_env__$private$metrics_qc_message <- NA_character_



    # filter dataset to user supplied thresholds
    size_filtered_df <- repeat_table_subset(
      repeat_table_df = fragments_repeats$repeat_table_df,
      allele_signal = fragments_repeats$get_allele_peak()$allele_signal,
      index_repeat = fragments_repeats$get_index_peak()$index_repeat,
      peak_threshold = peak_threshold,
      window_around_index_peak = window_around_index_peak
    )

    if(!is.null(fragments_repeats$.__enclos_env__$private$index_samples) && length(fragments_repeats$.__enclos_env__$private$index_samples) > 0){
      control_weighted_mean_repeat <- sapply(fragments_repeats$.__enclos_env__$private$index_samples, function(x){
        control_filtered_df <- repeat_table_subset(
          repeat_table_df = x[[2]],
          allele_signal = x[[2]][which(x[[2]]$repeats == x[[1]]), "signal"],
          index_repeat = x[[1]],
          peak_threshold = peak_threshold,
          window_around_index_peak = window_around_index_peak
        )

        weighted.mean(control_filtered_df$repeats, control_filtered_df$signal)
      })

      index_weighted_mean_repeat <- median(control_weighted_mean_repeat, na.rm = TRUE)
    } else{
      index_weighted_mean_repeat <- NA
    }

    # first subset to make some dataframe that are just for contractions or expansions
    size_filtered_df$repeat_delta_index_peak <- size_filtered_df$repeats - fragments_repeats$get_index_peak()$index_repeat
    expansion_filtered <- size_filtered_df[which(size_filtered_df$repeat_delta_index_peak >= 0), ]
    contraction_filtered <- size_filtered_df[which(size_filtered_df$repeat_delta_index_peak <= 0), ]

    # QCs
    QC_modal_peak_signal <- if (fragments_repeats$get_allele_peak()$allele_signal > 500) {
      NA_character_
    } else if (fragments_repeats$get_allele_peak()$allele_signal > 100) {
      "Low"
    } else {
      "Extremely low"
    }

    QC_peak_number <- if (nrow(fragments_repeats$repeat_table_df) > 20) {
      NA_character_
    } else if (nrow(fragments_repeats$repeat_table_df) > 10) {
      "Low"
    } else {
      "Extremely low"
    }

    QC_off_scale <- if (any(fragments_repeats$repeat_table_df$off_scale)) {
      paste(
        "The following repeats were determined off scale (check ladder too, could be scans in any channel):",
        paste(round(fragments_repeats$repeat_table_df[which(fragments_repeats$repeat_table_df$off_scale), "repeats"]), collapse = ", ")
      )
    } else {
      NA_character_
    }

    # make a wide dataframe
    metrics <- data.frame(
      unique_id = fragments_repeats$unique_id,
      QC_comments = NA_character_,
      QC_modal_peak_signal = QC_modal_peak_signal,
      QC_peak_number = QC_peak_number,
      QC_off_scale = QC_off_scale,
      modal_peak_repeat = fragments_repeats$get_allele_peak()$allele_repeat,
      modal_peak_signal = fragments_repeats$get_allele_peak()$allele_signal,
      index_peak_repeat = fragments_repeats$get_index_peak()$index_repeat,
      index_peak_signal = fragments_repeats$get_index_peak()$index_signal,
      index_weighted_mean_repeat = index_weighted_mean_repeat,
      n_peaks_total = nrow(fragments_repeats$repeat_table_df),
      n_peaks_analysis_subset = nrow(size_filtered_df),
      n_peaks_analysis_subset_expansions = nrow(expansion_filtered),
      min_repeat = min(size_filtered_df$repeats),
      max_repeat = max(size_filtered_df$repeats),
      mean_repeat = mean(size_filtered_df$repeats),
      weighted_mean_repeat = weighted.mean(size_filtered_df$repeats, size_filtered_df$signal),
      median_repeat = median(size_filtered_df$repeats),
      max_signal = max(size_filtered_df$signal),
      max_delta_neg = min(size_filtered_df$repeat_delta_index_peak),
      max_delta_pos = max(size_filtered_df$repeat_delta_index_peak),
      skewness = fishers_skewness(size_filtered_df$repeats, size_filtered_df$signal),
      kurtosis = fishers_kurtosis(size_filtered_df$repeats, size_filtered_df$signal),
      modal_repeat_delta = fragments_repeats$get_allele_peak()$allele_repeat - fragments_repeats$get_index_peak()$index_repeat,
      average_repeat_gain = weighted.mean(size_filtered_df$repeats, size_filtered_df$signal) - index_weighted_mean_repeat,
      instability_index = instability_index(
        repeats = size_filtered_df$repeats,
        signals = size_filtered_df$signal,
        index_peak_signal = fragments_repeats$get_allele_peak()$allele_signal,
        index_peak_repeat = fragments_repeats$get_index_peak()$index_repeat,
        peak_threshold = peak_threshold,
        abs_sum = FALSE
      ),
      instability_index_abs = instability_index(
        repeats = size_filtered_df$repeats,
        signals = size_filtered_df$signal,
        index_peak_signal = fragments_repeats$get_allele_peak()$allele_signal,
        index_peak_repeat = fragments_repeats$get_index_peak()$index_repeat,
        peak_threshold = peak_threshold,
        abs_sum = TRUE
      ),
      expansion_index = instability_index(
        repeats = expansion_filtered$repeats,
        signals = expansion_filtered$signal,
        index_peak_signal = fragments_repeats$get_allele_peak()$allele_signal,
        index_peak_repeat = fragments_repeats$get_index_peak()$index_repeat,
        peak_threshold = peak_threshold,
        abs_sum = FALSE
      ),
      contraction_index = instability_index(
        repeats = contraction_filtered$repeats,
        signals = contraction_filtered$signal,
        index_peak_signal = fragments_repeats$get_allele_peak()$allele_signal,
        index_peak_repeat = fragments_repeats$get_index_peak()$index_repeat,
        peak_threshold = peak_threshold,
        abs_sum = FALSE
      ),
      expansion_ratio = sum(expansion_filtered$peak_percent) - 1, # remove the main peak by subtracting 1
      contraction_ratio = sum(contraction_filtered$peak_percent) - 1
    )

    expansion_percentile <- find_percentiles(
      expansion_filtered$repeats,
      expansion_filtered$signal,
      fragments_repeats$get_index_peak()$index_repeat,
      type = "percentile",
      range = percentile_range,
      col_prefix = "expansion_percentile"
    )

    expansion_repeat <- find_percentiles(
      expansion_filtered$repeats,
      expansion_filtered$signal,
      fragments_repeats$get_index_peak()$index_repeat,
      type = "repeat",
      range = repeat_range,
      col_prefix = "expansion_percentile_for_repeat"
    )

    metrics <- cbind(metrics, expansion_percentile)
    metrics <- cbind(metrics, expansion_repeat)

    return(metrics)
  })

  metrics <- do.call(rbind, metrics_list)

  # add back in any samples that were removed earlier or failed to calculate metrics (they are returned as NULL and therefore not in the dataframe)
  missing_samples <- names(fragments_list)[!names(fragments_list) %in% metrics$unique_id]
  if (length(missing_samples) > 0) {
    metrics[nrow(metrics) + seq_along(missing_samples), "unique_id"] <- missing_samples
    rownames(metrics) <- metrics$unique_id

    # add in the reason for skip
    metrics$QC_comments <- sapply(fragments_list, function(x) x$.__enclos_env__$private$metrics_qc_message)[metrics$unique_id]

  }

  return(metrics)
}
