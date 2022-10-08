subtract_scale <-
  function(data,
           id,
           value
  ){

    library(dplyr)
    library(magrittr)

    {{data}} %>%
      mutate(across(.cols = c(everything(), -{{id}}),
                    ~.x - value))
  }

# reverse scoring is messed up. It converts back to original qualtrics scale 1 - ...
composite_total_avg_fun <- function(
    data,
    id,
    reverse = NULL,
    max_value = NULL,
    n_items
){

  library(magrittr)
  library(dplyr)
  library(tidyselect)
  library(tibble)

  dataset <- {{data}} %>%
    select(
      -{{id}}
    )

  no_reverse <- dataset %>%
    select(
      -all_of({{reverse}})
    )

  need_reverse <- dataset %>%
    select(
      all_of({{reverse}})
    ) %>%
    mutate(
      across(
        .cols = everything(),
        ~1 + max_value - .x,
        .names = '{.col}_r'
      )
    )
  # select(all_of(reverse))

  comp_data <- tibble(no_reverse, need_reverse) %>%
    select(
      -all_of({{reverse}})
    ) %>%
    mutate(
      sum_values = rowSums(
        across(
          .cols = everything()
        ),
        na.rm = TRUE
      ),
      mean_values = sum_values/{{n_items}}
    )

  full_data <- left_join(comp_data, {{data}})

  message('Make sure that your variables are coded correctly before using these composite scores.')

  tibble(full_data)
}

subscale_create <- function(data,
                            total_only = c(TRUE, FALSE),
                            scale1,
                            scale1_nitems,
                            scale2,
                            scale2_nitems,
                            scale3 = NULL,
                            scale3_nitems = NULL,
                            scale4 = NULL,
                            scale4_nitems = NULL,
                            scale5 = NULL,
                            scale5_nitems = NULL,
                            scale6 = NULL,
                            scale6_nitems = NULL,
                            scale7 = NULL,
                            scale7_nitems = NULL,
                            scale8 = NULL,
                            scale8_nitems = NULL,
                            scale9 = NULL,
                            scale9_nitems = NULL,
                            scale10 = NULL,
                            scale10_nitems = NULL
){

  library(magrittr)
  library(dplyr)
  library(tidyselect)

  if (total_only == TRUE){

    if(is.null(scale3) &
       is.null(scale3_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE)
        )
    }

    else if(is.null(scale4) &
            is.null(scale4_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE)
        )
    }

    else if(is.null(scale5) &
            is.null(scale5_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE)
        )

    }

    else if(is.null(scale6) &
            is.null(scale6_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE),
          total5 = rowSums(subset({{data}}, select = scale5), na.rm = TRUE)
        )

    }

    else if(is.null(scale7) &
            is.null(scale7_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE),
          total5 = rowSums(subset({{data}}, select = scale5), na.rm = TRUE),
          total6 = rowSums(subset({{data}}, select = scale6), na.rm = TRUE)
        )

    }

    else if(is.null(scale8) &
            is.null(scale8_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE),
          total5 = rowSums(subset({{data}}, select = scale5), na.rm = TRUE),
          total6 = rowSums(subset({{data}}, select = scale6), na.rm = TRUE),
          total7 = rowSums(subset({{data}}, select = scale7), na.rm = TRUE)
        )

    }

    else if(is.null(scale9) &
            is.null(scale9_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE),
          total5 = rowSums(subset({{data}}, select = scale5), na.rm = TRUE),
          total6 = rowSums(subset({{data}}, select = scale6), na.rm = TRUE),
          total7 = rowSums(subset({{data}}, select = scale7), na.rm = TRUE),
          total8 = rowSums(subset({{data}}, select = scale8), na.rm = TRUE)
        )

    }

    else if(is.null(scale10) &
            is.null(scale10_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE),
          total5 = rowSums(subset({{data}}, select = scale5), na.rm = TRUE),
          total6 = rowSums(subset({{data}}, select = scale6), na.rm = TRUE),
          total7 = rowSums(subset({{data}}, select = scale7), na.rm = TRUE),
          total8 = rowSums(subset({{data}}, select = scale8), na.rm = TRUE),
          total9 = rowSums(subset({{data}}, select = scale9), na.rm = TRUE)
        )

    }

    else {

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE),
          total5 = rowSums(subset({{data}}, select = scale5), na.rm = TRUE),
          total6 = rowSums(subset({{data}}, select = scale6), na.rm = TRUE),
          total7 = rowSums(subset({{data}}, select = scale7), na.rm = TRUE),
          total8 = rowSums(subset({{data}}, select = scale8), na.rm = TRUE),
          total9 = rowSums(subset({{data}}, select = scale9), na.rm = TRUE),
          total10 = rowSums(subset({{data}}, select = scale10), na.rm = TRUE)
        )

    }

  }

  else if(total_only == FALSE){

    if(is.null(scale3) &
       is.null(scale3_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          avg1 = total1/scale1_nitems,
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          avg2 = total2/scale2_nitems
        )
    }

    else if(is.null(scale4) &
            is.null(scale4_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          avg1 = total1/scale1_nitems,
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          avg2 = total2/scale2_nitems,
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          avg3 = total3/scale3_nitems
        )
    }

    else if(is.null(scale5) &
            is.null(scale5_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          avg1 = total1/scale1_nitems,
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          avg2 = total2/scale2_nitems,
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          avg3 = total3/scale3_nitems,
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE),
          avg4 = total4/scale4_nitems
        )

    }

    else if(is.null(scale6) &
            is.null(scale6_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          avg1 = total1/scale1_nitems,
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          avg2 = total2/scale2_nitems,
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          avg3 = total3/scale3_nitems,
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE),
          avg4 = total4/scale4_nitems,
          total5 = rowSums(subset({{data}}, select = scale5), na.rm = TRUE),
          avg5 = total5/scale5_nitems
        )

    }

    else if(is.null(scale7) &
            is.null(scale7_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          avg1 = total1/scale1_nitems,
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          avg2 = total2/scale2_nitems,
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          avg3 = total3/scale3_nitems,
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE),
          avg4 = total4/scale4_nitems,
          total5 = rowSums(subset({{data}}, select = scale5), na.rm = TRUE),
          avg5 = total5/scale5_nitems,
          total6 = rowSums(subset({{data}}, select = scale6), na.rm = TRUE),
          avg6 = total6/scale6_nitems
        )

    }

    else if(is.null(scale8) &
            is.null(scale8_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          avg1 = total1/scale1_nitems,
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          avg2 = total2/scale2_nitems,
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          avg3 = total3/scale3_nitems,
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE),
          avg4 = total4/scale4_nitems,
          total5 = rowSums(subset({{data}}, select = scale5), na.rm = TRUE),
          avg5 = total5/scale5_nitems,
          total6 = rowSums(subset({{data}}, select = scale6), na.rm = TRUE),
          avg6 = total6/scale6_nitems,
          total7 = rowSums(subset({{data}}, select = scale7), na.rm = TRUE),
          avg7 = total7/scale7_nitems
        )

    }

    else if(is.null(scale9) &
            is.null(scale9_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          avg1 = total1/scale1_nitems,
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          avg2 = total2/scale2_nitems,
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          avg3 = total3/scale3_nitems,
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE),
          avg4 = total4/scale4_nitems,
          total5 = rowSums(subset({{data}}, select = scale5), na.rm = TRUE),
          avg5 = total5/scale5_nitems,
          total6 = rowSums(subset({{data}}, select = scale6), na.rm = TRUE),
          avg6 = total6/scale6_nitems,
          total7 = rowSums(subset({{data}}, select = scale7), na.rm = TRUE),
          avg7 = total7/scale7_nitems,
          total8 = rowSums(subset({{data}}, select = scale8), na.rm = TRUE),
          avg8 = total8/scale8_nitems
        )

    }

    else if(is.null(scale10) &
            is.null(scale10_nitems)){

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          avg1 = total1/scale1_nitems,
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          avg2 = total2/scale2_nitems,
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          avg3 = total3/scale3_nitems,
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE),
          avg4 = total4/scale4_nitems,
          total5 = rowSums(subset({{data}}, select = scale5), na.rm = TRUE),
          avg5 = total5/scale5_nitems,
          total6 = rowSums(subset({{data}}, select = scale6), na.rm = TRUE),
          avg6 = total6/scale6_nitems,
          total7 = rowSums(subset({{data}}, select = scale7), na.rm = TRUE),
          avg7 = total7/scale7_nitems,
          total8 = rowSums(subset({{data}}, select = scale8), na.rm = TRUE),
          avg8 = total8/scale8_nitems,
          total9 = rowSums(subset({{data}}, select = scale9), na.rm = TRUE),
          avg9 = total9/scale9_nitems
        )

    }

    else {

      {{data}} %>%
        mutate(
          total1 = rowSums(subset({{data}}, select = scale1), na.rm = TRUE),
          avg1 = total1/scale1_nitems,
          total2 = rowSums(subset({{data}}, select = scale2), na.rm = TRUE),
          avg2 = total2/scale2_nitems,
          total3 = rowSums(subset({{data}}, select = scale3), na.rm = TRUE),
          avg3 = total3/scale3_nitems,
          total4 = rowSums(subset({{data}}, select = scale4), na.rm = TRUE),
          avg4 = total4/scale4_nitems,
          total5 = rowSums(subset({{data}}, select = scale5), na.rm = TRUE),
          avg5 = total5/scale5_nitems,
          total6 = rowSums(subset({{data}}, select = scale6), na.rm = TRUE),
          avg6 = total6/scale6_nitems,
          total7 = rowSums(subset({{data}}, select = scale7), na.rm = TRUE),
          avg7 = total7/scale7_nitems,
          total8 = rowSums(subset({{data}}, select = scale8), na.rm = TRUE),
          avg8 = total8/scale8_nitems,
          total9 = rowSums(subset({{data}}, select = scale9), na.rm = TRUE),
          avg9 = total9/scale9_nitems,
          total10 = rowSums(subset({{data}}, select = scale10), na.rm = TRUE),
          avg10 = total10/scale10_nitems
        )

    }
  }
}

prop_fun_plot <- function(data,
                          x,
                          decline = FALSE
){
  library(ggplot2)
  library(dplyr)
  library(magrittr)

  x <- enquo(x)

  if (decline == TRUE){

    table_x <-
      {{data}} %>%
      group_by(!!x) %>%
      summarize(n = n(),
                prop = n/nrow({{data}}),
                percentage = scales::percent(prop)) %>%
      ungroup()

    plot_x <-
      table_x %>%
      mutate(
        declined = case_when(
          !!x == -77 ~ 'Decline',
          TRUE ~ 'Response'
        )
      ) %>%
      ggplot(aes(!!x, n)) +
      geom_col(color = 'white',
               fill = 'gray50',
               alpha = .7) +
      geom_text(aes(label = percentage), vjust = 1.5) +
      geom_text(aes(label = n), vjust = -.5) +
      facet_wrap(vars(declined), scales = 'free')

    return(list(table_x, plot_x))
  }

  else {

    table_x <-
      {{data}} %>%
      group_by(!!x) %>%
      summarize(n = n(),
                prop = n/nrow({{data}}),
                percentage = scales::percent(prop)) %>%
      ungroup()

    plot_x <-
      table_x %>%
      ggplot(aes(!!x, n)) +
      geom_col(color = 'white',
               fill = 'gray50',
               alpha = .7) +
      geom_text(aes(label = percentage), vjust = 2) +
      geom_text(aes(label = n), vjust = -.5)

    return(list(table_x, plot_x))
  }
}


composite_hist <- function(
    data,
    x,
    bins = 20
){

  library(magrittr)
  library(dplyr)
  library(ggplot2)

  {{data}} %>%
    ggplot(aes({{x}})) +
    geom_histogram(bins = {{bins}},
                   color = 'white',
                   fill = 'black',
                   alpha = .7)
}

cutoff_plot <-
  function(data,
           x,
           cutoff = 0,
           cutoff_color = '#30123BFF',
           cutoff_other = NULL,
           cutoff_other_color = '#1AE4B6FF',
           cutoff_other2 = NULL,
           cutoff_other2_color = '#FABA39FF',
           bins = 20){

    library(magrittr)
    library(dplyr)
    library(ggplot2)

    x <- enquo(x)

    if(is.null(cutoff_other)){

      {{data}} %>%
        ggplot(aes({{x}})) +
        geom_histogram(bins = {{bins}},
                       color = 'white',
                       fill = 'black',
                       alpha = .7) +
        geom_vline(xintercept = {{cutoff}},
                   color = {{cutoff_color}},
                   linetype = 2,
                   size = 1.25)
    }

    else if(is.null(cutoff_other2)){

      {{data}} %>%
        ggplot(aes({{x}})) +
        geom_histogram(bins = {{bins}},
                       color = 'white',
                       fill = 'black',
                       alpha = .7) +
        geom_vline(xintercept = {{cutoff}},
                   color = {{cutoff_color}},
                   linetype = 2,
                   size = 1.25) +
        geom_vline(xintercept = {{cutoff_other}},
                   color = {{cutoff_other_color}},
                   linetype = 3,
                   size = 1.25)
    }

    else {

      {{data}} %>%
        ggplot(aes({{x}})) +
        geom_histogram(bins = {{bins}},
                       color = 'white',
                       fill = 'black',
                       alpha = .7) +
        geom_vline(xintercept = {{cutoff}},
                   color = {{cutoff_color}},
                   linetype = 2,
                   size = 1.25) +
        geom_vline(xintercept = {{cutoff_other}},
                   color = {{cutoff_other_color}},
                   linetype = 3,
                   size = 1.25) +
        geom_vline(xintercept = {{cutoff_other2}},
                   color = {{cutoff_other2_color}},
                   linetype = 4,
                   size = 1.25)
    }
  }

index_total_fun <-
  function(
    data,
    id
  ){

    library(magrittr)
    library(dplyr)
    library(tidyselect)

    full_data <-
      {{data}} %>%
      mutate(
        sum_values = rowSums(
          across(
            .cols = c(everything(), -{{id}})
          ),
          na.rm = TRUE
        )
      )

    message('Make sure that your variables are coded correctly before using these composite scores.')

    tibble::as_tibble(full_data)
  }

pct_miss_fun <- function(
    data,
    id,
    n_items
){

  library(magrittr)
  library(dplyr)
  library(tidyr)

  {{data}} %>%
    pivot_longer(
      cols = -{{id}},
      names_to = 'items',
      values_to = 'item_values'
    ) %>%
    group_by(id, items) %>%
    count(item_values) %>%
    filter(item_values == -77 |
             is.na(item_values)) %>%
    ungroup() %>%
    group_by(id) %>%
    summarize(
      missing_n = sum(n)
    ) %>%
    mutate(
      miss_pct = (missing_n/{{n_items}})*100
    ) %>%
    ungroup() %>%
    arrange(desc(miss_pct))
}


severity_plot <- function(
    data,
    x,
    bins = 20,
    low_xmin = 1,
    low_xmax = 5,
    medium_xmin = NULL,
    medium_xmax = NULL,
    large_xmin = NULL,
    large_xmax = NULL,
    critical_xmin = NULL,
    critical_xmax = NULL
)
{

  library(ggplot2)
  library(dplyr)
  library(magrittr)

  x <- tidyselect::enquo(x)

  histo_plot <-
    {{data}} %>%
    ggplot(
      aes(
        {{x}}
      )
    ) +
    geom_histogram(
      bins = {{bins}},
      color = 'white',
      fill = 'black',
      alpha = .7
    )

  plot_data <- ggplot_build(histo_plot)

  plot_data <- plot_data$data[[1]]

  histo_plot +
    # geom_vline(
    #   xintercept = {{low_xmax}},
    #   color = 'black',
    #   linetype = 2
    # ) +
    # geom_vline(
    #   xintercept = {{medium_xmax}},
    #   color = 'black',
    #   linetype = 2
    # ) +
    # geom_vline(
  #   xintercept = {{large_xmax}},
  #   color = 'black',
  #   linetype = 2
  # ) +
  # geom_vline(
  #   xintercept = {{critical_xmax}},
  #   color = 'black',
  #   linetype = 2
  # ) +
  annotate(
    geom = 'rect',
    fill = '#30123BFF',
    alpha = .5,
    xmin = {{low_xmin}},
    xmax = {{low_xmax}},
    ymin = min(plot_data$data[[1]]$ymin),
    ymax = max(plot_data$data[[1]]$ymax)
  ) +
    annotate(
      geom = 'rect',
      fill = '#1AE4B6FF',
      alpha = .5,
      xmin = {{medium_xmin}},
      xmax = {{medium_xmax}},
      ymin = min(plot_data$data[[1]]$ymin),
      ymax = max(plot_data$data[[1]]$ymax)
    ) +
    annotate(
      geom = 'rect',
      fill = '#FABA39FF',
      alpha = .5,
      xmin = {{large_xmin}},
      xmax = {{large_xmax}},
      ymin = min(plot_data$data[[1]]$ymin),
      ymax = max(plot_data$data[[1]]$ymax)
    ) +
    annotate(
      geom = 'rect',
      fill = '#7A0403FF',
      alpha = .5,
      xmin = {{critical_xmin}},
      xmax = {{critical_xmax}},
      ymin = min(plot_data$data[[1]]$ymin),
      ymax = max(plot_data$data[[1]]$ymax)
    )
}
