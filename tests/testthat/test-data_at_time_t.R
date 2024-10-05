test_that(
  desc = "Censoring matches survival::Surv()",
  code = {
    expect_no_condition(
      object =
        prepared_test_data_tte <-
        prepare_monitored_study_data(
          data = test_data,
          study_time = 20,
          id_variable = ".id",
          covariates_variables = c("x_1"),
          enrollment_time_variable = "enrollment",
          treatment_variable = c("tx"),
          outcome_variables = c("event_1", "event_2"),
          outcome_time_variables = c("tte_1", "tte_2"),
          observe_missing_times = c(0, 0),
          outcomes_sequential = FALSE,
          time_to_event = TRUE
        )
    )

    tte_1_at_time_t <- tte_2_at_time_t <- list()
    unique_times_1 <-
      sort(unique(with(data = test_data, enrollment + tte_1)))
    unique_times_2 <-
      sort(unique(with(data = test_data, enrollment + tte_2)))

    events_at_time_1 <- censor_at_time_1 <- at_risk_at_time_1 <-
      NA*unique_times_1
    events_at_time_2 <- censor_at_time_2 <- at_risk_at_time_2 <-
      NA*unique_times_2

    for(i in 1:length(unique_times_1)){
      tte_1_at_time_t[[i]] <-
        data_at_time_t(
          prepared_data = prepared_test_data_tte,
          study_time = unique_times_1[i]
        )$data

      events_at_time_1[i] <-
        with(
          data = tte_1_at_time_t[[i]],
          expr = sum(event_1 %in% 1 & (`tte_1` + `.e`) == unique_times_1[i])
        )
    }

    censor_times <-
      with(
        data = tail(x = tte_1_at_time_t, n = 1)[[1]],
        expr =
          (`.e` + tte_1)[which(event_1 == 0)]
      )

    for(i in 1:length(unique_times_1)){
      censor_at_time_1[i] <- sum(censor_times == unique_times_1[i])
      at_risk_at_time_1[i] <-
        with(
          data = tail(x = tte_1_at_time_t, n = 1)[[1]],
          expr =
            sum((`.e` < unique_times_1[i]) &
                  (`.e` + tte_1) >= unique_times_1[i])
        )
    }


    for(i in 1:length(unique_times_2)){
      tte_2_at_time_t[[i]] <-
        data_at_time_t(
          prepared_data = prepared_test_data_tte,
          study_time = unique_times_2[i]
        )$data

      events_at_time_2[i] <-
        with(
          data = tte_2_at_time_t[[i]],
          sum(event_2 %in% 1 & (`tte_2` + `.e`) == unique_times_2[i])
        )
    }

    censor_times <-
      with(
        data = tail(x = tte_2_at_time_t, n = 1)[[1]],
        expr =
          (`.e` + tte_2)[which(event_2 == 0)]
      )

    for(i in 1:length(unique_times_2)){
      censor_at_time_2[i] <- sum(censor_times == unique_times_2[i])
      at_risk_at_time_2[i] <-
        with(
          data = tail(x = tte_2_at_time_t, n = 1)[[1]],
          expr =
            sum((`.e` < unique_times_2[i]) &
                  (`.e` + tte_2) >= unique_times_2[i])
        )
    }

    surv_event_1 <-
      survival::survfit(
        survival::Surv(
          time =  enrollment,
          time2 = enrollment + tte_1,
          event = event_1
        ) ~ 1,
        data = test_data
      )

    surv_event_2 <-
      survival::survfit(
        survival::Surv(
          time =  enrollment,
          time2 = enrollment + tte_2,
          event = event_2
        ) ~ 1,
        data = test_data
      )

    expect_equal(
      object = unique_times_1,
      expected = surv_event_1$time
    )

    expect_equal(
      object = events_at_time_1,
      expected = surv_event_1$n.event
    )

    expect_equal(
      object = censor_at_time_1,
      expected = surv_event_1$n.censor
    )

    expect_equal(
      object = at_risk_at_time_1,
      expected = surv_event_1$n.risk
    )

    expect_equal(
      object = unique_times_2,
      expected = surv_event_2$time
    )

    expect_equal(
      object = events_at_time_2,
      expected = surv_event_2$n.event
    )

    expect_equal(
      object = censor_at_time_2,
      expected = surv_event_2$n.censor
    )

    expect_equal(
      object = at_risk_at_time_2,
      expected = surv_event_2$n.risk
    )
  }
)

test_that(
  desc = "Continuous/Binary/Ordinal Outcomes",
  code = {
    expect_no_condition(
      object =
        # prepared_test_data_numeric <-
        prepared_data <-
        prepare_monitored_study_data(
          data = test_data,
          study_time = 20,
          id_variable = ".id",
          covariates_variables = c("x_1"),
          enrollment_time_variable = "enrollment",
          treatment_variable = c("tx"),
          outcome_variables = c("y_1", "y_2"),
          outcome_time_variables = c(".t_1", ".t_2"),
          observe_missing_times = c(7, 14),
          outcomes_sequential = TRUE
        )
    )

    # # Check Against Double Coding of Event Counts
    # event_times <-
    #   c(1:10, (1:10) + 7) |> unique() |> sort()
    #
    # numeric_at_time_t <- list()
    #
    # n_outcome_1 <- n_outcome_2 <- event_times*NA
    #
    # for(i in 1:length(event_times)){
    #   numeric_at_time_t[[i]] <-
    #     data_at_time_t(
    #       prepared_data = prepared_test_data_numeric,
    #       study_time = event_times[i]
    #     )
    # }
  }
)
