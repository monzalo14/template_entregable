#!/usr/bin/env Rscript
# Function to plot a graphical model

graficar_red <- function(p_dag){
  cbbPalette <- c("reportada" = "#C84630",
                  "verificada" = "#0099CC",
                  "otra" = "#55868C",
                  "municipal" = "darkorange")

  graf_1 <- tidygraph::as_tbl_graph(bnlearn::as.graphNEL(p_dag))
  graf_1 <- graf_1 %>%
            tidygraph::activate(nodes) %>%
            dplyr::mutate(tipo = ifelse(str_detect(name, "_v"), "verificada",
                                 ifelse(name %in% municipales, "municipal",
                                        "reportada"))) %>%
            tidygraph::activate(edges) %>%
            dplyr::mutate(tipo = ifelse(.N()$tipo[from] == "verificada" &
                                        .N()$tipo[to] == "verificada", "verificada",
                                        ifelse(.N()$tipo[from] == 'municipal' &
                                               .N()$tipo[to] == "municipal", 'municipal',
                                               ifelse(.N()$tipo[from] == 'reportada' &
                                                      .N()$tipo[to] == "reportada", 'reportada',
                                                      'otra'))))

  gg <- ggraph(graf_1, "fr") +
        ggraph::geom_edge_link(aes(colour = tipo),
                               arrow = arrow(length = unit(0.1, "inches")),
                               edge_width = 0.3,
                               end_cap = circle(1.5, 'mm')) +
        ggraph::geom_node_label(aes(label = name, colour = tipo),
                                repel = TRUE,
                                label.size = 0.10,
                                size = 2) +
        ggraph::geom_node_point(aes(colour = tipo, shape = tipo)) +
        ggraph::theme_graph(base_family = "sans") +
        ggraph::scale_edge_color_manual(values = cbbPalette) +
        ggplot2::scale_colour_manual(values = cbbPalette) +
        ggplot2::scale_fill_manual(values = cbbPalette)
  gg
}

# Function to evaluate a graphical model using the validation deviance score

eval_deviance <- function(data,
                          blacklist,
                          evidence_vars,
                          model_name,
                          k_vec = c(1, 10, 30, 100, 500, 2000)){

  input_data <- data %>%
                dplyr::select(one_of(c(evidence_vars, verificadas)))

  loss_k <- sapply(k_vec, function(k){
    print(k)
    evaluate_p_dag <- bnlearn::bn.cv(method = "hold-out",
                                     data = input_data,
                                     bn = "hc", fit = "bayes",
                                     k = 5, m = floor(0.20 * nrow(data)),
                                     algorithm.args = list(score = "aic",
                                                           k = k,
                                                           blacklist = blacklist,
                                                           restart = 5))

    loss(evaluate_p_dag)
  })

  data.frame(k = k_vec, devianza = loss_k, modelo = model_name)
}

# AIC/BIC scoring for BN models

score_dag <- function(dag, data, model_name){
  data.frame(model_name = model_name,
             aic = bnlearn::score(x = dag,
                                  data = data,
                                  type = 'aic'),
             bic = bnlearn::score(x = dag,
                                  data = data,
                                  type = 'bic'))
}


eval_model <- function(train_data, evidence_vars, model_name, blacklist){

  input_data <- train_data %>%
                dplyr::select(one_of(c(evidence_vars, verificadas)))

  purrr::map_df(k_vec, function(k) eval_deviance(train_data = data.frame(input_data),
                                                 test_data = data.frame(test_data),
                                                 blacklist = blacklist,
                                                 evidence_vars = evidence_vars,
                                                 model_name = model_name))
}


##################### Evaluation by imputation of probabilities ##########################

# Fit a BN with the structure specified by p_dag
fit_bn <- function(data, p_dag){
  fit_p <- bnlearn::bn.fit(p_dag, data = data,
                           method = "bayes", iss = 2)
  as.grain(fit_p)

}

# Predict values for unobserved variables, given some observed evidence
imputar <- function(data, i, fit_grain, evidence_vars, vars_to_predict){

  # Define evidence to input into the model
  observed_nodes <- lapply(as.list(data[i, evidence_vars]),
                       function(x)(as.character(x)))
  fit_evidence <- setEvidence(fit_grain,
                              nodes = evidence_vars,
                              states = observed_nodes)

  # Get predictions
  query_result <- querygrain(fit_evidence, nodes = vars_to_predict) %>%
                  lapply(., function(dist) { dist [['1']]})
  query_result$id <- i

  # Join predictions, evidence and true labels into one resulting dataframe
  predictions_df <- query_result %>%
                    tibble::as_data_frame() %>%
                    tidyr::gather(variable, prob_tiene, -id) %>%
                    dplyr::mutate(variable = gsub('_v$', '', variable))
  observed_df <- data_frame(variable = evidence_vars,
                       reportada = unlist(observed_nodes))
  labels_df <- data[i, vars_to_predict] %>%
               tidyr::gather(variable, verificada) %>%
               dplyr::mutate(variable = gsub('_v$', '', variable))
  result_df <- predictions_df %>%
               dplyr::left_join(observed_df, by = "variable") %>%
               dplyr::left_join(labels_df, by = "variable") %>%
               dplyr::mutate(prob_tiene = round(100 * prob_tiene, 1),
                             diferencia = abs(prob_tiene - 100*as.numeric(reportada)))
  result_df
}


get_roc <- function(prediction_data, c){
    prediction_data %>%
    dplyr::mutate(incorrecta = reportada != verificada) %>%
    dplyr::group_by(id, model) %>%
    dplyr::summarise_at(vars(incorrecta, diferencia), funs(sum)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cutoff = c,
                  positive = incorrecta > 0,
                  selected = diferencia > cutoff,
                  true_positive = selected & positive,
                  false_positive = selected & !positive,
                  true_negative = !selected & !positive,
                  false_negative = !selected & positive)# %>%
     dplyr::group_by(cutoff, model) %>%
     dplyr::summarise(TPR = sum(true_positive)/sum(positive),
                      FPR = sum(false_positive)/sum(!positive),
                      TNR = sum(true_negative)/sum(!positive),
                      FNR = sum(false_negative)/sum(positive)) %>%
     dplyr::ungroup()
}


aprox_auc <- function(x, y){
  x_lead <- dplyr::lead(x)
  y_lead <- dplyr::lead(y)

  delta_x <- x - x_lead
  f_x_avg <- (y + y_lead)/2

  sum(delta_x*f_x_avg, na.rm = T)
}


eval_at_k <- function(train_data,
                      test_data,
                      evidence_vars,
                      blacklist,
                      model_name, k,
                      vars_to_predict = verificadas){

  print(model_name)

  input_data <- train_data %>%
                dplyr::select(one_of(c(evidence_vars, verificadas))) %>%
                data.frame()

  test_data <- data.frame(test_data)

  # Get the best structure for the data and fit the probability distributions accordingly
  p_dag <- bnlearn::hc(x = input_data,
                       score = 'aic',
                       restart = 5,
                       k = k,
                       blacklist = blacklist)
  fit_grain <- fit_bn(data = input_data, p_dag = p_dag)

  # Given that structure, predict values for a sample
  set.seed(140693)
  sample_i <- sample(1:nrow(test_data), size = 500)
  prediction_data <- map_df(sample_i, function(i) imputar(data = test_data,
                                                          i = i,
                                                          fit_grain = fit_grain,
                                                          evidence_vars = evidence_vars,
                                                          vars_to_predict = vars_to_predict))
  # Get TPR and FPR for those predictions and labels
  # roc_data <- map_df(cutoff_vec,
  #                    function(cutoff) get_roc(prediction_data = prediction_data, c = cutoff))

  # Compute AUC score
  # data.frame(auc = aprox_auc(roc_data, n = length(cutoff_vec)),
  #            k = k,
  #            model = model_name)

  prediction_data %>%
    dplyr::mutate(k = k,
                  model = model_name)
}

