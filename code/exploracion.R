# Cargamos todas las librerias necesarias, generamos la conexion con la base de datos
library(igraph)
library(DBI)
library(dbrsocial)
library(dotenv)
library(readr)
library(tidyverse)
library(ggrepel)
library(ggraph)
library(tidygraph)
library(bnlearn)
library(gRain)

dotenv::load_dot_env()
source('/graph_models_funs.R')

con <- prev_connect()

# Definimos las variables de interes
reportadas <- c('estufa', 'refrigerador', 'horno', 'lavadora',
                'television', 'telefono', 'celular', 'computadora',
                'clima', 'vehiculo', 'internet', 'servicio_agua',
                'escusado_exclusivo', 'tipo_escusado', 'servicio_drenaje',
                'material_pisos', 'tinaco')
verificadas <- glue::glue('{reportadas}_v') %>% as.character()
ids <- c('ent_fed', 'munici', 'hogar_id')

# Filtrar variables relevantes para usar menos RAM
familias <- dbrsocial::large_table(con, clean, encaseh_familias) %>%
  dplyr::filter(data_date == '2016-a') %>%
  dplyr::select(one_of(c(ids, reportadas, verificadas))) %>%
  dplyr::collect() %>%
  dplyr::mutate(cve_ent = stringr::str_pad(ent_fed, width = 2,
                                           side = "left", pad = '0'),
                cve_mun = stringr::str_pad(munici, width = 3,
                                           side = "left", pad = '0'),
                cve_muni = paste0(cve_ent, cve_mun))

municipales_cut <- c('analfabetismo', 'sin_primaria', 'sin_drenaje_exc',
                     'sin_electricidad', 'sin_agua_entubada', 'hacinamiento',
                     'piso_tierra', 'loc_menor_5000', 'menos_2sm')
municipales <- c(municipales_cut, 'grado_marginacion')

municipios <- dbrsocial::large_table(con, raw, conapo_marginacion) %>%
  dplyr::filter(anio == 2015) %>%
  dplyr::collect() %>%
  dplyr::mutate(cve_muni = stringr::str_pad(cve_mun, width = 5,
                                            side = 'left', pad = '0')) %>%
  dplyr::mutate_at(vars(one_of(municipales_cut)),
                   .funs = function(x) cut(as.numeric(x),
                                           breaks = c(0, 10, 40, 100),
                                           include.lowest = TRUE)) %>%
  dplyr::select(cve_muni, one_of(municipales))

familias <- dplyr::left_join(familias, municipios)
rm(municipios)

discon_db(con)
dim(familias)

# Creamos el dataset de trabajo que vamos a utilizar, dividimos en entrenamiento y prueba
familias_base <- familias %>%
  dplyr::mutate_at(vars(starts_with('servicio_agua')), function(x) if_else(x == 7, 1, 0)) %>%
  dplyr::mutate_at(vars(starts_with('servicio_drenaje')), function(x) if_else(x == 5, 1, 0)) %>%
  dplyr::mutate_at(vars(starts_with('material_pisos')), function(x) if_else(x == 2, 1, 0)) %>%
  dplyr::mutate_at(vars(starts_with('tipo_escusado')), function(x) if_else(x == 1, 1, 0)) %>%
  tidyr::replace_na(list(escusado_exclusivo = 0, escusado_exclusivo_v = 0)) %>%
  dplyr::select(hogar_id, one_of(c(reportadas, verificadas, municipales)))

set.seed(140693)
familias_base <- familias_base %>%
                   dplyr::sample_n(400000)

familias_entrena <- familias_base %>%
                      dplyr::mutate(id = row_number()) %>%
                      dplyr::sample_frac(size = 0.7)

familias_valida <- familias_base %>%
                     dplyr::mutate(id = row_number()) %>%
                     dplyr::filter(!(id %in% familias_entrena$id)) %>%
                     dplyr::select(one_of(c(reportadas, verificadas, municipales))) %>%
                     dplyr::mutate_all(as.factor)

familias_entrena <- familias_entrena %>%
                      dplyr::select(one_of(c(reportadas, verificadas, municipales))) %>%
                      dplyr::mutate_all(as.factor)


#### Modelo sin restricciones
p_dag <- bnlearn::hc(x = data.frame(familias_entrena %>% select(-one_of(municipales))),
                     score = 'aic', k = 100,
                     restart = 20, perturb = 5)

graficar_red(p_dag)
ggsave('p_dag.png')

### Modelo base
blacklist_1 <- expand.grid(from = reportadas, to = verificadas)

p_dag_1 <- bnlearn::hc(x = data.frame(familias_entrena %>% select(-one_of(municipales))),
                       score = 'aic', k = 100,
                       restart = 20, perturb = 5,
                       blacklist = blacklist_1)

graficar_red(p_dag_1)
ggsave('p_dag_1.png')

### Modelo base con variables municipales y GM
blacklist_mun1 <- expand.grid(from = c(reportadas, verificadas),
                              to = municipales) %>%
                    dplyr::bind_rows(blacklist_1) %>%
                    dplyr::mutate_all(as.factor)

p_dag_mun1 <- bnlearn::hc(x = data.frame(familias_entrena),
                          score = 'aic', k = 100,
                          restart = 20, perturb = 5,
                          blacklist = blacklist_mun1)


graficar_red(p_dag_mun1)
ggsave('p_dag_mun1.png')

### Modelo base con GM
blacklist_mun2 <- blacklist_mun1 %>%
                    dplyr::filter(!(from %in% municipales_cut),
                                  !(to %in% municipales_cut))

p_dag_mun2 <- bnlearn::hc(x = data.frame(familias_entrena %>%
                                         dplyr::select(-one_of(municipales_cut))),
                          score = 'aic', k = 100,
                          restart = 20, perturb = 5,
                          blacklist = blacklist_mun2)


graficar_red(p_dag_mun2)
ggsave('p_dag_mun2.png')

### Modelo base + variables municipales
blacklist_mun3 <- blacklist_mun1 %>%
                    dplyr::filter(from != 'grado_marginacion',
                                  to != 'grado_marginacion')

p_dag_mun3 <- bnlearn::hc(x = data.frame(familias_entrena %>%
                                         dplyr::select(-grado_marginacion)),
                          score = 'aic', k = 100,
                          restart = 20, perturb = 5,
                          blacklist = blacklist_mun3)


graficar_red(p_dag_mun3)
ggsave('p_dag_mun3.png')

### Modelo base + 3 variables municipales
vars_mun4 <- c(reportadas, verificadas, c("analfabetismo", "sin_drenaje_exc", "hacinamiento"))

blacklist_mun4 <- blacklist_mun1 %>%
                   dplyr::filter(from %in% vars_mun4,
                                 to %in% vars_mun4)


p_dag_mun4 <- bnlearn::hc(x = data.frame(familias_entrena %>%
                                         dplyr::select(one_of(vars_mun4))),
                          score = 'aic', k = 100,
                          restart = 20, perturb = 5,
                          blacklist = blacklist_mun4)


graficar_red(p_dag_mun4)
ggsave('p_dag_mun4.png')

### Evaluamos la devianza de validacion
model_list = list(modelo_base = list(evidence_vars = reportadas,
#                                     blacklist = NULL,
                                     name = 'Modelo sin restricciones'),
                  modelo_r1 = list(evidence_vars = reportadas,
#                                   blacklist = blacklist_1,
                                   name = 'Modelo base'),
                  modelo_mun1 = list(evidence_vars = c(reportadas, municipales),
#                                     blacklist = blacklist_mun1,
                                     name = 'Modelo base\n+ variables municipales + GM'),
                  modelo_mun2 = list(evidence_vars = c(reportadas, 'grado_marginacion'),
#                                     blacklist = blacklist_mun2,
                                     name = 'Modelo base + GM'),
                  modelo_mun3 = list(evidence_vars = c(reportadas, municipales_cut),
#                                     blacklist = blacklist_mun3,
                                     name = 'Modelo base\n+ variables municipales'),
                  modelo_mun4 = list(evidence_vars = c(reportadas, c('analfabetismo', 'sin_drenaje_exc', 'hacinamiento')),
#                                     blacklist = blacklist_mun4,
                                     name = 'Modelo base\n+ 3 variables municipales'))
set.seed(140693)

results_deviance <- purrr::map_df(names(model_list),
                     function(model_name) eval_deviance(data = data.frame(train_data),
                                                        blacklist = model_list[[model_name]]$blacklist,
                                                        evidence_vars = model_list[[model_name]]$evidence_vars,
                                                        model_name = model_list[[model_name]]$name))

ggplot(results_deviance, aes(x = k, y = devianza)) +
  ggplot2::geom_point(color = 'maroon') +
  ggplot2::geom_line(color = 'maroon') +
  ggplot2::scale_x_log10(breaks = c(1, 10, 30, 100, 500, 2000)) +
  ggplot2::facet_wrap(vars(modelo),
                      scales = 'free_y',
                      ncol = 3) +
  ggplot2::labs(x = 'Parametro de regularizacion (k)',
                y = 'Devianza de validacion') +
  ggplot2::theme_minimal()

ggsave('devianza_validacion.png')


### Generamos las predicciones de nuestro clasificador a partir de la suma de probabilidades
results_prediction <- purrr::map_df(names(model_list),
                             function(model_name) eval_at_k(train_data = familias_entrena,
                                                            test_data = familias_valida,
                                                            evidence_vars = model_list[[model_name]]$evidence_vars,
                                                            blacklist = model_list[[model_name]]$blacklist,
                                                            model_name = model_list[[model_name]]$name,
                                                            k = 10))


cutoff_vec <- seq(0, 200, length.out = 200)
results_selections <- map_df(cutoff_vec, function(c) get_roc(results_prediction, c))
ggplot(results_selections, aes(diferencia,
                               color = positive,
                               fill = positive)) +
geom_density(alpha=0.2) +
scale_color_manual(values=c('darkred', 'navyblue')) +
scale_fill_manual(values=c('darkred', 'navyblue'),
                  breaks=c(FALSE, TRUE),
                  labels=c('Ningun incorrecto', 'Algun incorrecto'),
                  name='Reportaje incorrecto') +
guides(colour='none') +
labs(x = 'Suma de diferencias en probabilidad', y = '') +
facet_wrap(.~model) +
theme_minimal()

ggsave('suma_probabilidades.png',
       height = 6, width = 9)


### Calculamos los valores para generar la curva ROC
results_roc  <- results_selections %>%
                dplyr::group_by(cutoff, model) %>%
                dplyr::summarise(TPR = sum(true_positive)/sum(positive),
                                 FPR = sum(false_positive)/sum(!positive),
                                 TNR = sum(true_negative)/sum(!positive),
                                 FNR = sum(false_negative)/sum(positive)) %>%
                dplyr::ungroup()

ggplot(results_roc, aes(x = 1-TNR, y = TPR,
                        group = model, colour = model)) +
  geom_line() +
  geom_abline(slope = 1, color = 'red') +
  ylab("Sensitivity") +
  xlab("1 - Specificity") +
  scale_colour_brewer(name = 'Modelo', palette = 'Dark2') +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal()
  ggsave('results_roc.png', height = 7, width = 9)
  
  
### Utilizamos la Regla del Trapecio para calcular el AUC
results_auc <- results_roc %>%
               dplyr::group_by(model) %>%
               dplyr::summarise(auc = aprox_auc(1-TNR, TPR))

results_auc %>%
  ggplot(aes(model, auc, fill = model)) +
  geom_bar(stat='identity') +
  scale_fill_brewer(name = 'Modelo', palette = 'Dark2') +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0.6, 0.7),
                     oob = rescale_none) +
  labs(title = "Area bajo la curva ROC",
       x = '',
       y = '') +
  theme_minimal()

  ggsave('results_auc.png', height = 6, width = 9)