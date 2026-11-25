library(dplyr)
library(tidyr)
library(purrr)
library(broom)

get_density_pctl <- function(df) {
  m <- lm(LOG_NPO ~ LOG_POP, data = df)
  logged_residuals <- residuals(m)
  density_pctl <- exp(logged_residuals) - 1
  density_pctl[density_pctl > 2] <- 2
  return(list(model = m, density_pctl = density_pctl))
}

df_density <- dd %>%
  group_by(SUB, YEAR) %>%
  nest() %>%
  mutate(
    model_results = map(data, get_density_pctl),
    model = map(model_results, "model"),                 # extract model objects
    data = map2(data, model_results, \(d, res) mutate(d, density_pctl = res$density_pctl))
  ) %>%
  dplyr::select(-model_results) %>%
  unnest(data)

df_models <- df_density %>% distinct(SUB, YEAR, model)

coef_summary <- df_models %>%
  mutate(coefs = map(model, broom::tidy)) %>%
  tidyr::unnest(coefs) %>%
  dplyr::filter(term == "LOG_POP") %>%
  dplyr::select(SUB, YEAR, estimate, std.error, statistic, p.value)

b1_summary1 <- coef_summary %>%
  group_by(SUB) %>%
  summarize(
    n_models = n(),
    mean_B1  = mean(estimate, na.rm = TRUE),
    sd_B1    = sd(estimate, na.rm = TRUE),
    min_B1   = min(estimate, na.rm = TRUE),
    max_B1   = max(estimate, na.rm = TRUE),
    q25_B1   = quantile(estimate, 0.25, na.rm = TRUE),
    q50_B1   = quantile(estimate, 0.50, na.rm = TRUE),
    q75_B1   = quantile(estimate, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

b1_summary1 |> knitr::kable()

|SUB | n_models|   mean_B1|     sd_B1|    min_B1|    max_B1|    q25_B1|    q50_B1|    q75_B1|
|:---|--------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|
|ALL |       13| 0.9617878| 0.0046071| 0.9476683| 0.9655129| 0.9611739| 0.9629488| 0.9642741|
|ART |       13| 0.9124142| 0.0192609| 0.8509582| 0.9231003| 0.9117736| 0.9191799| 0.9212977|
|EDU |       13| 1.0110945| 0.0208103| 0.9450888| 1.0235722| 1.0090899| 1.0193965| 1.0209856|
|ENV |       13| 0.8367396| 0.0129167| 0.8077940| 0.8476096| 0.8300215| 0.8434591| 0.8464148|
|HEL |       13| 0.9377690| 0.0135804| 0.8971389| 0.9518446| 0.9368449| 0.9398334| 0.9454866|
|HMS |       13| 0.9394413| 0.0055264| 0.9241112| 0.9443442| 0.9369554| 0.9412261| 0.9428114|
|HOS |       13| 0.5129285| 0.0456203| 0.3762924| 0.5515040| 0.5009311| 0.5241512| 0.5409972|
|IFA |       13| 0.8054922| 0.0160780| 0.7644500| 0.8182152| 0.8071856| 0.8102463| 0.8159096|
|MMB |       13| 0.7348838| 0.0237454| 0.6635400| 0.7522837| 0.7351219| 0.7429421| 0.7473329|
|PSB |       13| 0.8717819| 0.0054167| 0.8544586| 0.8755941| 0.8721590| 0.8728841| 0.8745523|
|REL |       13| 0.9232199| 0.0103462| 0.8908250| 0.9308112| 0.9234736| 0.9256734| 0.9288411|
|UNI |       13| 0.5091408| 0.0467426| 0.3590547| 0.5392616| 0.5063090| 0.5183269| 0.5327747|
|UNU |       13| 0.6272686| 0.0608197| 0.5303289| 0.6991259| 0.5822695| 0.6458317| 0.6808332|


# 501C3 PUBLIC CHARITIES ONLY

|SUB | n_models|   mean_B1|     sd_B1|    min_B1|    max_B1|    q25_B1|    q50_B1|    q75_B1|
|:---|--------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|
|ALL |       13| 0.9805578| 0.0054779| 0.9644244| 0.9854654| 0.9795004| 0.9820824| 0.9838954|
|ART |       13| 0.9024118| 0.0186146| 0.8424290| 0.9134956| 0.9021252| 0.9091517| 0.9107953|
|EDU |       13| 0.9997257| 0.0205681| 0.9340723| 1.0110125| 0.9982836| 1.0068395| 1.0101100|
|ENV |       13| 0.8171693| 0.0147641| 0.7914829| 0.8311109| 0.8068253| 0.8248643| 0.8290109|
|HEL |       13| 0.9195181| 0.0137564| 0.8767630| 0.9327917| 0.9197506| 0.9229235| 0.9259599|
|HMS |       13| 0.9355916| 0.0060860| 0.9165648| 0.9397763| 0.9349775| 0.9370975| 0.9389363|
|HOS |       13| 0.4743612| 0.0467459| 0.3364683| 0.5154632| 0.4613116| 0.4840831| 0.5045213|
|IFA |       13| 0.7966251| 0.0169886| 0.7564080| 0.8105376| 0.7967155| 0.8011957| 0.8082785|
|MMB |       13| 0.3247087| 0.0282473| 0.2619231| 0.3501026| 0.3010458| 0.3355315| 0.3469580|
|PSB |       13| 0.9089558| 0.0115311| 0.8715037| 0.9165129| 0.9101693| 0.9116493| 0.9137850|
|REL |       13| 0.9213375| 0.0104829| 0.8888015| 0.9296800| 0.9210664| 0.9241917| 0.9269522|
|UNI |       13| 0.4890668| 0.0482004| 0.3352777| 0.5213886| 0.4851515| 0.5003600| 0.5134959|
|UNU |       13| 0.6003829| 0.0555144| 0.5114716| 0.6684859| 0.5619100| 0.6149855| 0.6439963|





### ROBUST IMPLEMENTATION 

library( MASS )

get_density_pctl_robust <- function(df) {
  # Fit a robust regression (M-estimator)
  m <- tryCatch(
    rlm(LOG_NPO ~ LOG_POP, data = df),
    error = function(e) NULL
  )
  if (is.null(m)) return(rep(NA_real_, nrow(df)))
  # Compute residuals
  logged_residuals <- residuals(m)
  density_pctl <- exp(logged_residuals) - 1
  density_pctl[density_pctl > 2] <- 2  # cap extreme residuals if desired
  list(model = m, density_pctl = density_pctl)
}


df_density <- dd %>%
  group_by(SUB, YEAR) %>%
  nest() %>%
  mutate(
    results = map(data, get_density_pctl_robust),
    model   = map(results, "model"),
    data    = map2(data, results, \(d, r) mutate(d, density_pctl = r$density_pctl))
  ) %>%
  dplyr::select(-results) %>%
  unnest(data)

df_models <- df_density %>% distinct(SUB, YEAR, model)

coef_summary <- df_models %>%
  mutate(coefs = map(model, broom::tidy)) %>%
  tidyr::unnest(coefs) %>%
  dplyr::filter(term == "LOG_POP") %>%
  dplyr::select(SUB, YEAR, estimate, std.error, statistic)

b1_summary2 <- coef_summary %>%
  group_by(SUB) %>%
  summarize(
    n_models = n(),
    mean_B1  = mean(estimate, na.rm = TRUE),
    sd_B1    = sd(estimate, na.rm = TRUE),
    min_B1   = min(estimate, na.rm = TRUE),
    max_B1   = max(estimate, na.rm = TRUE),
    q25_B1   = quantile(estimate, 0.25, na.rm = TRUE),
    q50_B1   = quantile(estimate, 0.50, na.rm = TRUE),
    q75_B1   = quantile(estimate, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

b1_summary2 |> dplyr::select(SUB, n_models, mean_B1) |> knitr::kable()


|SUB | n_models|   mean_B1|     sd_B1|    min_B1|    max_B1|    q25_B1|    q50_B1|    q75_B1|
|:---|--------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|
|ALL |       13| 0.9630739| 0.0057759| 0.9452212| 0.9670109| 0.9622859| 0.9647559| 0.9664683|
|ART |       13| 0.9215155| 0.0197169| 0.8603146| 0.9371184| 0.9192873| 0.9271602| 0.9315216|
|EDU |       13| 1.0201489| 0.0206792| 0.9555360| 1.0326131| 1.0166909| 1.0279902| 1.0313334|
|ENV |       13| 0.8578233| 0.0136009| 0.8221678| 0.8696557| 0.8540390| 0.8636727| 0.8668003|
|HEL |       13| 0.9419419| 0.0130346| 0.9038395| 0.9553011| 0.9407618| 0.9438094| 0.9492575|
|HMS |       13| 0.9403349| 0.0070316| 0.9214143| 0.9468001| 0.9378938| 0.9421168| 0.9451944|
|HOS |       13| 0.4973862| 0.0520818| 0.3430324| 0.5420464| 0.4830016| 0.5107793| 0.5310474|
|IFA |       13| 0.7959764| 0.0181511| 0.7472460| 0.8102701| 0.7968227| 0.8016306| 0.8058726|
|MMB |       13| 0.7445164| 0.0263840| 0.6659402| 0.7651354| 0.7441866| 0.7525461| 0.7585143|
|PSB |       13| 0.8764767| 0.0062507| 0.8561103| 0.8809783| 0.8771222| 0.8778926| 0.8789487|
|REL |       13| 0.9343590| 0.0109827| 0.8999460| 0.9418826| 0.9347381| 0.9367482| 0.9402910|
|UNI |       13| 0.4955271| 0.0528283| 0.3268079| 0.5294738| 0.4913643| 0.5060989| 0.5235781|
|UNU |       13| 0.6080981| 0.0733346| 0.4924569| 0.6963826| 0.5541299| 0.6318088| 0.6687674|

# 501C3 PUBLIC CHARITIES ONLY 

|SUB | n_models|   mean_B1|     sd_B1|    min_B1|    max_B1|    q25_B1|    q50_B1|    q75_B1|
|:---|--------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|
|ALL |       13| 0.9824976| 0.0074398| 0.9609252| 0.9879462| 0.9803097| 0.9851410| 0.9869991|
|ART |       13| 0.9116427| 0.0192714| 0.8508903| 0.9251630| 0.9094819| 0.9181011| 0.9208286|
|EDU |       13| 1.0093279| 0.0202071| 0.9457033| 1.0213543| 1.0060961| 1.0174889| 1.0193310|
|ENV |       13| 0.8390011| 0.0149055| 0.8059947| 0.8519077| 0.8317859| 0.8473985| 0.8496125|
|HEL |       13| 0.9243291| 0.0133965| 0.8836462| 0.9373469| 0.9237589| 0.9271572| 0.9308250|
|HMS |       13| 0.9352559| 0.0063798| 0.9154382| 0.9393900| 0.9347072| 0.9370974| 0.9384996|
|HOS |       13| 0.4551393| 0.0540641| 0.2960549| 0.5017377| 0.4395988| 0.4673795| 0.4923170|
|IFA |       13| 0.7844653| 0.0193659| 0.7375052| 0.8016433| 0.7849661| 0.7908963| 0.7963769|
|MMB |       13| 0.2666625| 0.0299772| 0.2110062| 0.3074364| 0.2435932| 0.2711613| 0.2875898|
|PSB |       13| 0.9157868| 0.0123922| 0.8776052| 0.9256807| 0.9145960| 0.9170659| 0.9235957|
|REL |       13| 0.9324317| 0.0111023| 0.8976911| 0.9403716| 0.9327900| 0.9351281| 0.9383044|
|UNI |       13| 0.4703192| 0.0536385| 0.3000646| 0.5063279| 0.4659041| 0.4813201| 0.4987910|
|UNU |       13| 0.5749161| 0.0674847| 0.4692949| 0.6589305| 0.5289519| 0.5927618| 0.6267080|



|SUB | n_models|   mean_B1|    
|:---|--------:|---------:|
|ALL |       13| 0.9630739| 
|ART |       13| 0.9215155| 
|EDU |       13| 1.0201489| 
|ENV |       13| 0.8578233| 
|HEL |       13| 0.9419419| 
|HMS |       13| 0.9403349| 
|HOS |       13| 0.4973862| 
|IFA |       13| 0.7959764| 
|MMB |       13| 0.7445164| 
|PSB |       13| 0.8764767| 
|REL |       13| 0.9343590| 
|UNI |       13| 0.4955271| 
|UNU |       13| 0.6080981| 




### RESOURCE VERSION: TOTAL EXPENSES

get_density_pctl_robust <- function(df) {
  # Fit a robust regression (M-estimator)
  m <- tryCatch(
    rlm(LOG_RES ~ LOG_POP, data = df),
    error = function(e) NULL
  )
  if (is.null(m)) return(rep(NA_real_, nrow(df)))
  # Compute residuals
  logged_residuals <- residuals(m)
  density_pctl <- exp(logged_residuals) - 1
  density_pctl[density_pctl > 2] <- 2  # cap extreme residuals if desired
  list(model = m, density_pctl = density_pctl)
}


df_density <- dd %>%
  group_by(SUB, YEAR) %>%
  nest() %>%
  mutate(
    results = map(data, get_density_pctl_robust),
    model   = map(results, "model"),
    data    = map2(data, results, \(d, r) mutate(d, density_pctl = r$density_pctl))
  ) %>%
  dplyr::select(-results) %>%
  unnest(data)

df_models <- df_density %>% distinct(SUB, YEAR, model)

coef_summary <- df_models %>%
  mutate(coefs = map(model, broom::tidy)) %>%
  tidyr::unnest(coefs) %>%
  dplyr::filter(term == "LOG_POP") %>%
  dplyr::select(SUB, YEAR, estimate, std.error, statistic)

b1_summary3 <- coef_summary %>%
  group_by(SUB) %>%
  summarize(
    n_models = n(),
    mean_B1  = mean(estimate, na.rm = TRUE),
    sd_B1    = sd(estimate, na.rm = TRUE),
    min_B1   = min(estimate, na.rm = TRUE),
    max_B1   = max(estimate, na.rm = TRUE),
    q25_B1   = quantile(estimate, 0.25, na.rm = TRUE),
    q50_B1   = quantile(estimate, 0.50, na.rm = TRUE),
    q75_B1   = quantile(estimate, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

b1_summary3 |> knitr::kable()


|SUB | n_models|  mean_B1|     sd_B1|    min_B1|   max_B1|   q25_B1|   q50_B1|   q75_B1|
|:---|--------:|--------:|---------:|---------:|--------:|--------:|--------:|--------:|
|ALL |       13| 1.351345| 0.0094701| 1.3348581| 1.362216| 1.341929| 1.354920| 1.358752|
|ART |       13| 1.436436| 0.0566707| 1.2565239| 1.485183| 1.437698| 1.448553| 1.461182|
|EDU |       13| 1.475421| 0.0499128| 1.3118050| 1.503625| 1.481805| 1.488574| 1.491271|
|ENV |       13| 1.514494| 0.0580218| 1.3290174| 1.560224| 1.511952| 1.529670| 1.538542|
|HEL |       13| 1.566599| 0.0309290| 1.5308803| 1.659663| 1.551728| 1.565222| 1.572462|
|HMS |       13| 1.273061| 0.0068565| 1.2545780| 1.283297| 1.271650| 1.273180| 1.277658|
|HOS |       13| 1.498395| 0.1003443| 1.2017457| 1.577204| 1.461443| 1.539813| 1.562115|
|IFA |       13| 1.526291| 0.0478686| 1.3935733| 1.574959| 1.506725| 1.534207| 1.554829|
|MMB |       13| 1.052430| 0.0898868| 0.8068985| 1.140432| 1.041848| 1.070535| 1.109009|
|PSB |       13| 1.421080| 0.0266550| 1.3436871| 1.455297| 1.416119| 1.425931| 1.431396|
|REL |       13| 1.399814| 0.0341692| 1.2947520| 1.426984| 1.395633| 1.409349| 1.415641|
|UNI |       13| 1.183336| 0.1292198| 0.7603797| 1.259619| 1.202685| 1.215237| 1.228124|
|UNU |       13| 1.257962| 0.0890056| 1.1152563| 1.372993| 1.179633| 1.276458| 1.333758|


b1_summary3 |> dplyr::select(SUB, n_models, mean_B1) |> knitr::kable()


|SUB | n_models|  mean_B1|
|:---|--------:|--------:|
|ALL |       13| 1.351345|
|ART |       13| 1.436436|
|EDU |       13| 1.475421|
|ENV |       13| 1.514494|
|HEL |       13| 1.566599|
|HMS |       13| 1.273061|
|HOS |       13| 1.498395|
|IFA |       13| 1.526291|
|MMB |       13| 1.052430|
|PSB |       13| 1.421080|
|REL |       13| 1.399814|
|UNI |       13| 1.183336|
|UNU |       13| 1.257962|



