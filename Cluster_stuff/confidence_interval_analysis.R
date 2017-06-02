
param_types <- as.data.frame(unique(gsub("\\[.*", "", rownames(params_summary))))
colnames(param_types) <- "param"
param_types$dim <- c(rep("vec", 3), rep("scalar", 2), 
                     rep("vec", 1), rep("scalar", 2), 
                     rep("vec", 1), rep("scalar", 1))

for(i in 1:nrow(param_types)){
  if(param_types[i, "dim"] == "vec"){
    assign(paste0(param_types[i, "param"], "_params_summary"), params_summary[
      grep(paste0("^", param_types[i, "param"], "\\["), rownames(params_summary)),])
  } else if(param_types[i, "dim"] == "scalar"){
    assign(paste0(param_types[i, "param"], "_params_summary"), params_summary[
      grep(paste0("^", param_types[i, "param"]), rownames(params_summary)),])
  }
}

#item_params: a, b, D
a_param_CIs <- CI_retrieval(true_item_params[, "a_param"], a_params_summary)
a_param_CI_prop <- sum(a_param_CIs)/n_items

b_param_CIs <- CI_retrieval(true_item_params[, "b_param"], b_params_summary)
b_param_CI_prop <- sum(b_param_CIs)/n_items

D_param_CIs <- CI_retrieval(true_item_params[, "dif_param"], D_params_summary)
D_param_CI_prop <- sum(D_param_CIs)/n_items

#ability_params: theta
theta_param_CIs <- CI_retrieval(true_ability[, "theta"], theta_params_summary)
theta_param_CI_prop <- sum(theta_param_CIs)/n_people

#scalar params: beta0, beta1, R2 
beta0_CIs <- CI_retrieval(beta0_true, t(beta0_params_summary))

beta1_CIs <- CI_retrieval(beta1_true, t(beta1_params_summary))

R2_CIs <- CI_retrieval(R2_true, t(R2_params_summary))

