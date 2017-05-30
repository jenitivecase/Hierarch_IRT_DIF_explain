
param_types <- unique(gsub("\\[.*", "", rownames(params_summary)))

param_summary_types <- vector("list", length(param_types))

for(i in 1:length(param_types)){
  assign(paste0(param_types[i], "_params_summary"), params_summary[
    grep(paste0("^", param_types[i], "\\["), rownames(params_summary)),])
  
}

#item_params: a, b, D


#ability_params: theta

#scalar params: beta0, beta1, mu, sigma2, R2, foc_mean