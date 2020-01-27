# data("diabetes",package="lars")
# x = diabetes$x
# y = diabetes$y
# df = data.frame(scale(cbind(y,x)))
# lm1 = lm(y ~ ., data = df)
# 
# sstab = function(mf, B = 100){
#   full_coeff = coefficients(mf)
#   kf = length(full_coeff)
#   coef.res = matrix(ncol = kf, nrow = B)
#   colnames(coef.res) = names(full_coeff)
#   n.obs = length(resid(mf))
#   for(i in 1:B){
#     mod = stats::lm(stats::formula(mf),
#                     data = model.frame(mf),
#                     weights = stats::rexp(n = n.obs, rate = 1))
#     coef.res[i,] = coefficients(mod)
#   }
#   return(coef.res)
# }
# 
# sj = sstab(lm1)
# sj_ranks = apply(sj, 1, rank)
# sj_rank_mean = sort(apply(sj_ranks, 1, mean), decreasing = TRUE)
# sj_rank_sd = sort(apply(sj_ranks, 1, sd), decreasing = TRUE)
