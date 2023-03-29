# problem set 1
setwd("c:/Users/franc/Drive/francesco_caporali/university/multivariate_statistical_analysis/problem_sets/problem_set_1")
rm(list = ls())



# exercise 1
# 	state.x77 data, package dataset

st = as.data.frame(state.x77)
st[, 9] = st$Population * 1000 / st$Area
names(st)[c(4, 6, 9)] = c("Life_Exp", "HS_Grad", "Density")
lookup <- c("darkgreen",  "brown", "lightblue",  "magenta", "purple",
			"blue", "red", "lightgreen", "orange", "cyan")


# 1.1
cor_mat = round(cor(st), 2)
library(corrplot)
corrplot(cor_mat, type = "upper", method = "circle", tl.col = "black", addCoef.col = "black")

index_mat = function(x, n) {
	return(c((x - 1) %% n + 1, (x - 1) %/% n + 1))
}
sorted_cor = data.frame(
	index = order(cor_mat, decreasing = T, na.last = NA),
	value = sort(cor_mat, decreasing = T)
   )
n = nrow(cor_mat)
sorted_cor[, 3] = sapply(sorted_cor$index, index_mat, n = n)[1, ]
sorted_cor[, 4] = sapply(sorted_cor$index, index_mat, n = n)[2, ]
names(sorted_cor)[c(3, 4)] = c("index_row", "index_col")
sorted_cor = sorted_cor[, 2:4]
name_fun = function(n, names) {
	return(names[n])
}
sorted_cor[, 4] = sapply(sorted_cor$index_row, name_fun, names = names(st))
sorted_cor[, 5] = sapply(sorted_cor$index_col, name_fun, names = names(st))
sorted_cor = sorted_cor[, c(1, 4, 5)]
names(sorted_cor)[c(2, 3)] = c("name_row", "name_col")
sorted_cor


# 1.2
scale_st = round(scale(st), 3)
quant = c(qnorm(0.99), qnorm(0.9875))
mat_1 = which(abs(scale_st[, ]) > quant[1], arr.ind = T)
mat_2 = which(abs(scale_st[, ]) > quant[2], arr.ind = T)
mat_1
mat_2

n = nrow(st)
out_1 = c(5, 32, 2, 18, 21, 30, 39)
out_2 = 43
out_all = c(out_1, out_2)
# maybe colored matrix


# 1.3
mat_copy = mat_2
n = ncol(st)
par(mfrow = c(3, 3))
for (j in 1:n){
	b = boxplot(st[, j], main = names(st)[j], outpch = 16, outcex = 1.25)
	if (length(b$out > 0)) {
		for (i in match(b$out, st[, j])) {
			if (i %in% mat_copy[, 1] && j == mat_copy[match(i, mat_copy[, 1]), 2]) {
				mat_copy = mat_copy[-1, ]
				text(1, st[i, j], labels = as.character(i), pos = 4, cex = 0.75, offset = 1, col = "red")
				points(1, st[i, j], col = "red", pch = 16, cex = 1.25)
			}
		}
	}
}
mat_2 = mat_2[-match(18, out_all), ]
out_all = out_all[-match(18, out_all)]
col_index = rep("black", nrow(st))
col_index[out_all] = lookup[seq_along(out_all)]

# 1.4
par(mfrow = c(3, 3))
for (j in 1:n){
	x = st[, j]
	qqnorm(x, main = names(st)[j], pch = 16)
	qqline(x, col = "blue", lwd = 2)
	for (i in seq_len(dim(mat_2)[1])) {
		if (j == mat_2[i, 2]) {
			current_x = qnorm(ppoints(st[, j]))[match(mat_2[i, 1], order(st[, j]))]
			current_y = st[mat_2[i, 1], j]
			text(current_x, current_y,
				labels = as.character(mat_2[i, 1]),
				pos = 3, cex = 0.75, offset = 0.3, col = "red")
			points(current_x, current_y, col = "red", pch = 16, cex = 1.25)
		}
	}
}

par(mfrow = c(3, 3))
for (j in 1:n){
	x = st[, j]
	hist(x, probability = T, main = names(st)[j], breaks = 10)
	lines(density(x), col = "blue")
	lines(sort(x), dnorm(sort(x), mean(x), sd(x)), col = "red")
}

x = rep(0, 9)
for (j in 1:n){
	x[j] = shapiro.test(st[, j])$p.value
}
shap_test = data.frame(round(x, 10))
rownames(shap_test) = names(st)
colnames(shap_test) = c("p.value")
shap_test

par(mfrow = c(2, 2))
index_out = unique(mat_2[, 2])
shap_test_out = rep(0, 4)
# population
j = 1
x = st[-c(5, 32), j]
qqnorm(x, main = names(st)[j], pch = 16)
qqline(x, col = "blue", lwd = 2)
shap_test_out[1] = shapiro.test(x)$p.value
# income
j = 2
x = st[-2, j]
qqnorm(x, main = names(st)[j], pch = 16)
qqline(x, col = "blue", lwd = 2)
shap_test_out[2] = shapiro.test(x)$p.value
# area
j = 8
x = st[-c(2, 43), j]
qqnorm(x, main = names(st)[j], pch = 16)
qqline(x, col = "blue", lwd = 2)
shap_test_out[3] = shapiro.test(x)$p.value
# density
j = 9
x = st[-c(21, 30, 39), j]
qqnorm(x, main = names(st)[j], pch = 16)
qqline(x, col = "blue", lwd = 2)
shap_test_out[4] = shapiro.test(x)$p.value
# all: shap_test
shap_test_out = data.frame(round(shap_test_out, 10))
rownames(shap_test_out) = names(st)[index_out]
colnames(shap_test_out) = c("p.value")
shap_test_out

# 1.5
par(mfrow = c(1, 1))
plot(st[, 8], st[, 1], pch = 16, xlab = names(st)[8], ylab = names(st)[1], main = "scatterplot", col = col_index)
for (j in out_all){
	text(st[j, 8], st[j, 1], as.character(j), pos = 4, col = col_index[j])
}

# 1.6
d = mahalanobis(st, center = colMeans(st), cov = var(st))
plot(qchisq(ppoints(d), df = ncol(st)), sort(d), main = "chisquared Q-Q plot of mahalanobis distance",
	xlab = "theoretical quantiles", ylab = "sample quantiles", pch = 16)
abline(0, 1, col = "red")
for (i in out_all){
	current_x = qchisq(ppoints(d)[match(i, order(d))], df = ncol(st))
	current_y = d[i]
	text(current_x, current_y, labels = as.character(i), pos = 3, cex = 0.75, offset = 0.3, col = col_index[i])
	points(current_x, current_y, col = col_index[i], pch = 16, cex = 1.25)
}
multi_out = c(2, 11)

# 1.7
label_out = rep("", nrow(st))
label_out[out_all] = as.character(out_all)
plot(d, pch = 16, xlab = "index", ylab = "squared mahalanobis distance",
	main = "multivariate outliers", col = col_index)
text(seq_len(nrow(st)), d, labels = label_out, pos = 3, cex = 0.75, offset = 0.3, col = col_index)
text(11, d[11], labels = "11", pos = 3, cex = 0.75, offset = 0.3)
abline(h = qchisq(0.95, df = ncol(st)), lty = 2, col = "red")
alpha_data = (nrow(st) - 0.5) / nrow(st)
abline(h = qchisq(alpha_data, df = ncol(st)), lty = 2, col = "blue")
legend(x = "topright", legend = c("0.95", round(alpha_data, 3)),
	col = c("red", "blue"), lty = 2, title = "chi-squared quantile")



# exercise 2

# 2.5
c = sqrt(qchisq(0.95, df = 2))

rho = 0.2
mu_y = 1 / 5 * c(1, 9)
sigma_y = 6 / 25 * matrix(c(4, 1, 1, 4), nrow = 2)
eig = eigen(sigma_y, symmetric = T)

library(ellipse)
plot(ellipse(x = sigma_y, centre = mu_y, level = 0.95), type = "l", lwd = 1.5,
	xlab = expression("y"[1]), ylab = expression("y"[2]),
	main = expression(paste("contour plot of the density of Y (", rho, " = 0.2)")),
	cex.main = 1,
	asp = 1, xlim = c(-4, 4), ylim = c(-2, 6))
b = -eig$vectors[1, 2] / eig$vectors[2, 2]
a = -b * mu_y[1] + mu_y[2]
abline(a, b, lwd = 1.5, lty = 2)
d = -eig$vectors[1, 1] / eig$vectors[2, 1]
c = -d * mu_y[1] + mu_y[2]
abline(c, d, lwd = 1.5, lty = 2)
points(mu_y[1], mu_y[2], col = "red", pch = 16)



# exercise 3
# 	nutritional.txt data

nutritional = read.table("data/nutritional.txt")
head(nutritional)

# 3.1
nt = nutritional[, -6] / nutritional[, 6]
head(round(nt, 3))

nt = scale(nt)
nt_pca = prcomp(nt)
nt_pca

# 3.2
summary(nt_pca)
screeplot(nt_pca, type = "l", main = "screeplot", pch = 16)
abline(v = 2, col = "darkgoldenrod2", lty = 2)
abline(v = 5, col = "lightblue", lty = 2)

library(ggplot2)
library(factoextra)
fviz_eig(nt_pca, addlabels = T)