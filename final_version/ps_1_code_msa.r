# problem set 1
#setwd("c:/Users/franc/Drive/francesco_caporali/university/multivariate_statistical_analysis/problem_sets/problem_set_1")
setwd("c:/Users/utente/Dropbox/PC/Documents/GitHub/msa_problem_sets/malgieri")
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

# not to insert
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
x = t(x)
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
col_index = rep("black", nrow(st))
col_index[out_all] = lookup[seq_along(out_all)]
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
nutritional_start = nutritional


# 3.1
nutritional = nutritional[, -6] / nutritional[, 6]
head(round(nutritional, 3))

nt = scale(nutritional)
nt_pca = prcomp(nt)
nt_pca$rotation


# 3.2
summary(nt_pca)
nt_sum = as.data.frame(summary(nt_pca)$importance)[-1, ]
nt_sum

par(mfrow = c(1, 2), cex.axis = 0.75, cex.lab = 0.75, cex.names = 0.75)
x = as.matrix(nt_sum[1, ])
colnames(x) = 1:6
bp = barplot(x, ylim = c(0, 0.55),
	xlab = "Dimensions", ylab = "Percentage of explained variances",
	col = "lightblue")
lines(bp, as.matrix(nt_sum[1, ]), type = "b", pch = 16, lwd = 1.5, cex = 0.75)
text(bp, as.matrix(nt_sum[1, ]),
	labels = paste0(round(as.matrix(nt_sum[1, ]) * 100, 1), "%"), pos = 3, cex = 0.75, offset = 0.5)
plot(cumsum(nt_pca$sdev^2) / sum(nt_pca$sdev^2), type = "b",
	xlab = "Dimensions", ylab = "Cumulative Proportion", pch = 16, cex = 0.75)
abline(h = 0.8, col = "blue", lty = 2)

screeplot(nt_pca, type = "l", main = "screeplot", pch = 16)
abline(h = 1, col = "red", lty = 2)


# 3.3
corrplot(t(round(nt_pca$rotation[, 1:2], 2)), method = "circle", tl.col = "black", addCoef.col = "black")

par(mfrow = c(1, 1))
plot(nt_pca$x[, 1], nt_pca$x[, 2], xlim = c(-9, 9), ylim = c(-11, 11), pch = 16, cex = 0.75,
	xlab = "PC1", ylab = "PC2")
abline(h = 0, col = "black", lty = 2)
abline(v = 0, col = "black", lty = 2)

# 3.4
par(mfrow = c(1, 3))
for (j in 1:3) {
	if (j == 1)
		boxplot(nt_pca$x[, j], main = paste0("PC", j), outpch = 16, outcex = 1.25, ylim = c(-8.5, 1.5))
	else
		boxplot(nt_pca$x[, j], main = paste0("PC", j), outpch = 16, outcex = 1.25)
	if (j == 1) {
		points(rep(1, 3), nt_pca$x[c(32, 286, 411), j], col = "red", cex = 1.25, pch = 16)
		text(1, nt_pca$x[c(32), j], label = c("32-33"), pos = 4, cex = 1, offset = 1, col = "red")
		text(1, nt_pca$x[c(286), j], label = c("286-287"), pos = 2, cex = 1, offset = 1, col = "red")
		text(1, nt_pca$x[c(411), j], label = c("411-412"), pos = 1, cex = 1, offset = 1, col = "red")
	}
	if (j == 2) {
		points(rep(1, 2), nt_pca$x[c(49, 866), j], col = "red", cex = 1.25, pch = 16)
		text(rep(1, 2), nt_pca$x[c(49, 866), j], label = c("49", "866"), pos = 4, cex = 1, offset = 1, col = "red")
	}
	if (j == 3) {
		points(rep(1, 2), nt_pca$x[c(84, 866), j], col = "red", cex = 1.25, pch = 16)
		text(rep(1, 2), nt_pca$x[c(84, 866), j], label = c("84", "866"), pos = 4, cex = 1, offset = 1, col = "red")
	}
}

# not to insert
a = 0.999998
scale_pc1 = scale(nt_pca$x[, 1])
univ_out_1 = which(abs(scale_pc1) > qnorm(a), arr.ind = T)
univ_out_1
a = 0.9999
scale_pc23 = scale(nt_pca$x[, 1:3])
univ_out_23 = which(abs(scale_pc23) > qnorm(a), arr.ind = T)
univ_out_23

nt = as.data.frame(nt)
min_mean_max = as.data.frame(round(matrix(c(sapply(nt, min),
	sapply(nt, mean), sapply(nt, max)),
	byrow = T, nrow = 3), 3))
colnames(min_mean_max) = colnames(nt)
rownames(min_mean_max) = c("min", "mean", "max")
min_mean_max

out_all = c(32, 33, 286, 287, 411, 412, 49, 866, 84)
univ_out = round(nt[out_all, ], 3)
univ_out


# 3.5
library(scatterplot3d)
par(mfrow = c(1, 1))
color_out = rep("gray35", dim(nt)[1])
label_out = rep("", dim(nt)[1])
color_out[out_all] = "red"
label_out[c(32, 286, 411, 49, 866, 84)] = c("32-33", "286-287", "411-412", "49", "866", "84")
plot3d = scatterplot3d(nt_pca$x[, 1], nt_pca$x[, 2], nt_pca$x[, 3], angle = 50,
	pch = 16, color = color_out, xlab = "PC1", ylab = "PC2", zlab = "PC3")
plot3d_coords = plot3d$xyz.convert(nt_pca$x[, 1], nt_pca$x[, 2], nt_pca$x[, 3])
text(plot3d_coords$x[-c(286, 411)],	plot3d_coords$y[-c(286, 411)],
	labels = label_out[-c(286, 411)], cex = .5, pos = 4, col = "red")
text(plot3d_coords$x[286],	plot3d_coords$y[286],
	labels = label_out[286], cex = .5, pos = 3, col = "red")
text(plot3d_coords$x[411],	plot3d_coords$y[411],
	labels = label_out[411], cex = .5, pos = 2, col = "red")

# not to insert
# library(rgl)
# par(mfrow = c(1, 1))
# color_out = rep("gray35", dim(nt)[1])
# color_out[out_all] = "red"
# plot3d(nt_pca$x[, 1], nt_pca$x[, 2], nt_pca$x[, 3], angle = 55,
#	pch = 16, size = 8, col = color_out, xlab = "PC1", ylab = "PC2", zlab = "PC3")


# 3.6
pc1_3 = nt_pca$x[, 1:3]
d = mahalanobis(pc1_3, center = colMeans(pc1_3), cov = var(pc1_3))
plot(qchisq(ppoints(d), df = ncol(pc1_3)), sort(d),
	 xlab = "theoretical quantiles", ylab = "sample quantiles", pch = 16, ylim = c(0, 140))
abline(0, 1, col = "blue")
pos_out_all = c(2, 1, 3, 1, 3, 1, 3, 3, 3)
for (i in out_all){
	current_x = qchisq(ppoints(d)[match(i, order(d))], df = ncol(pc1_3))
	current_y = d[i]
	text(current_x, current_y, labels = as.character(i),
		pos = pos_out_all[match(i, out_all)], cex = 0.6, offset = 0.3, col = color_out[i])
	points(current_x, current_y, col = color_out[i], pch = 16, cex = 1.25)
}

pc1_3_out = pc1_3[-out_all, ]
d_out = mahalanobis(pc1_3_out, center = colMeans(pc1_3_out), cov = var(pc1_3_out))
plot(qchisq(ppoints(d_out), df = ncol(pc1_3_out)), sort(d_out),
	 xlab = "theoretical quantiles", ylab = "sample quantiles", pch = 16, ylim = c(0, 10), xlim = c(0, 12))
abline(0, 1, col = "blue")


# 3.7
color_out = rep("black", dim(nt)[1])
label_out = rep("", dim(nt)[1])
color_out[out_all] = "red"
label_out[c(32, 286, 411, 49, 866, 84)] = c("32-33", "286-287", "411-412", "49", "866", "84")
plot(d, pch = 16, xlab = "index", ylab = "squared mahalanobis distance",
	col = color_out, ylim = c(0, 140))
text(seq_len(nrow(pc1_3)), d, labels = label_out, pos = 3, cex = 0.75, offset = 0.3, col = color_out)
abline(h = qchisq(0.95, df = ncol(pc1_3)), lty = 2, col = "red")
alpha_data = (nrow(pc1_3) - 0.5) / nrow(pc1_3)
abline(h = qchisq(alpha_data, df = ncol(pc1_3)), lty = 2, col = "blue")
legend(x = "topleft", legend = c("0.95", round(alpha_data, 3)),
	col = c("red", "blue"), lty = 2, title = "chi-squared quantile")

out_all = c(49, 84, 866)
color_out = rep("black", dim(nt)[1])
label_out = rep("", dim(nt)[1])
color_out[out_all] = "red"
label_out[out_all] = c("49", "84", "866")
out_2 = c(439, 429, 440, 438)
color_out[out_2] = rgb(171 / 255, 248 / 255, 228 / 255)
d = mahalanobis(nt, center = colMeans(nt), cov = var(nt))
plot(d, pch = 16, xlab = "index", ylab = "squared mahalanobis distance",
	col = color_out, ylim = c(0, 410))
text(seq_len(nrow(nt)), d, labels = label_out, pos = 3, cex = 0.75, offset = 0.3, col = color_out)
abline(h = qchisq(0.95, df = ncol(nt)), lty = 2, col = "red")
alpha_data = (nrow(nt) - 0.5) / nrow(nt)
abline(h = qchisq(alpha_data, df = ncol(nt)), lty = 2, col = "blue")
legend(x = "topleft", legend = c("0.95", round(alpha_data, 3)),
	col = c("red", "blue"), lty = 2, title = "chi-squared quantile")