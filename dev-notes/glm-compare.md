# `glm` Arguments

argument|default|won't impl|impl
---|---|---|---
formula|||x|
family|||
data|||x|
weights|||x|
subset||x|implement in feed
na.action||x|implement in feed
start|||
etastart|||
mustart|||
offset|||x|
control|||
model|TRUE|x||
method|"qlm.fit"|x|always iwls with as274
x|FALSE|x||
y|TRUE|x||
contrasts||x||
intercept|||
...||x||


# `glm` Values

value|won't impl|impl
---|---|---
coefficients||x
residuals|x|
fitted.values|x|
rank||x
family||x
linear.predictors|x|
deviance||
aic||
null.deviance||
iter||x
weights|x|
prior.weights||
df.residual||x
df.null||
y|x|
x|x|
model|x|
converged||x
boundary||
call||x
formula||
terms||x
data||
offset|x|
control||
method||
contrasts|x|
xlevels|x|
na.action|x|
effects|x|
qr||x


# `summary.glm` Arguments

argument|default|won't impl|impl
---|---|---|---
object|||x
x||x|
dispersion|||
correlation|FALSE||x
digits|||x
symbolic.corr|FALSE||x
signif.stars|||x
...|||


# `summary.glm` Values

values|won't impl|impl
---|---|---
call||
family||
deviance||
contrasts||
df.residual||
null.deviance||
df.null||
deviance.resid||
coefficients||x
aliased||x
dispersion||
df||
cov.unscaled||
cov.scaled||
correlation||
symbolic.cor||
