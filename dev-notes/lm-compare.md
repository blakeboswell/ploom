# `lm` Arguments

argument|default|won't impl|impl
---|---|---|---
formula|||x|
data|||x|
subset||| implement in feed
weights|||x|
na.action||| implement in feed
method|"qr"|x|"as274"
model|TRUE|x||
x|TRUE|x||
y|TRUE|x||
qr|TRUE||x|
singular.ok|TRUE|||
contrasts||x||
offset|||x|
...||||


# `lm` Values

value|won't impl|impl
---|---|---
coefficients||x
residuals|x|
fitted.values|x|
rank||x
weights|x|
df.residual||x
call||x
terms||x
contrasts|x|
xlevels|x|
offset|x|
y|x|
x|x|
model|x|
na.action|x|
assign||x
effects|x|
qr||x


# `summary.lm` Arguments

argument|default|won't impl|impl
---|---|---|---
object|||x
x||x|
correlation|FALSE||x
digits|||x
symbolic.corr|FALSE||x
signif.stars|||x
...|||


# `summary.lm` Values

values|won't impl|impl
---|---|---
residuals|x|
coefficients||x
aliased||x
sigma||x
df||x
fstastic||x
r.squared||x
adj.r.squared||x
cov.unscaled||x
correlation||x
na.action|x|