summary(pool(model))
nul <- capture.output(
  aov_fit <- miceadds::mi.anova(mi.res=impCompleteDashboardNurOhneFeedback, formula="Joy~ Dashboard" )
)

aov_fit$r.squared
(fval <- mean(round(aov_fit$anova.table$`F value`, 2), na.rm=TRUE) )


df_mod <- aov_fit$anova.table$df1[- nrow(aov_fit$anova.table)]  ## DF model

df_res <- el(model$analyses)$df.residual  ## DF residual

c(df_mod, df_res)

pf(q=fval, df1=sum(df_mod), df2=df_res, lower.tail=FALSE)


sprintf('Pooled R-squared: %s', round(aov_fit$r.squared, 4))


tmp <- aov_fit$anova.table

sprintf('Pooled F-statistic: %s on %s and %s DF,  p-value: %s', 
        mean(round(tmp$`F value`, 2), na.rm=TRUE), 
        round(sum(tmp$df1[- nrow(aov_fit$anova.table)]), 2),
        round(el(model$analyses)$df.residual, 2),
        format.pval(pf(fval, sum(df_mod), df_res, lower.tail=FALSE)))

adjR2 <- \(r2, n, p) {
  1 - (n - 1)/(n - p - 1)*(1 - r2)
}

adjR2(aov_fit$r.squared, nrow(nhanes), sum(aov_fit$anova.table$df1, na.rm=TRUE))

pool.r.squared(model)

View(datasetMerged)

