# Results

Columns in *saved_results.csv*:
- **repeat_id** (*numeric*): Number of the repeat
- **true_base** (*numeric*): True base (intercept) value
- **true_sex** (*numeric*): True main effect of sex
- **true_cond** (*numeric*): True main effect of condition
- **true_sex_cond** (*numeric*): True interation effect between sex and condition
- **var_pop** (*numeric*): variation in population (drawn from a gamma distribution)
- **expt** (*numeric*): experiment number in the current repeat
- **analysis_type** (*character*): type of analysis, e.g. "pp_citation_pb_sym" refers to citation chain with symmetrical publication bias. 
- **pub_method** (*character*): Whether asymmetrical or symmetrical publication bias was used in the simulation, indicated by "asym" and "sym"
- **pub_true** (*numeric*): whether the study was published or not, indicated by 0 and 1.
- **posteriors_included** (*numeric*): the number of posteriors included in the prior. If 0, then standard priors were used.
- **b_base_med** (*numeric*): median of the estimated intercept
- **b_sex_med** (*numeric*): median of the estimated effect of sex
- **b_cond_med** (*numeric*): median of the estimated effect of condition
- **b_sex_cond_med** (*numeric*): median of the estimated effect of sex*cond
- **b_base_lower** (*numeric*): lower SE of the estimated intercept
- **b_sex_lower** (*numeric*): lower SE of the estimated effect of sex
- **b_cond_lower** (*numeric*): lower SE of the estimated effect of condition
- **b_sex_cond_lower** (*numeric*): lower SE of the estimated effect of sex*cond
- **b_base_upper** (*numeric*): upper SE of the estimated intercept
- **b_sex_upper** (*numeric*): upper SE of the estimated effect of sex
- **b_cond_upper** (*numeric*): upper SE of the estimated effect of condition
- **b_sex_cond_upper** (*numeric*): upper SE of the estimated effect of sex*cond