webapp:
	Rscript ./simplified_r_statistic/initial_data.R
	Rscript -e 'rmarkdown::render("./simplified_r_statistic/Process_Flow_Chart.rmd", "html_document")'
	Rscript -e 'rmarkdown::render("./simplified_r_statistic/Code_Explanation.rmd", "html_document")'
	Rscript -e 'rmarkdown::render("./simplified_r_statistic/Update.rmd", "html_document")'
	Rscript -e "rsconnect::deployApp('./simplified_r_statistic/', forceUpdate = getOption('rsconnect.force.update.apps', TRUE))"

base_line_data:
	Rscript ./scripts/Generate_Data_For_Publication_Results.R -smooth 7 -mean 4 -sd 3 -tau 7 -o "./presentations/Results/DATA - baseline_parameters.csv"
	
vary_smoothing_data:
	Rscript ./scripts/Generate_Data_For_Publication_Results.R -smooth 1:7 -mean 4 -sd 3 -tau 7 -o "./presentations/Results/DATA - vary_smoothing_parameter.csv"

vary_tau_data:
	Rscript ./scripts/Generate_Data_For_Publication_Results.R -smooth 7 -mean 4 -sd 3 -tau 1:7 -o "./presentations/Results/DATA - vary_tau_parameter.csv"

vary_GT_data:
	Rscript ./scripts/Generate_Data_For_Publication_Results.R -smooth 7 -mean "c(2,4,6,8,10)" -sd "c(2,4,6,8,10)" -tau 7 -o "./presentations/Results/DATA - vary_GT_parameter.csv"

results_doc: 
	Rscript -e 'rmarkdown::render("./presentations/Results/Results.Rmd", c("html_document", "pdf_document"))'
	
results: base_line_data vary_smoothing_data vary_tau_data vary_GT_data results_doc