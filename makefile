all:
	Rscript -e 'rmarkdown::render("./simplified_r_statistic/Process_Flow_Chart.rmd", "html_document")'
	Rscript -e 'rmarkdown::render("./simplified_r_statistic/Code_Explanation.rmd", "html_document")'
	Rscript -e 'rmarkdown::render("./simplified_r_statistic/Update.rmd", "html_document")'
	Rscript -e "rsconnect::deployApp('./simplified_r_statistic/', forceUpdate = getOption('rsconnect.force.update.apps', TRUE))"