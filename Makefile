cat := $(if $(filter $(OS),Windows_NT),type,cat)
FINAL_DATA = $(shell cat data/curr_final_anno_docs.txt)

.PHONY: all
all: data/curr_final_anno_docs.txt output/03a_coded_data/wodata.rds

data/curr_final_anno_docs.txt: data/curr_texts.txt
	bash src/dev/update_final_anno_docs.sh

data/01a_r_tables/%.csv: data/00_rezrObj/%.Rdata 
	Rscript src/01_prep_wo_predictors.R -d $(basename $(@F)) --debug FALSE > output/debug/01/$(basename $(@F)).txt

output/02_final_data/%.csv: data/01a_r_tables/%.csv data/01b_manual_tables/%.csv
	Rscript src/02_integrate_changes.R -d $(basename $(@F)) --debug FALSE

data/02_rezrDF/%.Rdata: data/01a_r_tables/%.csv data/01b_manual_tables/%.csv
	Rscript src/02_integrate_changes.R -d $(basename $(@F)) --debug FALSE

output/03a_coded_data/wodata.rds: $(FINAL_DATA)
	@echo "$<"
	Rscript src/03a_analysis_preprocess.R

output/best_model.rds: output/03a_coded_data/wodata.rds
	Rscript src/03b_analysis_model.R