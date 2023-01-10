# phi_psii_sensitivity
Model and code for phi psii temperature sensitivity analysis for 'The temperature response of photosystem II quantum yield is an important driver of leaf photosynthesis: a review and data synthesis' Posch & Smith (in prep)

## folders
- [analysis](analysis)
	- [box_figure.R](analysis/box_figure.R): creates the figure for Box 1
	- [model_figure.R](analysis/model_figure.R): creates figure 3
- [literature_psii_data](literature_psii_data)
	-  [extracted_psii_t_response_data_v2.csv](literature_psii_data/extracted_psii_t_response_data_v2.csv):
	 phi PSII data extracted from literature and used for supplementary figures
	 	- column descriptions
	 		- paper: 
	 		- id: 
	-  [phi.psii.meta.analysis.tansley.R](literature_psii_data/phi.psii.meta.analysis.tansley.R):
	 creates figure 2 and supplementary figures
- [model_code](model_code)
	- [functions](model_code/functions): contains functions required to run photosynthesis model
	- [photosynthesis_model.R](model_code/photosynthesis_model.R): photosynthesis model based on Farquahr, von Caemmerrer and Berry (1980)
	- [test_photosynthesis_model.R](model_code/test_photosynthesis_model.R): test of photosynthesis model 
