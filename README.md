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
	 		- paper: paper that data was sourced from (corresponds to papers listed in Table 1)
	 		- id: identification code for individual phi PSII temperature response curve
			- species: plant species (genus and species specified except where not supplied by source paper)
			- growth.temp: growth temperature of plant (°C)
			- meas.temp: measurement temperatures (°C)
			- phi.psii: phi PSII values extracted from publication
			- topt.normalised: extracted phi PSII values normalised according to the maximum phi PSII value reported for each curve (e.g. in Bernacchi (2003) the phi PSII value measured at 10°C = 0.538, and the maximum reported phi PSII value = 0.702. Therefore, the normalised phi PSII value at 10°C = 0.538/0.702 = 0.766)
			- omitted_fig2: was the data omitted from figure 2 (yes/no)?
	-  [phi.psii.meta.analysis.tansley.R](literature_psii_data/phi.psii.meta.analysis.tansley.R):
	 creates figure 2 and supplementary figures
- [model_code](model_code)
	- [functions](model_code/functions): contains functions required to run photosynthesis model
	- [photosynthesis_model.R](model_code/photosynthesis_model.R): photosynthesis model based on Farquahr, von Caemmerrer and Berry (1980)
	- [test_photosynthesis_model.R](model_code/test_photosynthesis_model.R): test of photosynthesis model

[![DOI](https://zenodo.org/badge/551106840.svg)](https://zenodo.org/badge/latestdoi/551106840)
