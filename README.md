# BasketballProphet AI

BasketballProphet AI is an artificial intelligence program designed to forecast basketball scores.  It has been tested on NBA and WNBA datasets, demonstrating an accuracy rate of around 2/3 in predicting the winner, with the expectation of further improvement in future versions of the program.

Technically, version 0.1.0 of the program is built upon an 8-neuron network that utilises the Adam gradient descent optimisation algorithm and employs a simple element-wise dot-product attention. It predicts scores for each of the teams.

A valid test file should include two columns representing past scores in .csv format, with the most recent score at the bottom and a minimum of 24 records (more data is preferable for enhanced accuracy). Provide the file as a command line argument.

To compile the program, you can use gfortran with the following command:

`gfortran -Ofast basketballprophet_ai.f90 -o basketballprophet_ai`

BasketballProphet AI is available under the ![BSD 3-Clause No Military License](https://github.com/piotrbajdek/BasketballProphet_AI/blob/main/LICENSE.md).

Disclaimer: The software creator condemns gambling. While the predictive capabilities of this program likely exceed those of any sports fan on the planet, attempting to profit easily from the bookmaker using the current version of the program, still in its early development stage, is statistically improbable, especially when taking the taxation system into account.
