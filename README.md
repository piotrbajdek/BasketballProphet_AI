# BasketballProphet AI

BasketballProphet AI is an artificial intelligence program designed to forecast basketball scores. It has been tested on NBA datasets, demonstrating an accuracy rate of around 86% in predicting the winner, after excluding 46% of the most uncertain results in the first step.

Technically, version 0.4.0 of the program is built upon a 6-neuron neural network that utilises the Adam gradient descent optimisation algorithm and employs a simple attention mechanism. It predicts scores for each of the teams. Moreover, the Random Walk Index and the Stochastic Oscillator are implemented.

A valid test file should include two columns representing past scores in .csv format, with the most recent score at the bottom and a minimum of 24 records (more data is preferable for enhanced accuracy). Provide the file as a command line argument.

![example-1](https://github.com/piotrbajdek/BasketballProphet_AI/blob/main/docs/images/example-1.png?raw=true)

To compile the program, you can use gfortran with the following command:

`gfortran basketballprophet_ai.f90 -o basketballprophet_ai`

BasketballProphet AI is available under the ![BSD 3-Clause No Military License](https://github.com/piotrbajdek/BasketballProphet_AI/blob/main/LICENSE.md).
