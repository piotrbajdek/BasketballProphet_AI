# BasketballProphet AI

BasketballProphet AI is an artificial intelligence program designed to forecast basketball scores. It has been tested on NBA datasets, demonstrating an accuracy rate of around 92% in predicting the winner, after excluding 49% of the most uncertain results in the first step.

Technically, version 0.5.0 of the program is built upon a 6-neuron neural network that utilises the Adam gradient descent optimisation algorithm and employs a simple attention mechanism. It predicts scores for each of the teams. Moreover, the Random Walk Index, the Stochastic Oscillator, and a kind of Volatility Channel are implemented. The technical indicators employ the Narayana's cows sequence and the Supergolden Ratio.

A valid test file should include two columns representing past scores in .csv format, with the most recent score at the bottom and a minimum of 24 records (more data is preferable for enhanced accuracy). Provide the file as a command line argument.

## FAQ

Q: Is it really over 90% effective at predicting the winner?

A: It appears to be so, based on my tests using NBA datasets. I have not extensively tested it with datasets from other basketball leagues.

Q: Can I use it for gambling?

A: You may utilise it as you see fit, provided you adhere to the conditions outlined in the program licence. These conditions absolve me of any legal responsibility.

Q: Why is it available publicly as open-source software?

A: When I initiated this project, I did not anticipate that the program would be so effective.

Q: Will bookmakers go bankrupt?

A: In the improbable event that this program gains popularity, it is conceivable that they might. This could disrupt the system.

Q: Can I get rich by the use of BasketballProphet AI?

A: Not really. Due to the inverse correlation between the predictability of results and potential gains in bets, along with the highly unfavorable taxation system in certain countries, even the most reliable software in the world won't necessarily make you rich. Consider this: if the program could generate significant income, I would never release it publicly!

Q: Being so, why do you develop it?

A: It serves as a testing ground for a scientific project.

## USAGE

![example-1](https://github.com/piotrbajdek/BasketballProphet_AI/blob/main/docs/images/example-1.png?raw=true)

To compile the program, you can use gfortran with the following command:

`gfortran basketballprophet_ai.f90 -o basketballprophet_ai`

BasketballProphet AI is available under the ![BSD 3-Clause No Military License](https://github.com/piotrbajdek/BasketballProphet_AI/blob/main/LICENSE.md).
