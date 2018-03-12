Hello! Welcome to the samplesizr web application.

While planning a medical experiment or a clinical trial, the calculation of the sample size is essential.
This web application gives you a fast first access to calculate the required sample
size for your project. 
On the panel above, you can choose the statistical test you plan to
perform. Choose the input parameters and press 'Calculate!' to perform the
sample size calculation.

For an introduction to sample size calcuation for clinical trials and
to understand the theory behind this application the following book may be helpful.
This application developed on basis of the calculations presented in this book.

> [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition,
Springer Verlag.

This app cannot replace the judgement and expertise of a biometrician 
required to perform a valid sample size calculation by considering the specific details of your study.
It provides a tool for the computational part of sample size calculation for a variety of common statistical tests.


#### Example

Imagine a clinical trial with an intervention group and a control group.
The primary endpoint is the quality of life for cancer patients. The QLQ-C30 questionnaire is used to operationalize this endpoint.
For a new intervention you expect an improval in quality of life as compared to the control.
A difference of 10 points on the scale is needed
to consider the new intervention clinically relevant 'better'.
To achieve a decision in favour of the new intervention, given the intervention is really
'better' by this amount, a probability of at least 0.9 (power) is desired.
You want to tolerate a maximal type I error rate of 0.05 (two-sided) / 0.025 (one-sided).
You know that the standard deviation will be 20 points.
Choose the tab 'z-test' above and click 'Calculate' to look up the results!

#### About samplesizr

Samplesizr is a web application based on the R package 
<a href="https://github.com/goseberg/samplesizr">
samplesizr.
</a> 
Special thanks to Kevin Kunzmann for his supervision, patience and help whilst 
teaching me the tools which made our work on this project very efficient and productive.
Special thanks to Prof. Meinhard Kieser for his inspiration to this project 
and teaching of the theory behind sample size calculation.

A click on the logo will lead you to the webpage of our Institute:

<a href="https://www.klinikum.uni-heidelberg.de/Homepage-Abteilung.7980.0.html">
  <img src="imbi.png" alt="IMBI Heidelberg">
</a>

Daniel Goseberg,  
IMBI Heidelberg,  
March 12th in Heidelberg