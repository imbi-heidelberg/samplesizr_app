# samplesizr
<hr>

Hello! Welcome to the samplesizr web application.

While planning a clinical trial the calculation of the sample size is essential.
This web application gives you a fast first access to estimate the needed sample
size for your clinical trial. 
On the panel above you can choose the statistical test you want to
perform. Choose the input parameters and press 'Calculate!' to perform the
sample size calculation.

For an introduction in sample size calcuation for clinical trials and
to understand the theory behind this application the following book is recommended.
This application developed on basis of the calculations for this book.

> [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition,
Springer Verlag.

This app cannot replace the judgement and expertise of an biometrician 
performing a sample size calculation cosidering the specific details of your study.
It should give an orientation and a 'feeling' for the power of the certain tests.


### Example

Imagine a clinical trial with an intervention group and a control group.
The primary endpoint is the quality of life for cancer patients. The QLQ-C30 questionnaire is used to operationalize this endpoint.
For a new intervention you expect an improval in quality of life.
A clinical relevant difference of 10 points on the scale is needed
to call the new intervention 'better'.
For the decision for the new intervention, given the intervention is really
'better' you want to have a minimal probability of 0.9 (power).
You want a maximal type I error of 0.05 (two-sided) / 0.025 (one-sided).
You know that the standard deviation will be 20 points.
Choose the tab 'z-test' above and click 'Calculate' to look up the results!

### About samplesizr

Samplesizr is the web application based on the R package 
<a href="https://github.com/goseberg/samplesizr">
samplesizr.
</a> 

The samplesizr is still under development. 
Now only the z test is implemented in the web app with full functionality 
but soon the other tests will follow.

A special thanks to KK for his supervision, patience and help whilst 
teaching me the tools which made our work on this project very efficient and productive.

A special thanks to MK for his inspiration to this project and teaching of the
needed theory behind sample size calculation.

A click on the logo will lead you to the Webpage of our Institute.

<a href="https://www.klinikum.uni-heidelberg.de/Homepage-Abteilung.7980.0.html">
  <img src="imbi.png" alt="IMBI Heidelberg">
</a>


Daniel Goseberg,  
IMBI Heidelberg,  
January 31th in Heidelberg