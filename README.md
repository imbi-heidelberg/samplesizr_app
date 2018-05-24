---
output:
  html_document: default
  pdf_document: default
---
Hello! Welcome to the *samplesizr* web application.

<span style="color:red">Please note: This App currently isn't working with the Edge browser. Please use another browser such as Internet Explorer, Google Chrome, Firefox etc.</span>

***

#### Introduction

While planning a medical experiment or a clinical trial, the calculation of the sample size is essential.
This web application gives you a fast first access to calculate the required sample size for your project. 
On the panel above, you can choose the statistical test you plan to
perform. Choose the input parameters and press 'Calculate!' to perform the
sample size calculation.

For an introduction to sample size calcuation for clinical trials and
to understand the theory behind this application the following book may be helpful:

> [1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition,
Springer Verlag.

***

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

The Results:

First the Input parameters will be reported:

    Significance level : 0.050
    Desired power : 90.00 %
    Effect size : 10.00
    Standard deviation : 20.00
    Allocation : 1.00.

Allocation is defined as the ratio (n intervention group) : (n control group).
The resulting sample size and the actual power using this sample size is reported:

    n control group : 85
    n intervention group : 85
    n total : 170
    Actual power : 90.31373 %.
    
***

#### R package

This application developed on basis of the calculations presented by M. Kieser [1]
and uses the functions provided by the R package *samplesizr*. 
To install the R package follow these steps.

Install and load the R package devtools:

    install.packages(devtools)
    library(devtools)
    
Install the *samplesizr* package from github.com:

    install_github('imbi-heidelberg/samplesizr')

Load the package and look up the documentation for an overview:

    library(samplesizr)
    ?samplesizr

The R package includes the functions needed to calculate power and sample-size
for each test. The power function for ANCOVA is not included.
***

#### About samplesizr

*samplesizr* is a web application based on the R package 
<a href="https://github.com/imbi-heidelberg/samplesizr">
samplesizr.
</a> 
Special thanks to Kevin Kunzmann for his supervision, patience and help whilst 
teaching me the tools which made our work on this project very efficient and productive.
Special thanks to Professor Meinhard Kieser for his inspiration to this project 
and teaching of the theory behind sample size calculation.

A click on the logo will lead you to the webpage of our Institute:

<a href="https://www.klinikum.uni-heidelberg.de/Homepage-Abteilung.7980.0.html">
  <img src="imbi.png" alt="IMBI Heidelberg">
</a>

Daniel Goseberg,  
IMBI Heidelberg,  
March 16th in Heidelberg