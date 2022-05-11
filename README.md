# Taiwan mortality prediction
Singular Value Decomposition (SVD) and Principal Component Analysis (PCA)\
both can be used to reduce the data dimensionality\
\
the Taiwan mortality data, 17 five-age groups for ages 0~ 4, 5~ 9, …, 80~84 in 2001-2020,\
from the webpage of Ministry of Interior uesd  to demonstrate how these two methods work.\
\
The data of the years 2001-2015 are used as the “training”(in-sample) data\
and the years 2016-2020 are used as the “testing” data.

<110-2 academic year computer and simulation, nccu, csyue>
---
### **SVD** 
mortality prediction using the model 'Lee-Carter model'\
ln(mortality) = a_x + b_x*k_t + error\
applying the SVD, i.e., lm(mortality - a_x) = UPV'\
the matrix U represents the time component,\
and V is the age component.\

### **PCA**
similar to SVD,\
loading represents the age component,\
scores reprensents the time components.\

### **output**
method : SVD\
<img src="mortality predict by SVD.png" alt="Cover" width="35%"/>

|year|2016|2017|2018|2019|2020|
|:-----|:-----|:-----|:-----|:-----|:-----|
|MAPE|4.3581%|1.5019%|0.7129%|0.4138%|3.3479%|

method : PCA\
<img src="mortality predict by PCA.png" alt="Cover" width="35%"/>

|year|2016|2017|2018|2019|2020|
|:-----|:-----|:-----|:-----|:-----|:-----|
|MAPE|10.0778%|8.1116%|8.0680%|7.7220%|5.7398%|



