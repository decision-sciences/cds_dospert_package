
#Overview
dospert is a pacakge used to aid the analysis of DOSPERT data. It includes three functions:

- **d_clean**: d_clean function returns raw DOSPERT data in a panel format. 
- **d_sum**: d_sum function returns sum of DOSPERT responses by domain and scales.
- **d_score**: d_score returns DOSPERT scores attached to the original input dataframe.

#Installation

make sure you have `devtools` installed and run the following code:

```r
library(devtools)
devtools::install_github("decision-sciences/cds_dospert_package")
```


# DOSPERT domains and scales
DOSPERT has five risk domains and three risk types. These are taken as arguments for d_sum, and are used for variable names. 

- Risk domains are abbreviated as follows:
    + financial: *fin*
    + health/safety: *hea* or *saf*
    + recreational: *rec*
    + ethical: *eth*
    + social: *soc*
    
- Risk types are:
    + risk taking: *RT*
    + risk benefit: *RB*
    + risk perception: *RP*
    

# Data
Data for analysis is raw data downloaded from qualtrics. File formats supported are .csv and .xml, with .csv as default. The main difference is that when downloaded, .csv files include first row of column names/questions but .xml files do not. There is no need to manipulate the variable names before using any of the functions. However, for the responses of the dospert questions, variable names should be in "(domain)(risk type)_Question number" (e. g. finRT_1) format. 






```r
dcsv <- read.csv("pilot_data.csv", header = TRUE) # [d]ospert in [csv]
head(dcsv[, 1:10])
```

```
##                  V1                   V2        V3                    V4
## 1        ResponseID          ResponseSet      Name ExternalDataReference
## 2 R_3fYJHku6e1c8gjp Default Response Set Anonymous                      
## 3 R_cGbH7i8U2jwQYo1 Default Response Set Anonymous                      
## 4 R_1LNYShwUeHQ669I Default Response Set Anonymous                      
## 5 R_2UgfnYJCqZrhlBL Default Response Set Anonymous                      
## 6 R_XTE8ILQrqTEmpVv Default Response Set Anonymous                      
##             V5             V6     V7           V8           V9      V10
## 1 EmailAddress      IPAddress Status    StartDate      EndDate Finished
## 2              128.59.199.242      0 6/3/15 17:04 6/3/15 17:07        1
## 3              128.59.199.242      0 6/3/15 17:30 6/3/15 17:31        1
## 4              128.59.199.242      0 6/4/15 10:07 6/4/15 10:08        1
## 5              128.59.199.242      0 6/5/15 16:35 6/5/15 16:35        1
## 6              70.192.234.116      0 6/8/15 13:24 6/8/15 13:26        1
```


```r
dxml <- xmlToDataFrame("pilot_data.xml", stringsAsFactors = F) %>% filter(uid != "")  
# [d]ospert in [xml]
# unique identifying variable for this dataset is 'uid'
# removed observations without uid variable value
head(dxml[, 1:10])
```

```
##          ResponseID          ResponseSet      Name ExternalDataReference
## 1 R_2q8GyJAKfgHxT3r Default Response Set Anonymous                      
## 2 R_33BaSpJt3vm38Hp Default Response Set Anonymous                      
## 3 R_1l5HCrHMeadd1Tj Default Response Set Anonymous                      
## 4 R_3psF61JNrj5d22M Default Response Set Anonymous                      
## 5 R_24w6c78hLXWE4Xk Default Response Set Anonymous                      
## 6 R_1JD6kEbdEIP7Xf7 Default Response Set Anonymous                      
##   EmailAddress       IPAddress Status           StartDate
## 1              207.239.110.242      0 2015-06-26 14:01:42
## 2               128.59.199.242      0 2015-06-26 15:57:24
## 3                 73.180.64.82      0 2015-07-28 10:17:49
## 4                 64.184.91.34      0 2015-07-28 10:17:43
## 5               71.227.179.100      0 2015-07-28 10:18:22
## 6                66.151.30.140      0 2015-07-28 10:20:05
##               EndDate Finished
## 1 2015-06-26 14:02:35        1
## 2 2015-06-26 15:58:18        1
## 3 2015-07-28 10:19:33        1
## 4 2015-07-28 10:20:18        1
## 5 2015-07-28 10:20:54        1
## 6 2015-07-28 10:25:19        1
```

# **1) d_clean**

d_clean function takes three arguments: raw dataframe, unique identifying variable and file type and returns a panel format dataframe, which can be used, for example, for data inspection purposes.



In the sample .csv format dataframe, first column uniquely identifies the respondents. 


```r
csvdclean <- d_clean(dcsv, "V1", file_type = "csv")
head(csvdclean)
```

```
## Source: local data frame [6 x 7]
## 
##           unique_id domain Qnumber    RT    RP    RB    id
##              (fctr)  (chr)   (chr) (dbl) (dbl) (dbl) (dbl)
## 1 R_1DUdsEimDpZTP4S    fin       1     1     1     1     1
## 2 R_1DUdsEimDpZTP4S    fin       2     3     2     2     1
## 3 R_1DUdsEimDpZTP4S    fin       3     4     3     3     1
## 4 R_1DUdsEimDpZTP4S    fin       4     5     4     4     1
## 5 R_1DUdsEimDpZTP4S    fin       5     6     5     5     1
## 6 R_1DUdsEimDpZTP4S    fin       6     7     6     6     1
```

For the .xml format dataframe, the usage is similar: unique identifying variable in this sample dataset is 'uid', and file type is xml.


```r
xmldclean <- d_clean(dxml, "uid", file_type = "xml")
head(xmldclean)
```

```
## Source: local data frame [6 x 7]
## 
##                          unique_id domain Qnumber    RT    RP    RB    id
##                              (chr)  (chr)   (chr) (dbl) (dbl) (dbl) (dbl)
## 1 002BNAWSGQT9YFW6PYQPBJYLKYPUYJUE    fin       1     2     4     3     1
## 2 002BNAWSGQT9YFW6PYQPBJYLKYPUYJUE    fin       2     5     2     4     1
## 3 002BNAWSGQT9YFW6PYQPBJYLKYPUYJUE    fin       3     3     3     4     1
## 4 002BNAWSGQT9YFW6PYQPBJYLKYPUYJUE    fin       4     2     6     5     1
## 5 002BNAWSGQT9YFW6PYQPBJYLKYPUYJUE    fin       5     1     5     2     1
## 6 002BNAWSGQT9YFW6PYQPBJYLKYPUYJUE    fin       6     4     4     5     1
```


# **2) d_sum**

d_sum takes five arguments: raw dataframe, unique identifying variable, risk domain, risk type (or scale) and file type, and returns the sum of the respondents DOSPERT response of the designated risk domain and type by unique id.


```r
csvdsum <- d_sum(dcsv, "V1", "fin", "RT", file_type = "csv")
head(csvdsum)
```

```
##           unique_id finRT_sum
## 2 R_3fYJHku6e1c8gjp        10
## 3 R_cGbH7i8U2jwQYo1        23
## 4 R_1LNYShwUeHQ669I        16
## 5 R_2UgfnYJCqZrhlBL        42
## 6 R_XTE8ILQrqTEmpVv        20
## 7 R_1DUdsEimDpZTP4S        26
```


```r
xmldsum <- d_sum(dxml, "uid", "fin", "RB", file_type = "xml")
head(xmldsum)
```

```
##                          unique_id finRB_sum
## 1                           123456        21
## 2                    1111111111111        21
## 3 BUK5HSGUUIMMFPI5IP286JFSNWKXYWIG        15
## 4 DZQ767VXX7XEDFZJFW6MU7CTWAJ3B1GM        24
## 5 BYF4NEUCXDYMXL2V5FQFGNTRFXU5DBV2        22
## 6 RJSE1JBXWWDLBHSHASKT5Y0I0GYPHREZ        18
```

# **3) d_score**

In order to use d_score, make sure you have the package `lm.beta` installed and loaded into the environment using the following code:

```r
install.packages(lm.beta)
library(lm.beta)
```

d_score takes same three arguments as d_clean, calculates the risk attitude coefficients and returns the results attached to the last columns of input dataframe. The risk attitude coefficients are named "(domain)_int'', "(domain)_RB'', "(domain)_RP''. It also calculates the standardized coefficents for "(domain)_RB'', and "(domain)_RP'' which are listed as "(domain).Standard_RB'', "(domain).Standard_RB'', and the r-squared for each participant for each domain, listed as "(domain).R_square''. 


```r
csvdscore <- d_score(dcsv, "V1", file_type = "csv")
head(csvdscore %>% dplyr::select(unique_ID, fin_int, fin_RB, fin_RP, fin.R_square, fin.Standard_RB, fin.Standard_RP))
```

```
 unique_ID      fin_int        fin_RB        fin_RP fin.R_square fin.Standard_RB
1    152068 5.893701e+00 -3.228346e-01 -4.763780e-01    0.9376136   -3.228346e-01
2    152069 7.333333e+00 -2.500000e-01  0.000000e+00   -0.2146226    4.435103e+00
3    152072 1.450389e-15  1.000000e+00  0.000000e+00    1.0000000    3.244752e-16
4    152074 1.000000e+00 -2.091908e-17  4.183815e-17    0.2753623            -Inf
5    152075 2.281690e+00  9.084507e-01 -3.239437e-01    0.2427404    8.323640e-01
6    152076 1.000000e+00  2.465190e-31           NaN    0.2647059    5.067452e-16
  fin.Standard_RP
1   -6.788627e-01
2    7.324332e-01
3    7.166459e-17
4             Inf
5   -1.649440e-01
6    1.241267e-16
```


```r
xmldscore <- d_score(dxml, "uid", file_type = "xml")
head(xmldscore %>% dplyr::select(unique_ID, rec_int, rec_RB, rec_RP, rec.R_square, rec.Standard_RB, rec.Standard_RP))
```

```
 unique_ID       rec_int        rec_RB        rec_RP rec.R_square rec.Standard_RB
1    152068  2.400000e+00 -4.000000e-01  0.000000e+00    0.2500000    6.782330e-01
2    152069  2.900779e-15  1.000000e+00  0.000000e+00    1.0000000    1.258875e-15
3    152072  8.870370e+00 -9.259259e-02 -1.111111e+00    0.6125731   -6.372659e-02
4    152074  1.000000e+00  1.485264e-30  1.359740e-16    0.3478261             Inf
5    152075 -1.066667e+00  1.266667e+00 -3.333333e-01    0.7333333    6.333333e-01
6    152076  3.500000e+00  1.500000e+00 -2.000000e+00    0.5833333    5.000000e-01
  rec.Standard_RP
1    2.449490e-01
2    2.298380e-16
3   -9.190780e-01
4             Inf
5   -4.772607e-01
6   -7.071068e-01
```




