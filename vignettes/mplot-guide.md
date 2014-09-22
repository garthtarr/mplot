---
title: "A brief introduction to mplot"
author: "Garth Tarr"
date: "`s Sys.Date()`"
output: html_document
---

<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{A brief introduction to mplot}
-->

# A brief introduction to mplot

The **mplot** package implements a range of model selection procedures and model stability plots designed to provide users with the information they need to make informed decisions about model selection issues in linear and mixed models.

There are two main componenets: model selection via the adaptive fence procedure (Jiang et. al., 2009) and model stability curves as described in Müller and Welsh (2010).

## Adaptive fence

The adaptive fence procedure is ... super brief recap of theory here.

### Artificial example

The adaptive fence procedure is (currently; in future it will be wrapped into the higher level `fencer` function) implemented through the function `af` as follows:


```r
require(mplot,quietly=TRUE)
op = options(gvis.plot.tag = "chart")
n = 100
set.seed(11)
e = rnorm(n)
x0 = 1
x1 = rnorm(n)
x2 = rnorm(n)
x3 = x1^2
x4 = x2^2
x5 = x1*x2
y = x0 + x1 + x2 + e
dat = data.frame(y,x1,x2,x3,x4,x5)
lm1 = lm(y~., data=dat)
af1 = af(lm1, n.cores=3)
```

```
## Loading required package: doMC
## Loading required package: foreach
## foreach: simple, scalable parallel programming from Revolution Analytics
## Use Revolution R for scalability, fault tolerance and more.
## http://www.revolutionanalytics.com
## Loading required package: iterators
```

```r
summary(af1)
```

```
## 
## Call:
## af(mf = lm1, n.cores = 3)
## 
## Adaptive fence model (c*=18.3):
## y ~ x1 + x2
## 
## Model sizes considered: 2 to 5 (including intercept).
## 
## Stepwise procedures:
## Forwards AIC: y ~ x1 + x2 + x3
## Backwards AIC: y ~ x1 + x2 + x3
## Forwards BIC: y ~ x1 + x2
## Backwards BIC: y ~ x1 + x2
```


```r
plot(af1)
```

<!-- ScatterChart generated in R 3.0.3 by googleVis 0.5.3 package -->
<!-- Tue Jul 22 07:58:08 2014 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataScatterChartID5ecbdf09e48 () {
var data = new google.visualization.DataTable();
var datajson =
[
 [
 2.48,
null,
null,
0.4 
],
[
 4.74,
null,
0.57,
null 
],
[
 7,
null,
0.7,
null 
],
[
 9.27,
null,
0.92,
null 
],
[
 11.53,
null,
0.97,
null 
],
[
 13.79,
null,
1,
null 
],
[
 16.05,
null,
1,
null 
],
[
 18.31,
null,
1,
null 
],
[
 20.58,
null,
1,
null 
],
[
 22.84,
null,
1,
null 
],
[
 25.1,
null,
1,
null 
],
[
 27.36,
null,
0.97,
null 
],
[
 29.62,
null,
1,
null 
],
[
 31.89,
null,
0.8,
null 
],
[
 34.15,
null,
0.77,
null 
],
[
 36.41,
null,
0.67,
null 
],
[
 38.67,
null,
0.55,
null 
],
[
 40.93,
0.47,
null,
null 
],
[
 43.19,
0.53,
null,
null 
] 
];
data.addColumn('number','c.range');
data.addColumn('number','y ~ x1');
data.addColumn('number','y ~ x1 + x2');
data.addColumn('number','y ~ x1 + x2 + x3');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartScatterChartID5ecbdf09e48() {
var data = gvisDataScatterChartID5ecbdf09e48();
var options = {};
options["allowHtml"] = true;
options["title"] = "Adaptive fence: c*=18.3";
options["vAxis"] = {title:'p*',minValue:0,maxValue:1,
                  ticks: [0.0,0.2,0.4,0.6,0.8,1.0]};
options["hAxis"] = {title:'c'};
options["axisTitlesPosition"] = "out";
options["chartArea"] = {left:50,top:30,width:'60%',height:'80%'};
options["width"] =    800;
options["height"] =    400;

    var chart = new google.visualization.ScatterChart(
    document.getElementById('ScatterChartID5ecbdf09e48')
    );
    chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = "corechart";
  
// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
  pkgs.push(chartid);
  
// Add the drawChart function to the global list of callbacks
callbacks.push(drawChartScatterChartID5ecbdf09e48);
})();
function displayChartScatterChartID5ecbdf09e48() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
  var pkgCount = pkgs.length;
  google.load("visualization", "1", { packages:pkgs, callback: function() {
  if (pkgCount != pkgs.length) {
  // Race condition where another setTimeout call snuck in after us; if
  // that call added a package, we must not shift its callback
  return;
}
while (callbacks.length > 0)
callbacks.shift()();
} });
}, 100);
}
 
// jsFooter
</script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartScatterChartID5ecbdf09e48"></script>
 
<!-- divChart -->
  
<div id="ScatterChartID5ecbdf09e48" 
  style="width: 800; height: 400;">
</div>


















