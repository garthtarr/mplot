<!--code almost entirely from http://bl.ocks.org/4063663 Mike Bostock's d3 Brushable Scatterplot -->

<style>

svg {
font: 14px sans-serif;
padding: 10px;
}

.axis,
.frame {
shape-rendering: crispEdges;
}

.axis line {
stroke: #ddd;
stroke-width: 1px;
}

.axis path {
display: none;
}

rect.extent {
fill: #000;
fill-opacity: .125;
stroke: #fff;
}

rect.frame {
fill: #fff;
fill-opacity: .7;
stroke: #aaa;
}

.frame {
fill: none;
stroke: #aaa;
}

circle {
fill-opacity: .7;
}

circle.greyed {
fill: #ccc !important;
}

.extent {
fill: #000;
fill-opacity: .125;
stroke: #fff;
}

</style>
<script src="http://d3js.org/d3.v3.min.js"></script>
<script>
var networkOutputBinding = new Shiny.OutputBinding();
$.extend(networkOutputBinding, {
find: function(scope) {
return $(scope).find('.shiny-network-output');
},
renderValue: function(el, data) {


//remove old graph    
d3.select("#scatterplot").text(null);
var svg = d3.select("#scatterplot").select("svg")
.remove();

var width = 960,
size = 150,
padding = 10;

var x = d3.scale.linear()
.range([padding / 2, size - padding / 2]);

var y = d3.scale.linear()
.range([size - padding / 2, padding / 2]);

var xAxis = d3.svg.axis()
.scale(x)
.orient("bottom")
.ticks(5);

var yAxis = d3.svg.axis()
.scale(y)
.orient("left")
.ticks(5);

var color = d3.scale.category10();

// where the magic happens with d3js interacting with shiny
var domainByTrait = {},
traits = d3.keys(data[0]).filter(function(d) { return d !== "factor"; }),
factor = d3.keys(data[0]).filter(function(d) { return d == "factor"; }),
n = traits.length;

traits.forEach(function(trait) {
domainByTrait[trait] = d3.extent(data, function(d) { return +d[trait]; });
});

//move the append svg down here since depends on n
svg = d3.select("#scatterplot").append("svg")
.attr("width", size * n + padding*2)
.attr("height", size * n + padding)
.append("g")
.attr("transform", "translate(" + padding*2 + "," + padding / 2 + ")");
xAxis.tickSize(size * n);
yAxis.tickSize(-size * n);

var brush = d3.svg.brush()
.x(x)
.y(y)
.on("brushstart", brushstart)
.on("brush", brushmove)
.on("brushend", brushend);

// X-axis.
svg.selectAll(".x.axis")
.data(traits)
.enter().append("g")
.attr("class", "x axis")
.attr("transform", function(d, i) { return "translate(" + i * size + ",0)"; })
//.attr("transform", function(d, i) { return "translate(" + (n - i - 1) * size + ",0)"; })
.each(function(d) { x.domain(domainByTrait[d]); 
d3.select(this).call(xAxis); });

// Y-axis.
svg.selectAll(".y.axis")
.data(traits)
.enter().append("g")
.attr("class", "y axis")
.attr("transform", function(d, i) { return "translate(0," + i * size + ")"; })
//.attr("transform", 
//      function(d, i) { return "translate(0," + i * size + ")"; })
.each(function(d) { y.domain(domainByTrait[d]); 
d3.select(this).call(yAxis); });

// Cell and plot.
var cell = svg.selectAll(".cell")
.data(cross(traits, traits))
.enter().append("g")
.attr("class", "cell")
.attr("transform", 
function(d) { return "translate(" + d.i * size + "," + d.j * size + ")"; })
//.attr("transform", 
//      function(d) { return "translate(" + (n - d.i - 1) * size + "," + d.j * size + ")"; })
.each(plot);



// Titles for the diagonal.
cell.filter(function(d) { return d.i === d.j; }).append("text")
.attr("x", size/2)
.attr("y", size/2)
.text(function(d) { return d.x; }).style("text-anchor", "middle");


cell.call(brush);

function plot(p) {
var cell = d3.select(this);

x.domain(domainByTrait[p.x]);
y.domain(domainByTrait[p.y]);

// Plot frame
cell.append("rect")
.attr("class", "frame")
.attr("x", padding / 2)
.attr("y", padding / 2)
.attr("width", size - padding)
.attr("height", size - padding);

// plot the data
if (p.x !== p.y){ // prevents a main diagonal being plotted
cell.selectAll("circle")
.data(data)
.enter().append("circle")
.attr("cx", function(d) {  return x(d[p.x]); })
.attr("cy", function(d) { return y(d[p.y]); })
.attr("r", 3)
// this tries to color by a factor variable
// called factorvar -- not yet implemented
// if/when implemented it would need a factor legend
.style("fill", function(d) { return color(d.factor); });
}

}

var brushCell;

// Clear the previously-active brush, if any.
function brushstart(p) {
if (brushCell !== this) {
d3.select(brushCell).call(brush.clear());
x.domain(domainByTrait[p.x]);
y.domain(domainByTrait[p.y]);
brushCell = this;
}
}

// Highlight the selected circles.
function brushmove(p) {
var e = brush.extent();
svg.selectAll("circle").classed("greyed", function(d) {
return e[0][0] > d[p.x] || d[p.x] > e[1][0]
|| e[0][1] > d[p.y] || d[p.y] > e[1][1];
});
}

// If the brush is empty, select all circles.
// function brushend() {
//  if (brush.empty()) svg.selectAll(".greyed").classed("greyed", false);
// }

// If the brush is empty, select all circles.
function brushend() {
if (brush.empty()){
svg.selectAll(".greyed").classed("greyed", false);
} 
var circleStates = d3.select('svg')
.select('g')
.selectAll('circle')[0]
.map(function(d) {return d.className['baseVal']});
Shiny.onInputChange("mydata", circleStates);
}



function cross(a, b) {
var c = [], n = a.length, m = b.length, i, j;
for (i = -1; ++i < n;) for (j = -1; ++j < m;) c.push({x: a[i], i: i, y: b[j], j: j});
return c;
}

d3.select(self.frameElement).style("height", size * n + padding + 20 + "px");


}});

Shiny.outputBindings.register(networkOutputBinding, 
'timelyportfolio.networkbinding');

</script>