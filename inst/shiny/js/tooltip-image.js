// Start of the tooltip-image.js file
// inspired, in part, by https://stackoverflow.com/a/48174836/1583084
function(el) {
  var tooltip = d3.select('#' + el.id + ' .svg-container')
    .append("div")
    .attr("class", "my-custom-tooltip");

  el.on('plotly_hover', function(d) {
    var pt = d.points[0];
    // Choose a location (on the data scale) to place the image
    // Here I'm picking the top-left corner of the graph
    var x = pt.x;
    var y = pt.y;
    // Transform the data scale to the pixel scale
    var xPixel = pt.xaxis.l2p(x) + pt.xaxis._offset;
    var yPixel = pt.yaxis.l2p(y) + pt.yaxis._offset;
    // Insert the base64 encoded image
    var img = "";
    if (pt.customdata) img = img + "<img src='" + pt.customdata + "' width=100>";
    img = img +
    '<p style="text-align:center;color:white;background-color:black">' +
    pt.text + "</p>";
    tooltip.html(img)
      .style("position", "absolute")
      .style("left", xPixel + "px")
      .style("top", yPixel + "px");
    // Fade in the image
    tooltip.transition()
      .duration(100)
      .style("opacity", 1);
  });

  el.on('plotly_unhover', function(d) {
    // Fade out the image
    tooltip.transition()
      .duration(0)
      .style("opacity", 0)
      .style("left", "-100px")
      .style("top", "-100px");
  });
}
