var d3 = require('d3');
require('d3-graphviz');

var app = Elm.Main.init({
    node: document.getElementById('elm-app')
});

app.ports.renderDot.subscribe(function (dot) {
    d3.select('#graph').graphviz()
        .renderDot(dot);
});

app.ports.clearGraph.subscribe(function () {
    var graph = document.getElementById('graph');
    while (graph.firstChild) {
        graph.removeChild(graph.firstChild);
    }
});
