const purescript = require ("./output/Main")
const Mustache = require ("mustache")

var mainTemplate = document.getElementById("main").innerHTML
var renderedMain = Mustache.render(mainTemplate, { services: [{service: {  id: 1, name: "S1" }}] })

document.getElementById("container").innerHTML = renderedMain

purescript.main ()
