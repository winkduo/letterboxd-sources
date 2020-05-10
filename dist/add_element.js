var v = document.createElement("p")
v.classList.add("service")
v.style = "display: block;"

var span = document.createElement("span")
span.classList.add("title")

var title = document.getElementsByClassName("headline-1")[0].textContent;
var span2 = document.createElement("span")
span2.classList.add("name")
span2.appendChild(document.createTextNode(title))

var a = document.createElement("a")
a.classList.add("label")
a.href = "http://tafdi.org"

span.appendChild(span2)
a.appendChild(span)
v.appendChild(a)

document.getElementsByClassName("services")[0].appendChild(v)
