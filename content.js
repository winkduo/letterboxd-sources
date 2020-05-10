chrome.runtime.onMessage.addListener(
  function(request, sender, sendResponse) {
  console.log(sender.tab ?
              "from a content script:" + sender.tab.url :
              "from the extension");
  if (request.type == "addService") {
    addService(request.serviceName);
    sendResponse({farewell: "goodbye"});
  }
});

function addService(name) {
  var v = document.createElement("p")
  v.classList.add("service")
  v.style = "display: block;"

  var span = document.createElement("span")
  span.classList.add("title")

  var _title = document.getElementsByClassName("headline-1")[0].textContent;
  var span2 = document.createElement("span")
  span2.classList.add("name")
  span2.appendChild(document.createTextNode(name))

  var a = document.createElement("a")
  a.classList.add("label")
  a.href = "http://tafdi.org"

  span.appendChild(span2)
  a.appendChild(span)
  v.appendChild(a)

  document.getElementsByClassName("services")[0].appendChild(v)
}
