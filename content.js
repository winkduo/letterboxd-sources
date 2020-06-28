const PUTIO_SERVICE_NODE_CLASS = "putio-extension-link";

chrome.runtime.onMessage.addListener(
  function(request, sender, sendResponse) {
  console.log(sender.tab ?
              "from a content script:" + sender.tab.url :
              "from the extension");
  if (request.type == "addService") {
    addService(request.serviceName, request.url, request.percentage);
    sendResponse({farewell: "goodbye"});
  } else if (request.type == "getMovieNameAndYear") {
    movieName = getMovieName();
    movieYear = getMovieYear();
    sendResponse({name: movieName, year: movieYear});
  } else if (request.type == "clearServices") {
    clearServices();
    sendResponse();
  }
});

function addService(name, url, percentage) {
  console.log("Adding service. Name: " + name + ", url: " + url);
  var v = document.createElement("p")
  v.classList.add("service")
  v.classList.add(PUTIO_SERVICE_NODE_CLASS)
  v.style = "display: block;"

  var span = document.createElement("span")
  span.classList.add("title")

  var _title = document.getElementsByClassName("headline-1")[0].textContent;
  var span2 = document.createElement("span")
  span2.classList.add("name")
  span2.appendChild(document.createTextNode(name))

  var a = document.createElement("a")
  a.classList.add("label")
  a.href = url

  var label = document.createElement("span")
  label.classList.add("options")

  var labelA = document.createElement("a")
  labelA.classList.add("link")
  labelA.appendChild(document.createTextNode(percentage + "%"))
  label.appendChild(labelA)

  span.appendChild(span2)
  a.appendChild(span)
  v.appendChild(a)
  v.appendChild(label)

  document.getElementsByClassName("services")[0].appendChild(v)
}

function getText(selector) {
  root = document.querySelector(selector);
  iter = document.createNodeIterator(root, NodeFilter.SHOW_TEXT);
  while(textnode = iter.nextNode()) {
    if(textnode !== undefined && textnode.textContent !== undefined) {
      return textnode.textContent;
    }
  }
}

function getMovieYear() {
  return getText('section#featured-film-header > p > small.number > a')
}

function getMovieName() {
  return getText('section#featured-film-header > h1')
}

function clearServices() {
  serviceNodes = document.getElementsByClassName('service ' + PUTIO_SERVICE_NODE_CLASS);
  console.log(serviceNodes);


  for(i = serviceNodes.length - 1; i >= 0; i--) {
    serviceNodes[i].remove();
  }
}
