exports._now = function () {
  return Date.now()
}

exports._addService = function () {
	chrome.tabs.query({active: true, currentWindow: true}, function(tabs) {
		chrome.tabs.sendMessage(tabs[0].id, {greeting: "hello"}, function(response) {
			console.log(response.farewell);

      chrome.tabs.executeScript({
        file: "add_element.js"
      })
		});
	});
}
