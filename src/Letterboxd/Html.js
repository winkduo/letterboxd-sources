exports._addService = function (serviceName) {
	chrome.tabs.query({active: true, currentWindow: true}, function(tabs) {
		chrome.tabs.sendMessage(tabs[0].id, {type: "addService", serviceName: serviceName}, function(response) {
      if(response !== undefined) {
        console.log(response.farewell);
      }
		});
	});
}
