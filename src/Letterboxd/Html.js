exports._addService = function (serviceName) {
  return function(url) {
    chrome.tabs.query({active: true, currentWindow: true}, function(tabs) {
      chrome.tabs.sendMessage(tabs[0].id, {type: "addService", serviceName: serviceName, url: url}, function(response) {
        if(response !== undefined) {
          console.log(response.farewell);
        }
      });
    });
  }
}
