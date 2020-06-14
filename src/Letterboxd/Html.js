exports._addService = function (serviceName) {
  return function(url) {
    return function(percentage) {
      chrome.tabs.query({active: true, currentWindow: true}, function(tabs) {
        chrome.tabs.sendMessage(tabs[0].id, {type: "addService", serviceName: serviceName, url: url, percentage: percentage}, function(response) {
          if(response !== undefined) {
            console.log(response.farewell);
          }
        });
      });
    }
  }
}

exports._getMovieName = function (onError, onSuccess) {
    chrome.tabs.query({active: true, currentWindow: true}, function(tabs) {
      chrome.tabs.sendMessage(tabs[0].id, {type: "getMovieName"}, function(response) {
        if(response !== undefined) {
          return onSuccess(response.name);
        }
      });
    });
}

exports._clearServices = function (onError, onSuccess) {
    chrome.tabs.query({active: true, currentWindow: true}, function(tabs) {
      chrome.tabs.sendMessage(tabs[0].id, {type: "clearServices"}, function(response) {
        onSuccess();
      });
    });
}
