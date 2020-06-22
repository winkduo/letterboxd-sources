"use strict"

const Mustache = require("mustache");
var Letterboxd_Html = require("./index.js");

exports._addService = function (serviceName) {
  return function(url) {
    return function(percentage) {
      return function() {
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

exports._renderEverything = function(movie) {
  return function(movie_state) {
    return function(downloadButtonClicked) {
      return function() {
        console.log(movie_state)
        var mainTemplate = document.getElementById("main").innerHTML
        var renderedMain = Mustache.render(mainTemplate, { movie_state: movie_state, movie: movie })
        document.getElementById("container").innerHTML = renderedMain;
        var buttons = document.getElementsByClassName('movie-channel-download')
        var iter = document.createNodeIterator(document.getElementById("container"), NodeFilter.SHOW_ELEMENT, { acceptNode: function(node) {
          var condition = node.classList.contains("movie-channel-download") && node.tagName === "BUTTON"
          return condition ? NodeFilter.FILTER_ACCEPT : NodeFilter.FILTER_REJECT
        }})
        var button;
        while(button = iter.nextNode()) {
          button.addEventListener("click", function(e) {
            downloadButtonClicked(e.currentTarget.getAttribute('href'))
          })
        }
      }
    }
  }
}
