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

exports._getMovieNameAndYear = function (onError, onSuccess) {
    chrome.tabs.query({active: true, currentWindow: true}, function(tabs) {
      chrome.tabs.sendMessage(tabs[0].id, {type: "getMovieNameAndYear"}, function(response) {
        if(response !== undefined) {
          return onSuccess({ value0: response.name, value1: response.year});
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
      return function(cancelDownloadClicked) {
        return function() {
          var mainTemplate = document.getElementById("main").innerHTML
          var renderedMain = Mustache.render(mainTemplate, { movie_state: movie_state, movie: movie })
          document.getElementById("container").innerHTML = renderedMain;
          addDownloadButtonActions(downloadButtonClicked)
          addCancelDownloadActions(cancelDownloadClicked)
        }
      }
    }
  }
}

function addCancelDownloadActions(cb) {
  var buttons = document.getElementsByClassName('movie-cancel-download')
  var iter = document.createNodeIterator(document.getElementById("container"), NodeFilter.SHOW_ELEMENT, { acceptNode: function(node) {
    var condition = node.classList.contains("movie-cancel-download") && node.tagName === "BUTTON"
    return condition ? NodeFilter.FILTER_ACCEPT : NodeFilter.FILTER_REJECT
  }})
  var button;
  while(button = iter.nextNode()) {
    button.addEventListener("click", function(e) {
      cb(e.currentTarget.getAttribute('href'))
    })
  }
}

function addDownloadButtonActions(cb) {
  var buttons = document.getElementsByClassName('movie-channel-download')
  var iter = document.createNodeIterator(document.getElementById("container"), NodeFilter.SHOW_ELEMENT, { acceptNode: function(node) {
    var condition = node.classList.contains("movie-channel-download") && node.tagName === "BUTTON"
    return condition ? NodeFilter.FILTER_ACCEPT : NodeFilter.FILTER_REJECT
  }})
  var button;
  while(button = iter.nextNode()) {
    button.addEventListener("click", function(e) {
      cb(e.currentTarget.getAttribute('href'))
    })
  }
}
