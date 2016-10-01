"use strict"

var ytdl = require('ytdl-core');
var cors = require('cors');

exports.cors = cors();

exports.getInfo = function(errback) {
  return function (callback) {
    return function(url) {
      return function() {
        ytdl.getInfo(url, [], function(err, response) {
          if (err) {
            return errback(err)();
          }
          return callback(response)();
        });
      }
    }
  }
}

exports.downloadFromInfo = function (info) {
  return function () {
    return ytdl.downloadFromInfo(info, { filter: 'videoonly' });
  }
}
