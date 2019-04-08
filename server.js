'use strict';

const express = require('express');
const proxy = require('express-http-proxy');
const path = require('path');

const PORT = 8000;
const HOST = '0.0.0.0';

const API_URL = process.env.API_HOST;
console.log(`API proxy set to ${API_URL}`);

const app = express();

// API proxy
app.use('/api', proxy(API_URL, {
    proxyReqPathResolver: function(req) {
        console.log(`Proxy request from: ${req.url}`);
        var parts = req.url.split('?');
        var queryString = parts[1];
        var updatedPath = parts[0].replace(/v1/, 'api/v1');
        return updatedPath + (queryString ? '?' + queryString : '');
    },
    proxyReqOptDecorator: function(proxyReqOpts, srcReq) {
        console.log(`Proxy request to: ${proxyReqOpts.path}`);
        proxyReqOpts.headers['X-Forwarded-Proto'] = srcReq.protocol;
        proxyReqOpts.headers['X-Real-IP'] = srcReq.ip;
        return proxyReqOpts;
    },
    proxyErrorHandler: function(err, res, next) {
        console.log(`API Proxy encountered error: ${err.code} - ${res}`);
        next(err);
    }
}));

// Static resources
app.use(express.static(path.join(__dirname + "/build", '/')));

app.listen(PORT, HOST);
console.log(`Running on http://${HOST}:${PORT}`);