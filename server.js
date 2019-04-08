'use strict';

const express = require('express');
const proxy = require('express-http-proxy');
const path = require('path');

const PORT = 8000;
const HOST = '0.0.0.0';

const API_URL = process.env.API_HOST + "/api";
console.log(`API proxy set to ${API_URL}`);

const app = express();

// API proxy
app.use('/api', proxy(API_URL, {
    proxyReqOptDecorator: function(proxyReqOpts, srcReq) {
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