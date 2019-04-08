'use strict';

const express = require('express');
const proxy = require('express-http-proxy');
const path = require('path');

const PORT = 8000;
const HOST = '0.0.0.0';

const API_URL = process.env.API_URL;

const app = express();

// API proxy
app.use('/api', proxy(API_URL));

// Static resources
app.use(express.static(path.join(__dirname + "/build", '/')));

app.listen(PORT, HOST);
console.log(`Running on http://${HOST}:${PORT}`);