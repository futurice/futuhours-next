'use strict';

const express = require('express');
const proxy = require('express-http-proxy');
const path = require('path');

const PORT = 8000;
const HOST = '0.0.0.0';

const API_URL = process.env.API_URL;

const app = express();

// Static resources
app.use(express.static(path.join(__dirname + "/build", '/')));

// API proxy
app.use('/api', proxy(API_URL));

app.listen(PORT, HOST);
console.log(`Running on http://${HOST}:${PORT}`);