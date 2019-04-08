'use strict';

const express = require('express');
const path = require('path');

const PORT = 8000;
const HOST = '0.0.0.0';

const app = express();

app.use(express.static(path.join(__dirname + "/build", '/')));

app.listen(PORT, HOST);
console.log(`Running on http://${HOST}:${PORT}`);