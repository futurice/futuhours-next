import path from 'path';
import './main.css';
import { Elm } from './Main.elm';
import { unregister } from './registerServiceWorker';

const futucortexIframeUrl = process.env.ELM_APP_FUTUCORTEX_IFRAME_URL? process.env.ELM_APP_FUTUCORTEX_IFRAME_URL: ""
const futucortexIframeViewers = process.env.ELM_APP_FUTUCORTEX_IFRAME_VIEWERS ? process.env.ELM_APP_FUTUCORTEX_IFRAME_VIEWERS.split(","): []  

fetch('../futucortexIframeViewers.txt')
  .then(response => response.text())
  .then(data => {
  	// Do something with your data
  	console.log(data);
  });

Elm.Main.init({
  node: document.getElementById('root'),
  flags: { 
    now: Date.now(),
    width: window.innerWidth,
    height: window.innerHeight,
    futucortexIframeUrl: futucortexIframeUrl,
    futucortexIframeViewers: futucortexIframeViewers
  }
});

unregister();