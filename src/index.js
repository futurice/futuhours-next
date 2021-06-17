import './main.css';
import { Elm } from './Main.elm';
import { unregister } from './registerServiceWorker';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: { 
    now: Date.now(),
    width: window.innerWidth,
    height: window.innerHeight,
    futucortexIframeUrl: process.env.ELM_APP_FUTUCORTEX_IFRAME_URL
  }
});

unregister();