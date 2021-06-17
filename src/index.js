import './main.css';
import { Elm } from './Main.elm';
import { unregister } from './registerServiceWorker';

const futucortexIframeUrl = process.env.ELM_APP_FUTUCORTEX_IFRAME_URL? process.env.ELM_APP_FUTUCORTEX_IFRAME_URL: ""

Elm.Main.init({
  node: document.getElementById('root'),
  flags: { 
    now: Date.now(),
    width: window.innerWidth,
    height: window.innerHeight,
    futucortexIframeUrl: futucortexIframeUrl
  }
});

unregister();