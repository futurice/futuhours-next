import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: { 
    now: Date.now(),
    width: window.innerWidth,
    height: window.innerHeight
  }
});

registerServiceWorker();
