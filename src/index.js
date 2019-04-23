import './main.css';
import { Elm } from './Main.elm';

Elm.Main.init({
  node: document.getElementById('root'),
  flags: { 
    now: Date.now(),
    width: window.innerWidth,
    height: window.innerHeight
  }
});