import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var app = Main.embed(document.getElementById('root'));

app.ports.windowTitle.subscribe(function (title) {
  document.title = title;
});

registerServiceWorker();
