Turing Machine Puzzler
======================


Setup
-----
* Run `npm install`. This will install the correct elm version locally and also run `elm-make` as an npm postinstall hook. When `npm install` has finished there should be a file called `elm.js`.

Development Mode
----------------

* `npm run dev` starts elm-reactor with the correct ELM_HOME set (alternatively, you could do `ELM_HOME=node_modules/elm/share node_modules/.bin/elm-reactor` manually).
* Go to <http://localhost:8000/index.debug.html>.
* You now see the Elm app in the integrated Elm debugger and live reload enabled.


Pseudo Production Mode
----------------------

* `npm start` starts the app in "production mode" by simply building it with `elm-make` and then serving it via [http-server](https://github.com/indexzero/http-server).
* Go to <http://localhost:8080/index.html>.
    * This is the "production" view. No live reload, no debugger.
    * If you change the code, you need to rebuild `elm.js` by executing `npm run make` or by stopping the running http-server and execute `npm start` again (which will also trigger `npm run make`).
