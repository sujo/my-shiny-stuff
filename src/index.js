// Styles
require('./assets/styles/main.scss');

// Vendor JS is imported as an entry in webpack.config.js

var storedData = localStorage.getItem('gw2_item_search_state');
var flags = storedData ? JSON.parse(storedData) : null;

// Elm
var Elm = require('./elm/Main.elm').Elm;
var app = Elm.Main.init({ flags: flags});

app.ports.setStorage.subscribe(function(state) {
    localStorage.setItem('gw2_item_search_state', JSON.stringify(state));
});
