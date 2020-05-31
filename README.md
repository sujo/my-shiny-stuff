# My Shiny Stuff

My Shiny Stuff shows, filters and searches in-game items of your Guild Wars 2 account bank and characters.

## API key

To use the application, create an API key with permissions "inventories" and "characters" at https://account.arena.net/applications .


## Source code

The source code for this application is available on
[Github](https://github.com/sujo/my-shiny-stuff).

This is an [elm](https://elm-lang.org) application based on the package
[elm-webpack-4-starter](https://github.com/romariolopezc/elm-webpack-4-starter).

### Requirements

* [Yarn](https://yarnpkg.com/lang/en/docs/install/)
* Node >= v8.9.0
* Webpack >= 4.0.0
* [Elm](https://guide.elm-lang.org/install.html)

### Development

After cloning this project, change into the cloned directory and run "yarn
install" to install the dependencies.

To compile and test this application on the local host, run "yarn dev" and
point your browser to http://localhost:8080 .

Most of the code is in src/elm/Main.elm.

### Deployment

The files for distribution are created in the directory "dist" by running
"yarn prod:compress".

To host the application on Github Pages, rename the "dist" directory to
"docs", which is the document root if Pages is configured so. Then commit and
push the changes to Github.


