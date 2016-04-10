# tin-web
Web interface to Tin

# Getting Started

## Install

tin-web is written in elm and will require you to have elm to run it locally.
Instructions for installing elm can be found on [elm-lang](http://elm-lang.org/install).

tin-web will look for tin running locally on the same port.
Usually when I am making changes to tin-web, I compile it with
```bash
elm-make Main.elm --yes --warn --output=../tin/public/js/application.js
```

where `tin` is in the same directory you cloned tin-web.

from Tin you can run `make s` to and find tin-web at [localhost:9393](http://localhost:9393/)
