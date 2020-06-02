build:
	elm make src/Main.elm --output=elm.min.js

run:
	elm-live src/Main.elm --pushstate --port=8000 -- --output=elm.min.js
