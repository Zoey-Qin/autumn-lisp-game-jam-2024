modules = \
  modules/dom/canvas.scm \
  modules/dom/document.scm \
  modules/dom/element.scm \
  modules/dom/event.scm \
  modules/dom/image.scm \
  modules/dom/media.scm \
  modules/dom/window.scm \
  modules/math.scm \
  modules/math/rect.scm \
  modules/math/vector.scm

game.wasm: game.scm $(modules)
	guild compile-wasm -L modules -o $@ $<

serve: game.wasm
	guile -c '((@ (hoot web-server) serve))'

build: game.wasm
	rm -rf public || true
	mkdir public
	cp reflect.js reflect.wasm game.js game.css game.wasm wtf8.wasm index.html public

publish: build
	npx wrangler pages deploy public --project-name autumn-lisp-game-jam-2024

bundle: game.wasm
	rm game.zip || true
	zip game.zip -r reflect.js reflect.wasm game.js game.css game.wasm wtf8.wasm index.html

clean:
	rm -f game.wasm game.zip public
