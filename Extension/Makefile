FS_SRC=$(shell find -maxdepth 1 -name "*.fs")
JS_OUT=$(subst .fs,.fs.js,$(FS_SRC))
../extension.zip: ../extension/ContentScript.js ../extension/Popup.js  ../extension/Background.js
	cd ../ ;\
	zip -0 extension.zip -j extension/*
./package.json.t: package.json
	femto --resolve
	echo "$$(date)" > ./package.json.t
$(JS_OUT): $(FS_SRC) package.json.t
	fable
../extension/%.js: %.fs.js
	npm exec -- browserify -p esmify $< > $@
clean:
	rm -rf ./fable_modules ./node_modules/ ./package.json.t
	rm -rf *.js
	rm -rf ../extension.zip
	rm -rf bin obj
