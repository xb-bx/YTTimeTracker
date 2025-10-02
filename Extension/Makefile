FS_SRC=$(shell find -maxdepth 1 -name "*.fs")
JS_OUT=$(subst .fs,.fs.js,$(FS_SRC))
all: ../extension.zip ../extension_chrome/manifest.json
	echo all
../extension.zip: ../extension/ContentScript.js ../extension/Popup.js  ../extension/Background.js
	cp ./node_modules/eld/minified/eld.M60.min.js ../extension/eld.js
	echo "window.eld = eld;" >> ../extension/eld.js
	cd ../ ;\
	zip -0 extension.zip -j extension/*
../extension_chrome/manifest.json: ../extension.zip manifest_chrome.json
	cd ../ ; \
	mkdir -p extension_chrome ; \
	cp extension/* extension_chrome/; \
	cp Extension/manifest_chrome.json extension_chrome/manifest.json
../extension_chrome.zip: ../extension_chrome/manifest.json
	cd ../extension_chrome/ ;\
	zip -0 ../extension_chrome.zip *
$(JS_OUT): $(FS_SRC) 
	fable
../extension/%.js: %.fs.js Shared.fs
	npm exec -- browserify -p esmify $< > $@
clean:
	rm -rf ./fable_modules ./node_modules/ ./package.json.t
	rm -rf ../extension_chrome/*
	rm -rf *.js
	rm -rf ../extension.zip
	rm -rf bin obj
