FS_SRC=$(shell find -maxdepth 1 -name "*.fs")
JS_OUT=$(subst .fs,.fs.js,$(FS_SRC))
all: ../extension/ContentScript.js ../extension/Popup.js ../extension/Stats.js ../extension/Background.js
	echo smth
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
	rm -rf bin obj
