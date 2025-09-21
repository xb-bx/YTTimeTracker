all: ./extension/ContentScript.js ./extension/Popup.js ./extension/Background.js
	echo smth
./out/ContentScript.js ./out/Popup.js ./out/Background.js: ./Extension/*.fs*
	fable ./Extension/Extension.fsproj
	cp ./Extension/ContentScript.fs.js out
	cp ./Extension/Popup.fs.js out
	cp ./Extension/Background.fs.js out
	cat out/ContentScript.fs.js | sed 's/.\/fable_modules\//..\/Extension\/\0/g' > ./out/ContentScript.js
	cat out/Popup.fs.js | sed 's/.\/fable_modules\//..\/Extension\/\0/g' > ./out/Popup.js
	cat out/Background.fs.js | sed 's/.\/fable_modules\//..\/Extension\/\0/g' > ./out/Background.js
	cat Extension/Shared.fs.js | sed 's/.\/fable_modules\//..\/Extension\/\0/g' > ./out/Shared.fs.js

./extension/%.js: ./out/%.js
	cd ./out; \
	browserify -p esmify ../$< > ../$@ 

clean:
	rm -rf out/*.js
	rm -rf extension/*.js
	find -name bin -o -name obj -o -name fable_modules | xargs rm -rf
