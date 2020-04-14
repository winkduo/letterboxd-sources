# letterboxd-sources

A Chrome extension for showing extra sources for letterboxd.com.

## Installation
```
$ npm install
$ bower install
```

## Development
```
$ npm run start-psc
```

## Building
Run the following:
```
$ npm run start-webpack
```

Then;
1. Go to chrome://extensions
2. Click "Load unpacked"
3. Select dist/ folder in the repository
4. Click the extension on top right, a small white box appears
5. Right click on the box, select "inspect element" and go to its console
6. It will display the current time, as written in src/Main.purs
