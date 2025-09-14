# Key Wallet
This is a non-custodial BTC and USDt wallet for Android devices. On top of that, it provides means for Tactical Advanage clients to manage their accounts.

# Building from source

## 1. NodeJS mobile binaries

Either obtain prebuilt `.so` files from [NodeJS mobile releases page](https://github.com/nodejs-mobile/nodejs-mobile/releases/tag/v18.20.4) 
or build them yourself by following [instructions](https://github.com/nodejs-mobile/nodejs-mobile/blob/main/doc_mobile/BUILDING.md), 
then copy `.so` files to `/app/libnode/bin/*`.

## 2. Biconomy JS bundle
```
$ cd <wallet>/biconomy
$ npm install --save-dev esbuild
$ npm run build
$ mv server.js <wallet>/app/src/main/assets/server.js
```

## 3. Assemble and APK file
```
$ ./gradlew assembleRelease
$ ls -l ./app/build/outputs/apk/release/app-release.apk
```

## Contributing

PRs are not welcome, this code is only made public to make it possible to inspect and verify it. 
Third party PRs will most likely be rejected so please don't waste your time on them.