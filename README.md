# PureScript React Three Fiber Examples

## Getting started

Install all the dependencies with

```shell
npm install
```

To run the app

```shell
npm run build && npm run dev
```

and it will open a browser.

## The project structure

This is a somewhat unusual webpack setup. The `src/Stories/**/*.purs` are the
entry points. Every PS file in `src/Stories` gets compiled and bundled to an
html file, which is then served through webpack dev server's directory view.

