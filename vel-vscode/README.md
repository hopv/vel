# VS Code Extension for Vel

<!-- We can't use an SVG in README.md of a VS Code extension -->
<p><img src="https://raw.githubusercontent.com/hopv/vel/main/img/logo.png" height="70" alt="Vel's logo" /></p>

The VS Code extension for the Vel language.

## Features

- [x] Auto indentation
- [x] Syntax highlighting
- [ ] Auto formatting
- [ ] LSP client

## Install

This extension is not yet published to [VS Code's marketplace](https://marketplace.visualstudio.com/vscode).

### Local install

To install the extension locally, install [`vsce`](https://github.com/microsoft/vscode-vsce) to `npm` by:
```shell
> npm install @vscode/vsce -g
```
And run in this folder:
```shell
> vsce package
```
This generates the package `vel-*.vsix`, so install it to your VS Code by:
```shell
> code --install-extension vel-*.vsix
```

If you have modified [`vel.tmLanguage.yml`](./syntaxes/vel.tmLanguage.yml),
regenerate [`vel.tmLanguage.json`](./syntaxes/vel.tmLanguage.json).  
For that, you can install [`yq`](https://github.com/mikefarah/yq) and run:
```
> yq -o=j '.' syntaxes/vel.tmLanguage.yml > syntaxes/vel.tmLanguage.json
```

The build and install are automated by `make` (see [`Makefile`](./Makefile)).
