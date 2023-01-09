.PHONY: all vsix code

veltm = syntaxes/vel.tmLanguage

all: code

# Generate Vel's JSON grammer file from YAML
$(veltm).json: $(veltm).yml
	yq -o=j '.' $(veltm).yml > $(veltm).json

# Generate Vel's VSIX package
vsix: $(veltm).json
	vsce package

# Install Vel's VSIX package in VS Code
code: vsix
	code --install-extension *.vsix
