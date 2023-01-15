.PHONY: all create-vsix install-to-code

veltm = syntaxes/vel.tmLanguage

all: install-to-code

# Generate Vel's JSON grammer file from YAML
$(veltm).json: $(veltm).yml
	yq -o=j '.' $(veltm).yml > $(veltm).json

icon.png: ../img/icon.png
	cp $< $@

# Create Vel's VSIX package
create-vsix: $(veltm).json icon.png
	vsce package

# Install Vel's VSIX package in VS Code
install-to-code: create-vsix
	code --install-extension vel-*.vsix
