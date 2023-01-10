.PHONY: all create-vsix install-to-code

veltm = syntaxes/vel.tmLanguage

all: install-to-code

# Generate Vel's JSON grammer file from YAML
$(veltm).json: $(veltm).yml
	yq -o=j '.' $(veltm).yml > $(veltm).json

# Create Vel's VSIX package
create-vsix: $(veltm).json
	vsce package

# Install Vel's VSIX package in VS Code
install-to-code: create-vsix
	code --install-extension vel-*.vsix
