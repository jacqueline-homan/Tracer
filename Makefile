MDS = $(wildcard *.md)
HTMLS = $(MDS:%.md=%.html)

PANDOC_OPTIONS = markdown \
	header_attributes \
	auto_identifiers \
	implicit_header_references \
	fancy_lists \
	startnum \
	table_captions \
	grid_tables \
	pandoc_title_block \
	yaml_metadata_block \
	superscript \
	subscript \
	inline_code_attributes \
	tex_math_dollars \
	shortcut_reference_links \
	implicit_figures \
	footnotes \
	inline_notes

.PHONY: all serve

all: $(HTMLS)

%.html: %.md
	@pandoc --smart \
	--from=$$(echo $(PANDOC_OPTIONS) | sed 's/ \+/+/g') \
	--standalone -o $@ $^

serve: $(HTMLS)
	python -m SimpleHTTPServer 8081
