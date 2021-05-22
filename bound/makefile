.PHONY: clean

cellular.html: Cellular.lhs makefile header.html
	pandoc --standalone -f markdown+lhs \
		Cellular.lhs -t html \
		-o cellular.html \
		--highlight-style=tango \
		-H header.html

clean:
	rm  *.html
