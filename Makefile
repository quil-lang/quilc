all: quilc

quilc:
	buildapp --output quilc \
		 --asdf-tree "~/quicklisp/dists/quicklisp/software" \
		 --asdf-tree "./../" \
		 --load-system "quilc" \
		 --compress-core \
		 --entry quilc::entry-point

clean:
	rm -f quilc build-output.log
