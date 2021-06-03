clojure_MODE = clj
SOURCES_LISP = src/mal/core.clj src/mal/step0_repl.clj
SRCS = $(SOURCES_BASE) src/mal/core.clj
SOURCES =  $(SOURCES_LISP)

DEPS = $(if $(filter cljs,$(clojure_MODE)),node_modules,deps)

dist: $(if $(filter cljs,$(clojure_MODE)),node_modules,mal.jar mal)

deps:
	lein deps

mal.jar: $(SOURCES)
	lein with-profile stepA uberjar
	cp target/stepA_mal.jar $@

SHELL := bash
mal: mal.jar
	cat <(echo -e '#!/bin/sh\nexec java -jar "$$0" "$$@"') mal.jar > $@
	chmod +x mal

src/mal/%.clj: $(DEPS)
	@true

target/%.jar: src/mal/%.clj $(SRCS)
	lein with-profile $(word 1,$(subst _, ,$*)) uberjar

clean:
	rm -rf target/ mal.jar mal
