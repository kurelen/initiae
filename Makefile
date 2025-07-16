.PHONY: all run clerk test lint format clean uberjar help

run:
	clojure -M:run

clerk:
	clojure -X:clerk

clerk-serve:
	clojure -M -e "(require '[nextjournal.clerk :as clerk]) (clerk/serve! {:browse? true}) (clerk/show! \"notebooks/clustering_analysis.clj\")"

test:
	clojure -M:test

lint:
	clojure -M:lint --lint src dev notebooks test build.clj

format:
	clojure -M:format fix

clean:
	clojure -T:build clean

uberjar:
	clojure -T:build uber

help:
	@echo "Usage: make <target>"
	@echo ""
	@echo "Targets:"
	@echo "  run       Run the application"
	@echo "  clerk     Build a clerk notebook"
	@echo "  test      Run unit tests"
	@echo "  lint      Lint the source and test code"
	@echo "  format    Format Clojure code"
	@echo "  clean     Delete the target directory"
	@echo "  uberjar   Build an uberjar"
