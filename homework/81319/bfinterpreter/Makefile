CURRENT_DIR = $(shell pwd)
GOPATH=$(CURRENT_DIR)/.tmp/

interpreter: tmp
	$(cd .tmp/src/bfinterpreter)
	@echo $(CURRENT_DIR)
	go build

tmp:
	$(shell ./make_temp_folders.sh)

clean:
	rm -rf .tmp
	rm -rf bfinterpreter

