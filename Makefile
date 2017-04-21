TARGET=target
EXE=$(TARGET)/ec2-linux-host-metrics
DIST_EXE=$(EXE)-$(shell uname -s)-$(shell uname -m)
DIST_EXE_SIG=$(DIST_EXE).sig

build:
	stack build $(STACK_OPTS) ec2-linux-host-metrics

build-prof:
	stack build $(STACK_OPTS) --profile --ghc-options="-rtsopts -fprof-auto" ec2-linux-host-metrics

install:
	stack install $(STACK_OPTS) ec2-linux-host-metrics

bindist:
	mkdir -p $(TARGET)
	stack --local-bin-path $(TARGET) install $(STACK_OPTS) ec2-linux-host-metrics
	upx --best $(EXE)
	mv $(EXE) $(DIST_EXE)
	gpg --output $(DIST_EXE_SIG) --detach-sign $(DIST_EXE)

clean:
	stack clean
	rm -rf target

tags:
	hasktags-generate .

sources:
	stack-unpack-dependencies


.PHONY: build build-prof clean tags sources

